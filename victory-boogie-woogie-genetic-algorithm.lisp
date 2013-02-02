;;;; victory-boogie-woogie-genetic-algorithm.lisp
;;;;
;;;; The painting has been turned 45 degrees clockwise!
;;;;
;;;; Where to switch from 0.0-1.0 to 0-max{r,g,b,x,y}?
;;;;
;;;; profiling:
;;;; o (require 'sb-sprof)
;;;; o (sb-sprof:with-profiling (:report :flat :loop nil :reset t
;;;;                             :sample-interval 0.001)
;;;;     <body>)

;;; Packages

(in-package :cl)

(asdf:oos 'asdf:load-op :eager-future2)
(rename-package :eager-future2 :eager-future2 '(:ef))

(asdf:oos 'asdf:load-op :png-read)
(asdf:oos 'asdf:load-op :zpng)


(defpackage :victory-boogie-woogie
  (:nicknames :vbw)
  (:use :cl))

(in-package :vbw)


;;; Classes

(defclass drawing ()
  ((genome     :accessor genome     :initarg :genome)
   (fitness    :reader   fitness    :initarg :fitness)
   (png        :reader   png        :initarg :png)
   (background :reader   background :initarg :background)))


;;; Methods

(defmethod print-object ((obj drawing) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "fitness:~,5E elements:~D"
            (fitness obj) (length (genome obj)))))


;;; Functions

(defun calculate-fitness (reference png)
  "Both REFERENCE and PNG are ZPNG objects."
  (when (or (not (= (zpng:width reference) (zpng:width png)))
            (not (= (zpng:height reference) (zpng:height png))))
    (error "dimensions of REFERENCE (~Dx~D) and PNG (~Dx~D) not equal"
           (zpng:width reference) (zpng:height reference)
           (zpng:width png) (zpng:height png)))
  (loop with difference = 0
        with datref = (zpng:image-data reference)
        with datpng = (zpng:image-data png)
        for i from 0 below (length datref) by 3
        for dr = (- (aref datref    i   ) (aref datpng    i   ))
        for dg = (- (aref datref (+ i 1)) (aref datpng (+ i 1)))
        for db = (- (aref datref (+ i 2)) (aref datpng (+ i 2)))
        do (incf difference (+ (* dr dr) (* dg dg) (* db db)))
        finally (return (/ 1 (1+ difference)))))


;; circles: #(red green blue alpha x y radius)
(defun create-random-gene (reference &optional (size 16) (type :circles))
  (let ((max-rgb (expt 2 (zpng::bpp reference))))
    (cond ((equal type :circles)
           (vector (random max-rgb) (random max-rgb) (random max-rgb)
                   (random max-rgb) (random (zpng:width reference))
                   (random (zpng:height reference)) size))
          (t (error "Unknown gene type: ~S!" type)))))


(defun create-random-genome (reference &optional (length 16) (size 16)
                                                 (type :circles))
  (when (<= length 0)
    (return-from create-random-genome nil))
  (loop with arr = (make-array (list length))
        for i from 0 below length
        do (setf (svref arr i) (create-random-gene reference size type))
        finally (return arr)))


(defun modify-color (reference gene &optional delta)
  (unless delta
    (setf delta (zpng::bpp reference)))
  (let* ((max-rgb (- (expt 2 (zpng::bpp reference)) 1))
         (new-gene (copy-seq gene))
         (delta*2 (* delta 2))
         (dr (- (random delta*2) delta))
         (dg (- (random delta*2) delta))
         (db (- (random delta*2) delta))
         (da (- (random delta*2) delta))
         (r (+ (elt new-gene 0) dr))
         (g (+ (elt new-gene 1) dg))
         (b (+ (elt new-gene 2) db))
         (a (+ (elt new-gene 3) da)))
    (when (and (>= r 0)
               (<= r max-rgb))
      (incf (elt new-gene 0) dr))
    (when (and (>= g 0)
               (<= g max-rgb))
      (incf (elt new-gene 1) dg))
    (when (and (>= b 0)
               (<= b max-rgb))
      (incf (elt new-gene 2) db))
    (when (and (>= a 0)
               (<= a max-rgb))
      (incf (elt new-gene 3) da))
    new-gene))


(defun modify-position (reference gene &optional delta)
  (unless delta
    (setf delta (zpng::bpp reference)))
  (let* ((max-rgb (- (expt 2 (zpng::bpp reference)) 1))
         (new-gene (copy-seq gene))
         (delta*2 (* delta 2))
         (dx (- (random delta*2) delta))
         (dy (- (random delta*2) delta))
         (x (+ (elt new-gene 4) dx))
         (y (+ (elt new-gene 5) dy)))
    (when (and (>= x 0)
               (<= x max-rgb))
      (incf (elt new-gene 4) dx))
    (when (and (>= y 0)
               (<= y max-rgb))
      (incf (elt new-gene 5) dy))
    new-gene))


(defun evolve-gene-circles (reference gene &optional delta)
  (unless delta
    (setf delta (zpng::bpp reference)))
  (let ((random-nr (random 1.0)))
    (cond ((< random-nr 0.05) (modify-color reference gene (* 10 delta)))
          ((< random-nr 0.10) (modify-position reference gene (* 10 delta)))
          ((< random-nr 0.55) (modify-color reference gene delta))
          (t                  (modify-position reference gene delta)))))


(defun evolve-genome (reference genome
                      &optional (modify-percentage 0.01) (type :circles))
  (declare (ignore modify-percentage))
  (loop with len = (length genome)
        with new-genome = (copy-seq genome)
        ;; #'ceiling to make sure at least 1 gene is modified
        ;repeat (ceiling (* modify-percentage len))
        ;; just a small fixed amount for now
        repeat 4
        for rnr = (random len)
        for new-gene = (cond ((equal type :circles)
                              (evolve-gene-circles reference (elt genome rnr)))
                             (t (error "Unknown gene type: ~S!" type)))
        do (setf (elt new-genome rnr) new-gene)
        finally (return new-genome)))


;; This is from either:
;;     - http://www.codeguru.com/cpp/cpp/algorithms/general/article.php/c15989/Tip-An-Optimized-Formula-for-Alpha-Blending-Pixels.htm
;; or
;;     - https://www.gamedev.net/topic/34688-alpha-blend-formula/
(defun set-pixel-unsafe (png x y r g b &optional (a 255) (max-rgb 255))
  "PNG is a ZPNG object.
  X and Y are integers, must be greater or equal to 0 and less than the
  width and height of the PNG.
  R, G, B and A are values between 0 and 1 (inclusive)."
  (declare (optimize (speed 3))
           (type fixnum x y r g b a max-rgb))
  (when (<= a 0)
    (return-from set-pixel-unsafe))
  (let* ((data (the (simple-array (unsigned-byte 8)) (zpng:image-data png)))
         (index   (the fixnum (+ (the fixnum (* y (the fixnum (zpng:width png))
                                                3))
                                 (the fixnum (* x 3)))))
         (index+1 (+ index 1))
         (index+2 (+ index 2)))
    (if (>= a 255)
        (setf (aref data index  ) r
              (aref data index+1) g
              (aref data index+2) b)
        (let* ((src-r (the fixnum (* (aref data index  ) (- max-rgb a))))
               (src-g (the fixnum (* (aref data index+1) (- max-rgb a))))
               (src-b (the fixnum (* (aref data index+2) (- max-rgb a))))
               (dst-r (the fixnum (* r a)))
               (dst-g (the fixnum (* g a)))
               (dst-b (the fixnum (* b a))))
          (setf (aref data index  ) (ash (+ src-r dst-r) -8)
                (aref data index+1) (ash (+ src-g dst-g) -8)
                (aref data index+2) (ash (+ src-b dst-b) -8))))))


(defun draw-horizontal-line (png x0 x1 y r g b &optional (a 255))
  (declare (optimize (speed 3))
           (type fixnum x0 x1 y r g b a))
  (when (or (< y 0) (>= y (the fixnum (zpng:height png))))
    (return-from draw-horizontal-line))
  (when (< x0 0)
    (setf x0 0))
  (when (>= x1 (the fixnum (zpng:width png)))
    (setf x1 (- (the fixnum (zpng:width png)) 1)))
  (loop for x from x0 to x1
        do (set-pixel-unsafe png x y r g b a)))


;; http://en.wikipedia.org/wiki/Midpoint_circle_algorithm
;; FIXME: This still produces artifacts (horizontal lines) for circles with
;;        a large radius and alpha.
(defun draw-filled-circle (png x y radius r g b &optional (a 255))
  (when (= radius 0)
    (set-pixel-unsafe png x y r g b a)
    (return-from draw-filled-circle))
  (let ((f (- 1 radius))
        (ddfx 1)
        (ddfy (* -2 radius))
        (x1 0)
        (y1 radius))
    (loop while (<= x1 y1)
          do (draw-horizontal-line png (- x y1) (+ x y1) (+ y x1) r g b a)
             (unless (= (+ y x1) (- y x1))  ; screws with transparency
               (draw-horizontal-line png (- x y1) (+ x y1) (- y x1) r g b a))
             (when (>= f 0)
               (draw-horizontal-line png (- x x1) (+ x x1) (+ y y1) r g b a)
               (draw-horizontal-line png (- x x1) (+ x x1) (- y y1) r g b a)
               (decf y1)
               (incf ddfy 2)
               (incf f ddfy))
             (incf x1)
             (incf ddfx 2)
             (incf f ddfx))))


(defun draw-genome-circles (background genome)
  (loop with png = (zpng:copy-png background)
        for gene across genome
        for r = (elt gene 0)
        for g = (elt gene 1)
        for b = (elt gene 2)
        for a = (elt gene 3)
        for x = (elt gene 4)
        for y = (elt gene 5)
        for radius = (elt gene 6)
        do (draw-filled-circle png x y radius r g b a)
        finally (return png)))


(defun make-drawing (reference background genome &optional (type :circles))
  (let* ((png (cond ((equal type :circles)
                     (draw-genome-circles background genome))
                    (t (error "Unknown gene type: ~S!" type))))
         (fitness (calculate-fitness reference png)))
    (make-instance 'drawing :fitness fitness :genome genome :png png
                            :background background)))


(defun evolve-drawing (reference background drawing &optional (type :circles))
  (let ((new-genome (evolve-genome reference (genome drawing) type)))
    (make-drawing reference background new-genome type)))


(defun create-random-drawing (reference background
                              &optional (length 128) (size 16)
                                        (type :circles))
  (make-drawing reference background
                (create-random-genome reference length size type)
                type))


(defun read-png (path)
  "Reads the PNG file at PATH and returns a ZPNG object."
  (let* ((png-in (png-read:read-png-file path))
         (png-out (make-instance 'zpng:png :color-type :truecolor
                                 :width (png-read:width png-in)
                                 :height (png-read:height png-in)))
         (data-in (png-read:image-data png-in))
         (data-out (zpng:data-array png-out)))
    (loop for y from 0 below (png-read:height png-in)
          do (loop for x from 0 below (png-read:width png-in)
                   do (setf (aref data-out y x 0) (aref data-in x y 0)
                            (aref data-out y x 1) (aref data-in x y 1)
                            (aref data-out y x 2) (aref data-in x y 2))))
    png-out))


(defun write-png (png &optional (path "tmp.png"))
  "Writes a ZPNG object to PATH."
  (zpng:write-png png path))


(defun save-drawing (drawing &optional (path "tmp.png"))
  (write-png (png drawing) path))


;;; Main Program

(defun main (reference-path &key (max-generations 1024) (genome-length 4)
                                 (population 1) (png-out-path "tmp.png")
                                 (min-size 1) (size 256) (type :circles)
                                 background)
  (declare (ignore min-size population))
  (let* ((ref (read-png reference-path))
         (bg (if background
                 background
                 (make-instance 'zpng:png :color-type :truecolor
                                :width (zpng:width ref)
                                :height (zpng:height ref))))
         (drw (create-random-drawing ref bg genome-length size type)))
    (format t "[  gen   /upd] ~S: ~F~%" drw (fitness drw))
    (save-drawing drw png-out-path)
    (loop with dgen = 0
          with last-change = 0
          repeat max-generations
          for gen from 1
          for new-drw = (evolve-drawing ref bg drw type)
          do (when (> (fitness new-drw) (fitness drw))
               (setf last-change gen
                     drw         new-drw)
               ;(save-drawing drw png-out-path)
               (format t "[~8D/~3D] ~S~%" gen dgen drw))
             (setf dgen (- gen last-change))
             (when (> dgen 512)  ; FIXME turn 512 into a variable
               (save-drawing drw "tmp-new-bg.png")
               (setf bg            (zpng:copy-png (png drw))
                     dgen          0
                     genome-length (* 2 genome-length)
                     last-change   gen
                     size          (ceiling (/ size 2))
                     (genome drw)  (create-random-genome ref genome-length
                                                         size type)
                     drw           (evolve-drawing ref bg drw type))
               (format t "*** Switching to genome-length=~D and size=~D.~%"
                       genome-length size))
             (when (= 0 (mod gen 1000))
               (save-drawing drw png-out-path)))
    (save-drawing drw png-out-path)))
