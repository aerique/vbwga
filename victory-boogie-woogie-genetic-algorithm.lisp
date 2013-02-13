;;;; victory-boogie-woogie-genetic-algorithm.lisp
;;;;
;;;; The painting has been turned 45 degrees clockwise!
;;;;
;;;; o This looks really nice: (main "reference-pictures/victory-boogie-woogie-marie-ll-flickr-512x512-rotated-45.png" :size 512 :genome-length 8 :min-size 2 :max-generations 3000000)
;;;; o Type should be hidden in drawing class and not in fn args.
;;;; o We need a way to make a genome resolution independent again.
;;;; o :circles is nice at 256x256 after 512k generations (and size = 2).
;;;;   - at 1024k+ generations straight lines and squares start to be made.
;;;; o :squares is nice at 256x256 after 1024k generations.
;;;; o :circles needs 2048000 gens at 512x512 and 8 elements and 512 radius.
;;;;
;;;; profiling:
;;;; o (require 'sb-sprof)
;;;; o (sb-sprof:with-profiling (:report :flat :loop nil :reset t
;;;;                             :sample-interval 0.001)
;;;;     <body>)
;;;;
;;;; To add new gene-types:
;;;; o Add DRAW-GENOME-<gene-type>
;;;; o Add DRAW-<gene-type> or DRAW-FILLED-<gene-type>
;;;; o Add gene-type to #'cond in:
;;;;   - CREATE-RANDOM-GENE
;;;;   - EVOLVE-GENOME
;;;;   - MAKE-DRAWING

;;; Packages

(in-package :cl)

;(asdf:oos 'asdf:load-op :eager-future2)
;(rename-package :eager-future2 :eager-future2 '(:ef))

(asdf:oos 'asdf:load-op :png-read)
(asdf:oos 'asdf:load-op :zpng)


(defpackage :victory-boogie-woogie
  (:nicknames :vbw)
  (:use :cl))

(in-package :vbw)


;;; Globals

(defparameter *gnuplot-data* nil)


;;; Classes

(defclass drawing ()
  ((genome     :accessor genome     :initarg :genome)
   (bg-genome  :accessor bg-genome  :initarg :bg-genome)
   (background :accessor background :initarg :background)
   (gene-type  :reader   gene-type  :initarg :gene-type)
   (fitness    :reader   fitness    :initarg :fitness)
   (png        :reader   png        :initarg :png)
   (width      :reader   width      :initarg :width)
   (height     :reader   height     :initarg :height)))


;;; Methods

(defmethod print-object ((obj drawing) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A fitness:~,5E elements:~D"
            (gene-type obj) (fitness obj) (length (genome obj)))))


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
;; squares: #(red green blue alpha x y width/2)
(defun create-random-gene (reference &optional (size 16) (type :circles))
  (let ((max-rgb (expt 2 (zpng::bpp reference))))
    (cond ((or (equal type :circles)
               (equal type :squares))
           (vector (random max-rgb) (random max-rgb) (random max-rgb)
                   (random max-rgb) (random (zpng:width reference))
                   (random (zpng:height reference)) size))
          (t (error "Unknown gene type: ~S" type)))))


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


(defun evolve-gene (reference gene &optional delta)
  (unless delta
    (setf delta (zpng::bpp reference)))
  (let ((random-nr (random 1.0)))
    (cond ((< random-nr 0.05) (modify-color reference gene (* 10 delta)))
          ((< random-nr 0.10) (modify-position reference gene (* 10 delta)))
          ((< random-nr 0.55) (modify-color reference gene delta))
          (t                  (modify-position reference gene delta)))))


(defun evolve-genome (reference genome &key (modify-percentage 0.01)
                                            (type :circles))
  (declare (ignore modify-percentage))
  (loop with len = (length genome)
        with new-genome = (copy-seq genome)
        ;; #'ceiling to make sure at least 1 gene is modified
        ;repeat (ceiling (* modify-percentage len))
        ;; just a small fixed amount for now
        repeat 4
        for rnr = (random len)
        for new-gene = (cond ((or (equal type :circles)
                                  (equal type :squares))
                              (evolve-gene reference (elt genome rnr)))
                             (t (error "Unknown gene type: ~S" type)))
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
  ;; (safety 0) gets rid of some extra optimization notes
  (declare (optimize (safety 0) (speed 3))
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
  ;; (safety 0) gets rid of some extra optimization notes
  (declare (optimize (safety 0) (speed 3))
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


(defun draw-filled-square (png x y width/2 r g b &optional (a 255))
  (when (= width/2 0)
    (set-pixel-unsafe png x y r g b a)
    (return-from draw-filled-square))
  (loop for y0 from (- y width/2) to (+ y width/2)
        do (draw-horizontal-line png (- x width/2) (+ x width/2) y0 r g b a)))


(defun draw-genome-squares (background genome)
  (loop with png = (zpng:copy-png background)
        for gene across genome
        for r = (elt gene 0)
        for g = (elt gene 1)
        for b = (elt gene 2)
        for a = (elt gene 3)
        for x = (elt gene 4)
        for y = (elt gene 5)
        for width/2 = (elt gene 6)
        do (draw-filled-square png x y width/2 r g b a)
        finally (return png)))


(defun make-drawing (reference background genome bg-genome
                     &optional (type :circles))
  (let* ((png (cond ((equal type :circles)
                     (draw-genome-circles background genome))
                    ((equal type :squares)
                     (draw-genome-squares background genome))
                    (t (error "Unknown gene type: ~S" type))))
         (fitness (calculate-fitness reference png)))
    (make-instance 'drawing :genome genome :bg-genome bg-genome :gene-type type
                   :fitness fitness :png png :background background
                   :width (zpng:width png) :height (zpng:height png))))


(defun evolve-drawing (reference drawing)
  (let ((new-genome (evolve-genome reference (genome drawing)
                                   :type (gene-type drawing))))
    (make-drawing reference (background drawing) new-genome (bg-genome drawing)
                  (gene-type drawing))))


(defun create-random-drawing (reference &optional (length 128) (size 16)
                                                  (type :circles))
  (make-drawing reference (empty-png reference)
                (create-random-genome reference length size type)
                (make-array '(0) :fill-pointer 0) type))


(defun draw-resolution-independent-genome-circles (genome width height)
  (let ((background (make-instance 'zpng:png :color-type :truecolor
                                   :width width :height height))
        ;; FIXME use bg-genome too
        (new-genome (loop for gene across genome
                          for r = (* (elt gene 0) 256)
                          for g = (* (elt gene 1) 256)
                          for b = (* (elt gene 2) 256)
                          for a = (* (elt gene 3) 256)
                          for x = (* (elt gene 4) width)
                          for y = (* (elt gene 5) height)
                          for radius = (* (elt gene 6) width)
                          collect (vector r g b a x y radius) into result
                          finally (return (coerce result 'vector)))))
    (draw-genome-circles background new-genome)))


;; XXX - only correct for circles and squares currently
;; XXX - background fuck things up, I need to keep the genome around that
;;       makes up the background
(defun resolution-independent-drawing (drawing)
  (loop with width = (width drawing)
        with height = (height drawing)
        for gene across (genome drawing)
        ;; yeah, hardcoded
        collect (loop for item across gene collect (/ item 256))
          into new-genome
        finally (return (make-instance 'drawing
                          :genome (coerce new-genome 'vector)
                          :gene-type (gene-type drawing)
                          :fitness (fitness drawing)
                          :png (zpng:copy-png (png drawing))
                          :background (zpng:copy-png (background drawing))
                          :width width :height height))))


(defun empty-png (reference)
  (make-instance 'zpng:png :color-type :truecolor
                 :width (zpng:width reference)
                 :height (zpng:height reference)))


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


;; In gnuplot do: plot 'tmp.dat' using 1:2 w l,           \
;;                     'tmp.dat' using 1:3 w l axes x1y2, \
;;                     'tmp.dat' using 1:4 w l axes x1y2
(defun write-gnuplot-data (&optional (path "tmp.dat"))
  (with-open-file (f path :direction :output :if-exists :supersede)
	(loop for lst across *gnuplot-data*
	   do (format f "~D ~F ~D ~D~%"
				  (getf lst :generation) (getf lst :fitness)
				  (getf lst :genome-length) (getf lst :size)))))


;;; Main Program

(defun main (reference-path &key (max-generations 256000) (genome-length 4)
                                 (size 256) (min-size 1) (type :circles)
                                 (png-out-path "tmp.png"))
  (setf *gnuplot-data* (make-array '(0) :fill-pointer 0))
  (let* ((ref (read-png reference-path))
         (drw (create-random-drawing ref genome-length size type)))
    (format t "[        /   ] ~S size=~D~%" drw size)
    (save-drawing drw png-out-path)
    (loop with dgen = 0
          with last-change = 0
          repeat max-generations
          for gen from 1
          for new-drw = (evolve-drawing ref drw)
          do (when (> (fitness new-drw) (fitness drw))
			   (vector-push-extend (list :generation gen
										 :fitness (fitness new-drw)
										 :genome-length genome-length
										 :size size)
								   *gnuplot-data*)
               (setf last-change gen
                     drw         new-drw)
               ;(save-drawing drw png-out-path)
               (format t "[~8D/~3D] ~S size=~D~%" gen dgen drw size))
             (setf dgen (- gen last-change))
             ;; 256 was maybe too little but 512 seems too much (for 256x256)
             (when (> dgen 512)  ; FIXME turn 512 into a variable
               (save-drawing drw "tmp-new-bg.png")
               (setf dgen             0
                     genome-length    (* 2 genome-length)
                     last-change      gen
                     size             (if (<= size min-size)
                                          min-size
                                          (ceiling (/ size 2)))
                     (genome drw)     (create-random-genome ref genome-length
                                                            size type)
                     (bg-genome drw)  (concatenate 'vector (bg-genome drw)
                                                           (genome drw))
                     (background drw) (zpng:copy-png (png drw))
                     drw              (evolve-drawing ref drw))
               (format t "*** Switching to genome-length=~D and size=~D.~%"
                       genome-length size))
             (when (= 0 (mod gen 1000))
               (save-drawing drw png-out-path)))
    (save-drawing drw png-out-path)
    drw))
