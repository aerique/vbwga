;;;;- victory-boogie-woogie-genetic-algorithm.lisp
;;;;-
;;;;- This source file can be converted to Markdown using src2markup:
;;;;- https://github.com/aerique/src2markup#readme
;;;;-
;;;; # Victory Boogie Woogie Contest
;;;;
;;;; This is my first entry for the [Victory Boogie Woogie (VBW)
;;;; contest](http://www.elegant.setup.nl/). It tries to reproduce the
;;;; painting using a reference picture and a [genetic algorithm
;;;; (GA)](https://en.wikipedia.org/wiki/Genetic_algorithm).
;;;;
;;;; This approach is inspired by my earlier work (which has not been
;;;; released yet) to find a way to make graphical content for an iOS
;;;; game without the need for an artist: take photographs, run the GA
;;;; over it and, voila: content!
;;;;
;;;; This earlier work was in turn inspired by Roger Alsing's seminal
;;;; blog about
;;;; [reproducing the Mona Lisa using polygons](http://rogeralsing.com/2008/12/07/genetic-programming-evolution-of-mona-lisa/).
;;;; Although he calls it
;;;; "[genetic programming](https://en.wikipedia.org/wiki/Genetic_programming)"
;;;; which it is *not*.
;;;;
;;;; My work differs from Roger Alsing's approach in that it doesn't try
;;;; to make an exact reproduction using polygons (which can take almost
;;;; any form) but rather a reinterpretation using only one kind of form.
;;;; Circles in this case, but I have experimented with squares and other
;;;; forms as well.
;;;;
;;;; My intent was to make something that would be similar to the
;;;; [Pointillism](https://en.wikipedia.org/wiki/Pointillism) style used
;;;; by classical painters.
;;;;
;;;; *Note*: The reference picture has been rotated 45 degrees
;;;; clockwise! This has been done to make optimal use of the available
;;;; space, since the algorithm is slow enough as it is. Once printed
;;;; the printout should be rotated 45 degrees anti-clockwise.
;;;;
;;;; ## Installation & Running
;;;;
;;;; This program uses [SBCL](http://www.sbcl.org/) and
;;;; [Quicklisp](http://www.quicklisp.org/).
;;;;
;;;; Depending on your machine it can take several hours to more than a
;;;; day for the drawing to finish! For this reason representative
;;;; output has been supplied with this contest entry as
;;;; `vbw-example.pdf`, although no two results will ever be the same.
;;;;
;;;; While running the program will print progress output in the
;;;; following format: "[X/Y] #<DRAWING CIRCLES fitness:Z elements:A>
;;;; size=B".
;;;;
;;;; * **X**: current generation,
;;;; * **Y**: number of generations without any progress,
;;;; * **Z**: fitness of the current best drawing,
;;;; * **A**: number of genomes currently used,
;;;; * **B**: brush size.
;;;;
;;;; By default the target fitness has been set to 9.0e-10 so whenever Z
;;;; goes over that number the drawing is done and a `vbw.pdf` will be
;;;; written to disk.
;;;;
;;;; Also, while the program is running a picture of the current
;;;; progress will be saved every 1000 generations as `tmp.png`. If you
;;;; use a picture viewer that refreshes whenever `tmp.png` has changed
;;;; you will have a live update of the progress.
;;;;
;;;; ### Unix (Linux, Ubuntu, etc.)
;;;;
;;;; 1. Preferably install SBCL using your distribution's package
;;;;    manager or otherwise use an archive from the SBCL website;
;;;; 2. [Install Quicklisp](http://www.quicklisp.org/beta/#installation)
;;;;    (make sure you do `(ql:add-to-init-file)`);
;;;; 3. Run this program using SBCL: `sbcl --load <stub>`;
;;;; 4. Wait...
;;;; 5. Once finished the result will be saved in `vbw.pdf`.
;;;;
;;;; ### OS X
;;;;
;;;; 1. Install SBCL using [Homebrew](http://mxcl.github.com/homebrew/);
;;;; 2. [Install Quicklisp](http://www.quicklisp.org/beta/#installation)
;;;;    (make sure you do `(ql:add-to-init-file)`);
;;;; 3. Run this program using SBCL: `sbcl --load
;;;;    victory-boogie-woogie-genetic-algorithm.lisp`;
;;;; 4. Wait...
;;;; 5. Once finished the result will be saved in `vbw.pdf`.
;;;;
;;;; ### Windows
;;;;
;;;; If you're using Window you are going to have a hard time running
;;;; this program but here are the instructions if you are feeling
;;;; adventurous:
;;;;
;;;; 1. Install the X86 SBCL from: http://www.sbcl.org/platform-table.html;
;;;; 2. [Install Quicklisp](http://www.quicklisp.org/beta/#installation)
;;;;    (make sure you do `(ql:add-to-init-file)`);
;;;; 3. Go to the commandline (type `cmd` in the "Search programs and
;;;;    files" input box of the Start menu;
;;;; 4. Run this program using SBCL: `sbcl --load <stub>`;
;;;; 5. Pray it all works;
;;;; 6. Wait...
;;;; 7. Once finished the result will be saved in `vbw.pdf`.

;; We **need* Quicklisp!
(unless (find-package :quicklisp)
  (format *error-output* "~&Please install Quicklisp! See the manual for instructions.~%"))


;;; Packages

(in-package :cl)

(ql:quickload :cl-pdf)
(ql:quickload :png-read)
(ql:quickload :zpng)

(defpackage :victory-boogie-woogie
  (:nicknames :vbw)
  (:use :cl))

(in-package :vbw)


;;; Globals

;; The maximum value each of the red, green, blue and alpha components
;; can take. This is more or less dictated by
;; [ZPNG](http://www.xach.com/lisp/zpng/).
(defparameter +max-rgba+ 255)


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
  "Both REFERENCE and PNG should be ZPNG objects."
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
    ;; XXX set max-rgba here?
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


;; One of the two optimized functions so it doesn't look very pretty.
;; It is called "unsafe" because no checks are made to see whether any
;; of the input arguments fall within the allowed parameters. You are
;; supposed to do this in a higher level function.
;;
;; Since no checks are made and this function is the inner loop of the
;; program we gain a significant speed increase. A higher level function
;; would call this function tens or hundreds of times while it only has
;; to check the bounds once.
;;
;; The alpha blending code is from either:
;; * http://www.codeguru.com/cpp/cpp/algorithms/general/article.php/c15989/Tip-An-Optimized-Formula-for-Alpha-Blending-Pixels.htm or
;; * https://www.gamedev.net/topic/34688-alpha-blend-formula/
(defun set-pixel-unsafe (png x y r g b
                         &optional (a +max-rgba+) (max-rgb +max-rgba+))
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
    (if (>= a max-rgb)
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


;; The other optimized function since it is one step above the inner
;; loop and profiling showed speed gains could be made (and were made)
;; here.
(defun draw-horizontal-line (png x0 x1 y r g b &optional (a +max-rgba+))
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
(defun draw-filled-circle (png x y radius r g b &optional (a +max-rgba+))
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


(defun draw-filled-square (png x y width/2 r g b &optional (a +max-rgba+))
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


(defun empty-png (reference)
  (make-instance 'zpng:png :color-type :truecolor
                 :width (zpng:width reference)
                 :height (zpng:height reference)))


(defun create-random-drawing (reference &optional (length 128) (size 16)
                                                  (type :circles))
  (make-drawing reference (empty-png reference)
                (create-random-genome reference length size type)
                (make-array '(0) :fill-pointer 0) type))


(defun draw-resolution-independent-genome-circles (genome width height)
  (let ((background (make-instance 'zpng:png :color-type :truecolor
                                   :width width :height height))
        (new-genome (loop with radius = (/ (+ width height) 2)
                          for gene across genome
                          ;; FLOOR or CEILING?
                          for r = (floor (* (elt gene 0) +max-rgba+))
                          for g = (floor (* (elt gene 1) +max-rgba+))
                          for b = (floor (* (elt gene 2) +max-rgba+))
                          for a = (floor (* (elt gene 3) +max-rgba+))
                          for x = (floor (* (elt gene 4) width))
                          for y = (floor (* (elt gene 5) height))
                          for s = (floor (* (elt gene 6) radius))
                          collect (vector r g b a x y s) into result
                          finally (return (coerce result 'vector)))))
    (draw-genome-circles background new-genome)))


;; XXX - only correct for circles and squares currently
(defun resolution-independent-drawing (drawing)
  (loop with width = (width drawing)
        with height = (height drawing)
        for gene across (concatenate 'vector (bg-genome drawing)
                                             (genome drawing))
        for r = (/ (elt gene 0) +max-rgba+)
        for g = (/ (elt gene 1) +max-rgba+)
        for b = (/ (elt gene 2) +max-rgba+)
        for a = (/ (elt gene 3) +max-rgba+)
        for x = (/ (elt gene 4) width)
        for y = (/ (elt gene 5) height)
        for s = (/ (elt gene 6) (/ (+ width height) 2))
        collect (vector r g b a x y s) into new-genome
        finally (return (make-instance 'drawing
                          :genome (coerce new-genome 'vector)
                          :bg-genome (make-array '(0) :fill-pointer 0)
                          :gene-type (gene-type drawing)
                          :fitness (fitness drawing)
                          :png (zpng:copy-png (png drawing))
                          :background (zpng:copy-png (background drawing))
                          :width width :height height))))



(defun write-pdf (drawing &optional (path "tmp.pdf"))
  (pdf:with-document ()
    (pdf:with-page ()
      (let* (;; we're assuming A4 portrait here
             (margin 16)
             (x-bound (elt (pdf::bounds pdf:*page*) 2))
             (y-bound (elt (pdf::bounds pdf:*page*) 3))
             (size (- x-bound (* 2  margin)))
             (x-offset margin)
             (y-offset (- y-bound size margin)))
        (pdf:set-rgb-fill 0 0 0)
        (pdf:set-fill-transparency 1.0)
        (pdf:rectangle x-offset y-offset size size)
        (pdf:close-and-fill)
        (loop with scale = size
              for gene across (genome drawing)
              for r = (elt gene 0)
              for g = (elt gene 1)
              for b = (elt gene 2)
              for a = (coerce (elt gene 3) 'float)
              for x = (elt gene 4)
              for y = (- 1 (elt gene 5))
              for s = (elt gene 6)
              do (pdf:set-rgb-fill r g b)
                 (pdf:set-fill-transparency a)
                 (pdf:circle (+ (* x scale) x-offset)
                             (+ (* y scale) y-offset)
                             (* s scale))
                 (pdf:close-and-fill))
        ;; redraw white margins since the circles aren't clipped
        (pdf:set-rgb-fill 1 1 1)
        (pdf:set-fill-transparency 1.0)
        (pdf:rectangle 0 (- y-bound margin) x-bound margin)   ; top
        (pdf:rectangle 0 y-offset x-offset size)              ; left
        (pdf:rectangle (+ size margin) y-offset margin size)  ; right
        (pdf:rectangle 0 0 x-bound y-offset)                  ; bottom
        (pdf:close-and-fill)))
    (pdf:write-document path)))


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

(defun main (reference-path &key (genome-length 4) (min-size 2) (size 512)
                                 (target-fitness 9e-10) (type :circles)
                                 (max-dgen 448) (png-out-path "tmp.png"))
  (let* ((ref (read-png reference-path))
         (drw (create-random-drawing ref genome-length size type)))
    (format t "[        /   ] ~S size=~D~%" drw size)
    (save-drawing drw png-out-path)
    (loop with dgen = 0
          with last-change = 0
          until (>= (fitness drw) target-fitness)
          for gen from 1
          for new-drw = (evolve-drawing ref drw)
          do ;; Check if the new drawing is better than the current
             ;; best, if so: start using the new drawing.
             (when (> (fitness new-drw) (fitness drw))
               (setf last-change gen
                     drw         new-drw)
               (format t "[~8D/~3D] ~S size=~D~%" gen dgen drw size))
             (setf dgen (- gen last-change))
             ;; If no improvements have been made for `dgen` generations
             ;; we double the number of genes and halve the brush size.
             ;; (Unless we are using the smallest brush already.)
             ;; We also start with a fresh genome and use what we have
             ;; made so far as its background.
             (when (> dgen max-dgen)
               (setf dgen             0
                     genome-length    (if (<= size min-size)
                                          genome-length
                                          (* 2 genome-length))
                     last-change      gen
                     size             (if (<= size min-size)
                                          min-size
                                          (ceiling (/ size 2)))
                     ;; **Note!**: must come before the (genome drw) setf!
                     (bg-genome drw)  (concatenate 'vector (bg-genome drw)
                                                           (genome drw))
                     (genome drw)     (create-random-genome ref genome-length
                                                            size type)
                     (background drw) (zpng:copy-png (png drw))
                     drw              (evolve-drawing ref drw))
               (format t "*** Switching to genome-length=~D and size=~D.~%"
                       genome-length size))
             (when (= 0 (mod gen 1000))
               (save-drawing drw png-out-path)))
    (save-drawing drw png-out-path)
    drw))


(write-pdf (resolution-independent-drawing (main "reference-pictures/victory-boogie-woogie-marie-ll-flickr-512x512-rotated-45.png"))
           "vbw.pdf")
