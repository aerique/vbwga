;;;;- victory-boogie-woogie-genetic-algorithm.lisp
;;;;-
;;;; # VBWGA
;;;;
;;;; <small><i>author: Erik Winkels ([aerique@xs4all.nl](mailto:aerique@xs4all.nl), [http://www.aerique.net/](http://www.aerique.net), [@aerique](https://twitter.com/aerique))  
;;;; version: v1.1</i></small>
;;;;
;;;; **Note**: skip ahead to the "Creation" chapter if you're primarily
;;;; interested in a description of the algorithm without all the extra
;;;; fluff.
;;;;
;;;; This is my first entry for the
;;;; [Victory Boogie Woogie contest](http://www.elegant.setup.nl/). It
;;;; tries to reproduce the painting using a reference picture and a
;;;; [genetic algorithm](https://en.wikipedia.org/wiki/Genetic_algorithm)
;;;; (GA).
;;;;
;;;; This approach is inspired by my earlier work (which has not been
;;;; released yet) to find a way to make graphical content for an iOS
;;;; game without the need for an artist: take photographs, run the GA
;;;; over it and, voila: content in a consistent style!  This earlier
;;;; work was in turn inspired by Roger Alsing's seminal blog about
;;;; [reproducing the Mona Lisa using polygons](http://rogeralsing.com/2008/12/07/genetic-programming-evolution-of-mona-lisa/).
;;;; Although he calls it
;;;; "[genetic programming](https://en.wikipedia.org/wiki/Genetic_programming)"
;;;; which it is *not*.
;;;;
;;;; My work differs from Roger Alsing's approach in that it doesn't try
;;;; to make an exact reproduction using polygons (which can take almost
;;;; any form) but rather a reinterpretation using only one kind of
;;;; form; circles in this case, but I have experimented with squares
;;;; and other forms as well.  The intent was to make something that
;;;; would be similar to the
;;;; [Pointillism](https://en.wikipedia.org/wiki/Pointillism) style used
;;;; by classical painters.
;;;;
;;;; Note: The reference picture has been rotated 45 degrees clockwise!
;;;; This has been done to make optimal use of the available space,
;;;; since the algorithm is slow enough as it is. Once printed the
;;;; result should be rotated 45 degrees anti-clockwise.
;;;;
;;;; ## Installation & Running
;;;;
;;;; This program uses [SBCL](http://www.sbcl.org/) and
;;;; [Quicklisp](http://www.quicklisp.org/).
;;;;
;;;; Depending on your machine it can take several hours to more than a
;;;; day for a drawing to finish! For this reason representative output
;;;; has been supplied with this contest entry as
;;;; [vbw-example.pdf](vbw-example.pdf), although no two results will
;;;; ever be the same.
;;;;
;;;; While running the program will print progress output in the
;;;; following format: "[X/Y] #&lt;DRAWING CIRCLES f=Z g=A&gt; size=B".
;;;;
;;;; - **X**: current generation,
;;;; - **Y**: number of generations without any progress since the
;;;;          previous update,
;;;; - **Z**: fitness of the current best drawing,
;;;; - **A**: number of genes currently used,
;;;; - **B**: circle size.
;;;;
;;;; By default the target fitness has been set to 2.0e-9 so whenever Z
;;;; goes over that number the drawing is done and a "vbw.pdf" file will
;;;; be written to disk. Also, while the program is running a picture of
;;;; the current progress will be saved every 256 generations as
;;;; "tmp.png".  If you use a picture viewer that refreshes whenever the
;;;; "tmp.png" file has changed you will have a live update of the
;;;; progress.
;;;;
;;;; ### Unix (Linux, Ubuntu, etc.)
;;;;
;;;; 1. Preferably install SBCL using your distribution's package
;;;;    manager;
;;;; 2. Unpack the archive and enter the directory it created;
;;;; 3. Run this program with: `./start-drawing.sh`;
;;;; 4. Wait... (the run can be aborted with Control-C);
;;;; 5. Once finished the result will be saved to "vbw.pdf".
;;;;
;;;; It is possible to download and install an archive from
;;;; [http://www.sbcl.org/](http://www.sbcl.org/) but you will need to
;;;; read to installation instructions carefully.  However, if you have
;;;; installed SBCL like this and it can be run by just typing `sbcl` on
;;;; the commandline then issuing `./start-drawing.sh` should work.
;;;;
;;;; ### OS X
;;;;
;;;; 1. Install [Homebrew](http://mxcl.github.com/homebrew/);
;;;; 2. Install SBCL: `brew install sbcl`;
;;;; 3. Unpack the archive and enter the directory it created;
;;;; 4. Run this program with: `./start-drawing.sh`;
;;;; 5. Wait... (the run can be aborted with Control-C);
;;;; 6. Once finished the result will be saved to "vbw.pdf".
;;;;
;;;; It is possible to download and install an archive from
;;;; [http://www.sbcl.org/](http://www.sbcl.org/) but you will need to
;;;; read to installation instructions carefully.  However, if you have
;;;; installed SBCL like this and it can be run by just typing `sbcl` on
;;;; the commandline then issuing `./start-drawing.sh` should work.
;;;;
;;;; ### Windows
;;;;
;;;; 1. Install SBCL (x86) from: [http://www.sbcl.org/platform-table.html](http://www.sbcl.org/platform-table.html)
;;;;    (just accept all defaults when going through the dialogs);
;;;; 2. Maybe reboot;
;;;; 3. Unpack the archive and enter the directory it created;
;;;; 4. Run this program by double-clicking `./start-drawing.bat`;
;;;; 5. Wait... (the run can be aborted with Control-C);
;;;; 6. Once finished the result will be saved to "vbw.pdf".

;;; ## Initialization

;; ### Quicklisp
;;
;; We **need** Quicklisp!
;;
;; Since this program is distributed to people with no SBCL experience
;; it should be as easy as possible for them to run it.  They should not
;; have to install Quicklisp by hand, so `quicklisp.lisp` is distributed
;; with this program and will automatically be installed.  However, for
;; people with working Quicklisp installations this program should work
;; without any issues as well.
;;
;; This approach should be good enough:
#-quicklisp
(progn (load "quicklisp.lisp")
       (handler-case (funcall (intern "INSTALL" :quicklisp-quickstart))
         ;; This will break if setup.lisp hasn't been installed in the default
         ;; spot (but then, how did we get here in the first place?!).
         (simple-error () (let ((init (merge-pathnames "quicklisp/setup.lisp"
                                                     (user-homedir-pathname))))
                            (if (probe-file init)
                                (load init)
                                (format *error-output* "~&Could not install ~
                                        Quicklisp, aborting...~%"))))))

;;; ### Packages
;;;
;;; This section takes care of any external libraries used by this
;;; program and it creates a `victory-boogie-woogie` package to do its
;;; work in.

(in-package :cl)

(ql:quickload :cl-pdf)
(ql:quickload :png-read)
(ql:quickload :zpng)

(defpackage :victory-boogie-woogie
  (:nicknames :vbw)
  (:use :cl))

(in-package :vbw)


;; Silence any style warnings generated by SBCL so as not to confuse the
;; users of this script.  This has only been enabled after development
;; on this program was finished.

 #+sbcl (declaim (sb-ext:muffle-conditions style-warning))


;;; ### Globals

;; The maximum value each of the red, green, blue and alpha components
;; can take. This is more or less dictated by
;; [ZPNG](http://www.xach.com/lisp/zpng/).
;;
;; Since this program is so focused on manipulating PNGs and using ZPNG
;; objects it is a passable offense to be clear about it and just
;; declaring 255 here:
(defparameter +max-rgba+ 255)


;;; ## Class: `drawing`
;;;
;;; This class combines all the data needed for a drawing in one neat
;;; package:

;; - **genome** : contains the set of genes currently being worked on,
;;                each gene is represented as a circle on the screen;
;; - **bg-genome** : the genes that make up the background;
;; - **background** : the background as a ZPNG object (for speed, so it
;;                    has not to be drawn again every generation);
;; - **fitness** : this instance's fitness, the higher the better;
;; - **png** : `bg-genome` + `genome` drawn as a ZPNG object, this is
;;             used to calculate the fitness against the reference
;;             picture (what really happens is `background` is copied
;;             and `genome` is drawn on that);
;; - **width** : the reference PNG's width;
;; - **height** : the reference PNG's height.
;;
;; More information about Common Lisp classes can be found in the
;; [Practical Common Lisp](http://www.gigamonkeys.com/book/object-reorientation-classes.html)
;; book.
(defclass drawing ()
  ((genome     :accessor genome     :initarg :genome)
   (bg-genome  :accessor bg-genome  :initarg :bg-genome)
   (background :accessor background :initarg :background)
   (fitness    :reader   fitness    :initarg :fitness)
   (png        :reader   png        :initarg :png)
   (width      :reader   width      :initarg :width)
   (height     :reader   height     :initarg :height)))


;; Since the default printed representation of `drawing` contains too
;; little information new `print-object` method is defined with
;; different output:
(defmethod print-object ((obj drawing) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "f=~,5E g=~D" (fitness obj) (length (genome obj)))))


;;; ## Creation

;; The most fundamental function in this program is
;; `create-random-gene`.  It creates a gene which is a container of
;; values.  These values define what a circle on the drawing will look
;; like.
;;
;; `create-random-gene` creates a vector formatted as follows:
;;
;; - `#(red green blue alpha x y radius)`
;;
;; Red, green and blue together make up the color of the circle, alpha
;; the transparency, x and y the position and radius the size of the
;; circle.
(defun create-random-gene (reference &optional (size 16))
  "Returns a random gene: #(red green blue alpha x y radius) of which
  all values are integers.  SIZE denotes the radius of the circle and
  denotes the size in pixels in a PNG.  There's no 1:1 relationship when
  generating a PDF.  REFERENCE should be a ZPNG object and SIZE should
  be an integer."
  (let ((max-rgb (expt 2 (zpng::bpp reference))))
    ;; XXX set +max-rgba+ here?
    (vector (random max-rgb) (random max-rgb) (random max-rgb) (random max-rgb)
            (random (zpng:width reference)) (random (zpng:height reference))
            size)))


;; These genes are then wrapped up in a genome which is a container for
;; genes.  This genome makes up the top layer of the drawing: the layer
;; currently being evolved.
(defun create-random-genome (reference &optional (length 16) (size 16))
  "Creates a genome of size LENGTH.  SIZE is passed on to the
  CREATE-RANDOM-GENE function and denotes the radius of the circle.
  REFERENCE should be a ZPNG object. LENGTH and SIZE should be
  integers."
  (when (<= length 0)
    (return-from create-random-genome nil))
  (loop with arr = (make-array (list length))
        for i from 0 below length
        do (setf (svref arr i) (create-random-gene reference size))
        finally (return arr)))


;; Since the `drawing` class is used as a container for all the data
;; pertaining to a drawing higher level functions are needed to make the
;; program easier to read (and use while working on it).
(defun make-drawing (reference background genome bg-genome)
  (let* ((png (draw-genome background genome))
         (fitness (calculate-fitness reference png)))
    (make-instance 'drawing :genome genome :bg-genome bg-genome
                   :fitness fitness :png png :background background
                   :width (zpng:width png) :height (zpng:height png))))


(defun create-random-drawing (reference &optional (length 128) (size 16))
  (make-drawing reference (empty-png reference)
                (create-random-genome reference length size)
                (make-array '(0) :fill-pointer 0)))



;;; ## Evolution
;;;
;;; The initial generated drawing is random.  This drawing is improved
;;; by randomly moving around circles and changing their colors and
;;; transparency.  Whenever the result of these random operations is
;;; better than the current best drawing it becomes the new best drawing
;;; and will serve as the foundation for further operations.

;; `evolve-gene` is the most basic evolution function for this program.
;; It evolves a single gene (see `create-random-gene`) by randomly
;; calling a modification function on it.  Observe that 90% of the time
;; one of those functions is called with a default `delta` and 10% of
;; the time one of those functions is called with an excessive `delta`
;; ten times as large as the default.  This is to avoid getting stuck on
;; local maxima.
(defun evolve-gene (reference gene &optional delta)
  "Evolves a GENE by calling either MODIFY-COLOR or MODIFY-POSITION on it.
  REFERENCE should be a ZPNG object, DELTA should be an integer.  See
  CREATE-RANDOM-GENE for the structure of GENE."
  (unless delta
    (setf delta (zpng::bpp reference)))
  (let ((random-nr (random 1.0)))
    (cond ((< random-nr 0.05) (modify-color reference gene (* 10 delta)))
          ((< random-nr 0.10) (modify-position reference gene (* 10 delta)))
          ((< random-nr 0.55) (modify-color reference gene delta))
          (t                  (modify-position reference gene delta)))))


;; During development it was settled upon to change four genes per
;; generation.  There is no specific reason.  Initially it was one gene
;; per generation but the drawing evolved too slow, so it was
;; arbitrarily set to four which worked well enough.  It was never again
;; looked at due to other issues being more pressing.  Note that it is
;; possible for the same gene to be selected multiple times,
;; i.e. there's no list kept of previously selected genes.
(defun evolve-genome (reference genome)
  "Returns a copy of GENOME with modified genes (also copies).
  REFERENCE should be a ZPNG object."
  (loop with len = (length genome)
        with new-genome = (copy-seq genome)
        repeat 4  ; hard-coded :-|
        for rnr = (random len)
        for new-gene = (evolve-gene reference (elt genome rnr))
        do (setf (elt new-genome rnr) new-gene)
        finally (return new-genome)))


;; Again, a higher level function is needed for working with the
;; `drawing` class.  If this is not done a lot of code will be repeated
;; in the program (look for the calls to `evolve-drawing`).
(defun evolve-drawing (reference drawing)
  (let ((new-genome (evolve-genome reference (genome drawing))))
    (make-drawing reference (background drawing) new-genome
                  (bg-genome drawing))))


;; The modification of a gene is split up into two logical parts: the
;; modification of the color and the modification of the position.  In
;; this case alpha is part of the color as well although a point could
;; be made for turning it into a third logical part.
;;
;; Splitting the modification functions both makes the code simpler and
;; also prevents too many changes per generation.  From personal
;; experience too many changes turn the evolution into a random search
;; again, going nowhere.  However, it might very well be possible that
;; this wouldn't be an issue for this program.  This has not been
;; tested.
(defun modify-color (reference gene &optional delta)
  "Returns a modified COPY of GENE.  DELTA denotes the maximum amount of
  change.  REFERENCE should be a ZPNG object.  See CREATE-RANDOM-GENE
  for the structure of GENE."
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
  "Returns a modified COPY of GENE.  DELTA denotes the maximum amount of
  change.  REFERENCE should be a ZPNG object.  See CREATE-RANDOM-GENE
  for the structure of GENE."
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
               (<  x (zpng:width reference)))
      (incf (elt new-gene 4) dx))
    (when (and (>= y 0)
               (<  y (zpng:height reference)))
      (incf (elt new-gene 5) dy))
    new-gene))


;; Whenever a new genome has been created the drawing is turned into a
;; PNG and that PNG's fitness is calculated by comparing it against the
;; reference picture.  This is done by comparing the red, green and blue
;; components of each pixel against those of the reference picture and
;; summing up the differences.  Finally 1 is divided by "sum + 1" (+1 to
;; avoid division by zero) so the values for good to bad fitness go from
;; one to zero.
(defun calculate-fitness (reference png)
  "Calculates the fitness of PNG and returns a value from 1.0 (good) to
  0.0 (bad).  Both REFERENCE and PNG should be ZPNG objects."
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


;;; ## Drawing
;;;
;;; This chapter describes the functions used to draw to the program's
;;; internal PNG representation.

;; `draw-genome` is the interface between the genome data and the
;; drawing functions.  It goes over each gene in the order they appear
;; in the genome and calls `draw-filled-circle` with the data from the
;; gene.
(defun draw-genome (background genome)
  "Returns a new ZPNG object: a copy of BACKGROUND with GENOME drawn on
  top of it."
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


;; This is the [midpoint circle algorithm](http://en.wikipedia.org/wiki/Midpoint_circle_algorithm)
;; as described on Wikipedia.  However it still produces some artifacts
;; for circles with a big radius and transparency lower than 1.0:
;; horizontal lines seem to be drawn twice sometimes.  This isn't really
;; an issue for our current program.
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


;; One of the two optimized functions.  The extra optimization code
;; makes it look a little more complex than it actually is.  All it
;; really does it drawing a horizontal line by calling
;; `set-pixel-unsafe` for each pixel on the line and performing the
;; clipping so `set-pixel-unsafe` doesn't draw outside of the PNG's
;; allocated memory.
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


;; The other optimized function and the function that gets called the
;; most in this program.  It is called "unsafe" because no checks are
;; made to see whether any of the input arguments fall within the
;; allowed parameters (this gives significant speed gains).  You are
;; supposed to do these checks in a higher level function.
;;
;; The alpha blending code is from: [https://www.gamedev.net/topic/34688-alpha-blend-formula/](https://www.gamedev.net/topic/34688-alpha-blend-formula/]).
;;
;; Again, due to the optimization code this function looks like a mess
;; but it is worth it.
(defun set-pixel-unsafe (png x y r g b
                         &optional (a +max-rgba+) (max-rgb +max-rgba+))
  "PNG is a ZPNG object.
  X and Y are integers, must be greater or equal to 0 and less than the
  width and height of the PNG.  R, G, B and A are values between 0 and
  1 (inclusive)."
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


;;; ## Reading and Writing PNGs
;;;
;;; These functions read and write PNG files.  The PNG-READ package is
;;; (obviously) used for reading PNGs and the ZPNG package is used for
;;; the internal representation and for writing PNG files.  For most of
;;; the development time it was planned to use a huge PNG file for the
;;; final result.  After asking some questions to the organization it
;;; became apparent that a PDF would be better suited for printing.
;;;
;;; PNGs are still used for the reference pictures and for writing out
;;; the intermediate results of the evolution.  It might very well be
;;; possible for the program to be made faster by using a custom
;;; internal representation of the drawing and the reference picture.

(defun read-png (path)
  "Reads the PNG file at PATH and returns a ZPNG object."
  (let* ((png-in (png-read:read-png-file path))
         (png-out ;; FIXME candidate for an EMPTY-PNG call
                  (make-instance 'zpng:png :color-type :truecolor
                                 :width (png-read:width png-in)
                                 :height (png-read:height png-in)))
         (data-in (png-read:image-data png-in))
         (data-out (zpng:data-array png-out)))
    (loop for y from 0 below (png-read:height png-in)
          do (loop for x from 0 below (png-read:width png-in)
                   do ;; convert from PNG-READ to ZPNG format
                      (setf (aref data-out y x 0) (aref data-in x y 0)
                            (aref data-out y x 1) (aref data-in x y 1)
                            (aref data-out y x 2) (aref data-in x y 2))))
    png-out))


(defun write-png (png &optional (path "tmp.png"))
  "Writes a ZPNG object to PATH."
  (zpng:write-png png path))


(defun save-drawing (drawing &optional (path "tmp.png"))
  "Saves DRAWING as a PNG file to PATH."
  (write-png (png drawing) path))


(defun empty-png (reference)
  (make-instance 'zpng:png :color-type :truecolor
                 :width (zpng:width reference)
                 :height (zpng:height reference)))


;;; ## Writing PDFs

;; During development it became clear that outputting to PDF instead of
;; a giant PNG was to be preferred.  `resolution-independent-drawing`
;; converts a `drawing` instance aimed at producing PNGs into a new
;; instance that is not tied to a specific resolution.  This new
;; instance can then be used to produce a PDF.
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
                          :fitness (fitness drawing)
                          :png (zpng:copy-png (png drawing))
                          :background (zpng:copy-png (background drawing))
                          :width width :height height))))


;; *Note*: this function has never been tested on anything but A4 in a
;; portrait orientation.
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


;;; ## Main Program

;; This function ties together all the previously described functions
;; and is what needs to be called to make a drawing.  The
;; `start-drawing.bat` and `start-drawing.sh` scripts call this function
;; as well.
;;
;; It is possible to play around with making drawings by not using the
;; start scripts but instead loading this file into SBCL:
;;
;; - `sbcl --load victory-boogie-woogie-genetic-algorithm.lisp`
;;
;; On the REPL go into the `vbw` package with `(in-package :vbw)` and
;; issue for example:
;;
;; - `(main "reference-pictures/victory-boogie-woogie-marie-ll-flickr-512x512-rotated-45.png" :genome-length 4 :min-size 2 :max-dgen 256 :target-fitness 3e-10)`
;;
;; For "vbw-example.pdf" the following command was used:
;;
;; - `(main "reference-pictures/victory-boogie-woogie-marie-ll-flickr-512x512-rotated-45.png" :genome-length 4 :min-size 2 :max-dgen 448 :target-fitness 2e-9)`
;;
;; The settings that have the most influence are:
;;
;; - **max-dgen** : the lower this value the quicker the program will
;;                  decrease the circle size and increase the number of
;;                  genes in the genome;
;; - **target-fitness**: the higher the target fitness the more of an
;;                       exact copy of the reference picture will be
;;                       made (by setting this too high the evolution
;;                       might never finish!).
;;
;; By reading this function from top to bottom you should get a good
;; impression of the high level functioning of this program.
(defun main (reference-path &key (genome-length 4) (min-size 2) (size 512)
                                 (target-fitness 1e-9) (max-dgen 384)
                                 (png-out-path "tmp.png"))
  (let* ((*random-state* (make-random-state t))
         (ref (read-png reference-path))
         (drw (create-random-drawing ref genome-length size)))
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
             ;; (Unless we are using the smallest brush already.)  We
             ;; also start with a fresh genome and use what we have made
             ;; so far as its background.
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
                                                            size)
                     (background drw) (zpng:copy-png (png drw))
                     drw              (evolve-drawing ref drw))
               (format t "*** Switching to genome-length=~D and size=~D.~%"
                       genome-length size))
             (when (= 0 (mod gen 256))
               (save-drawing drw png-out-path)))
    (save-drawing drw png-out-path)
    drw))


;; When making a binary using `sb-ext:save-lisp-and-die` we need to
;; supply a top level function for it to start at.
(defun toplevel-for-exe ()
  (write-pdf
    (resolution-independent-drawing
      (vbw::main "reference-pictures/victory-boogie-woogie-marie-ll-flickr-512x512-rotated-45.png"))
    "vbw.pdf")
  (quit))


;;;; ## An Attempt at Literary Programming
;;;;
;;;; This document has been created by running
;;;; [victory-boogie-woogie-genetic-algorithm.lisp](victory-boogie-woogie-genetic-algorithm.lisp)
;;;; through [src2markup](https://github.com/aerique/src2markup#readme)
;;;; and [Markdown](http://daringfireball.net/projects/markdown/).
