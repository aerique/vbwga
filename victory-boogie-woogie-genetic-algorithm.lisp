;;;; victory-boogie-woogie-genetic-algorithm.lisp
;;;;
;;;; Functional (as in programming paradigm) rewrite.
;;;;
;;;; - avconv -f image2 -i mona-lisa-%06d.bmp -r 12 -s 256x256 mona-lisa.avi
;;;; - avconv -f image2 -i mona-lisa-%06d.bmp -s 256x256 -vf setpts=0.1*PTS mona-lisa.avi
;;;;
;;;; convert-surface, copy-surface & draw-surface don't work (at least when
;;;; the destination isn't *default-display* but a surface made with
;;;; create-surface... however draw-surface-at-* does work (with x=0 and y=0)
;;;;
;;;; target fitness is 5.0e-8?  seems good from Mona Lisa trials
;;;;
;;;; g=330201  f=5.000e-8  for mona-lisa trial
;;;;
;;;; alternatives:
;;;; - voronoi
;;;; - small circles all the same size, pop=x*y/4
;;;; - optimize one circle at a time, p=1, accept local maxima
;;;; - instead of random colours use the histogram of the target as probability
;;;;
;;;; Couldn't find 64-bit SDL_gfx.dll so needs 32-bit SBCL and SDL libs.
;;;;
;;;; Install libsdl_gfx for transparency.
;;;;
;;;; The painting has been turned 45 degrees clockwise!

;;; Packages

(asdf:oos 'asdf:load-op :eager-future2)
(asdf:oos 'asdf:load-op :lispbuilder-sdl)

(in-package :lispbuilder-sdl)


;;; Classes

;; A genome is a list containing one or more '(x y radius red green blue)
;; elements.  x, y, radius, red, green and blue are all floats between 0 and
;; 1.  These will be translated to coordinates, size and color values when a
;; drawing is converted to a PNG.
(defclass drawing ()
  ((fitness :reader fitness :initarg :fitness)
   (genome  :reader genome  :initarg :genome)
   (surface :reader surface :initarg :surface)))


;;; Methods

(defmethod print-object ((obj drawing) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "fitness:~,3E elements:~D"
            (fitness obj) (length (genome obj)))))


;;; Functions

(defun draw-genome (genome reference background)
  (let* ((width (width (surface reference)))
         (height (height (surface reference)))
         (surface (create-surface width height)))
    (draw-surface-at-* background 0 0 :surface surface)
    (loop for gene across genome
          for color = (color :r (* 255 (svref gene 0))
                             :g (* 255 (svref gene 1))
                             :b (* 255 (svref gene 2))
                             :a (* 255 (svref gene 3)))
          for x = (floor (* width (svref gene 4)))
          for y = (floor (* height (svref gene 5)))
          for r = (floor (* (svref gene 6) (if (> height width) width height)))
          do (draw-filled-circle-* x y r :color color :surface surface))
    surface))


(defun make-drawing (fitness genome surface)
  (make-instance 'drawing :fitness fitness :genome genome :surface surface))


(defun calculate-fitness (reference-surface surface)
  (with-pixels ((s1 (fp reference-surface))
                (s2 (fp surface)))
    (loop with difference = 0
          with width = (width reference-surface)
          with height = (height reference-surface)
          for y from 0 below height
          do (loop for x from 0 below width
                   for p1 = (read-pixel s1 x y)
                   for r1 = (ash (logand p1 #xff0000) -16)
                   for g1 = (ash (logand p1 #x00ff00)  -8)
                   for b1 =      (logand p1 #x0000ff)
                   for p2 = (read-pixel s2 x y)
                   for r2 = (ash (logand p2 #xff0000) -16)
                   for g2 = (ash (logand p2 #x00ff00)  -8)
                   for b2 =      (logand p2 #x0000ff)
                   for dr = (- r1 r2)
                   for dg = (- g1 g2)
                   for db = (- b1 b2)
                   do (incf difference (+ (* dr dr) (* dg dg) (* db db))))
          finally (return (/ 1 (1+ difference))))))


;; #(red green blue alpha x y radius)
(defun create-random-gene (radius)
  (vector (random 1.0) (random 1.0) (random 1.0) (random 1.0)
          (random 1.0) (random 1.0) radius))


(defun create-random-genome (max-length radius)
  (when (<= max-length 0)
    (return-from create-random-genome nil))
  (loop with len = max-length
        with arr = (make-array (list len))
        for i from 0 below len
        do (setf (svref arr i) (create-random-gene radius))
        finally (return arr)))


(defun create-drawing (reference genome background)
  (let ((surface (draw-genome genome reference background)))
    (make-drawing (calculate-fitness (surface reference) surface)
                  genome surface)))


(defun create-random-drawing (reference max-genome-length radius background)
  (create-drawing reference (create-random-genome max-genome-length radius)
                  background))


(defun draw-drawing (drawing &optional (offset 0))
  (draw-surface-at-* (surface drawing) offset 0))


(defun draw-surface (surface &optional (offset 0))
  (draw-surface-at-* surface offset 0))


(defun modify-color (drawing reference background)
  (let ((new-genome
         (loop with genome = (genome drawing)
               with length = (length genome)
               with index = (random length)
               with vec = (make-array length)
               for gene across genome
               for i from 0
               if (= i index)
                 do (let ((new-gene (copy-seq gene))
                          (dr (- (random 0.08) 0.04))
                          (dg (- (random 0.08) 0.04))
                          (db (- (random 0.08) 0.04))
                          (da (- (random 0.08) 0.04)))
                      (when (and (>= (+ (elt new-gene 0) dr) 0)
                                 (<  (+ (elt new-gene 0) dr) 1.0))
                        (incf (elt new-gene 0) dr))
                      (when (and (>= (+ (elt new-gene 1) dg) 0)
                                 (<  (+ (elt new-gene 1) dg) 1.0))
                        (incf (elt new-gene 1) dg))
                      (when (and (>= (+ (elt new-gene 2) db) 0)
                                 (<  (+ (elt new-gene 2) db) 1.0))
                        (incf (elt new-gene 2) db))
                      (when (and (>= (+ (elt new-gene 3) da) 0)
                                 (<  (+ (elt new-gene 3) da) 1.0))
                        (incf (elt new-gene 3) da))
                      (setf (svref vec i) new-gene))
               else
                 do (setf (svref vec i) (copy-seq gene))
               finally (return vec))))
    (create-drawing reference new-genome background)))


(defun modify-position (drawing reference background)
  (let ((new-genome
         (loop with genome = (genome drawing)
               with length = (length genome)
               with index = (random length)
               with vec = (make-array length)
               for gene across genome
               for i from 0
               if (= i index)
                 do (let ((new-gene (copy-seq gene))
                          (dx (- (random 0.08) 0.04))
                          (dy (- (random 0.08) 0.04)))
                      (when (and (>= (+ (elt new-gene 4) dx) 0)
                                 (<  (+ (elt new-gene 4) dx) 1.0))
                        (incf (elt new-gene 4) dx))
                      (when (and (>= (+ (elt new-gene 5) dy) 0)
                                 (<  (+ (elt new-gene 5) dy) 1.0))
                        (incf (elt new-gene 5) dy))
                      (setf (svref vec i) new-gene))
               else
                 do (setf (svref vec i) (copy-seq gene))
               finally (return vec))))
    (create-drawing reference new-genome background)))


(defun evolve (drawing reference background)
  (let ((rnr (random 1.0)))
    (cond ((< rnr 0.5) (modify-color drawing reference background))
          (t           (modify-position drawing reference background)))))


(defun load-reference (path)
  (make-drawing 1.0 nil (load-image path)))


;;; Main Program

;; make min-radius one step smaller than 0.0125?
(defun main (reference-path &key (max-genome-length 128) (min-radius 0.00625)
             (target-fitness 5e-8) (threads 1) (visualize t))
  (with-init ()
    (window 128 128 :title-caption "float-me" :flags '(sdl-sw-surface))
    (let* ((gen 0)
           (last-gen-update 0)
           (genome-length 8)
           (radius 0.8)
           (ref (load-reference reference-path))
           (bg (create-surface (width (surface ref)) (height (surface ref))))
           (candidate (create-random-drawing ref genome-length radius bg))
           (result-genomes (make-array 0 :fill-pointer 0)))
      (if visualize
          (progn (resize-window (+ 2 (* 3 (width (surface ref))))
                                 (height (surface ref)))
                 (draw-drawing candidate (+ 1 (width (surface ref))))
                 (draw-surface bg (+ 2 (* 2 (width (surface ref))))))
          (resize-window (width (surface ref)) (height (surface ref))))
      (draw-drawing ref)
      (update-display)
      (with-events ()
        (:key-down-event (:key key) (when (or (key= key :sdl-key-escape)
                                              (key= key :sdl-key-q))
                                      (push-quit-event)))
        (:quit-event () t)
        (:video-expose-event () (update-display))
        (:idle ()
          (incf gen)
          (when (>= (- gen last-gen-update) 128)
            (when (< genome-length max-genome-length)
              (setf genome-length (* genome-length 2)))
            (setf radius (if (<= radius min-radius)
                             (* 2 min-radius)
                             (/ radius 2)))
            (draw-surface-at-* (surface candidate) 0 0 :surface bg)
            (loop for gene across (genome candidate)
                  do (vector-push-extend gene result-genomes))
            (setf candidate (create-random-drawing ref genome-length radius bg)
                  last-gen-update gen))
          (let ((new-candidates (loop repeat threads
                                      collect (eager-future2:pcall
                                               (lambda ()
                                                 (evolve candidate ref bg))))))
            (loop with all-futures-done = nil
                  until all-futures-done
                  do (loop with done = t
                           for future in new-candidates
                           do (unless (eager-future2:ready-to-yield? future)
                                (setf done nil))
                           finally (when done (setf all-futures-done done))))
            (setf new-candidates (loop for future in new-candidates
                                       collect (eager-future2:yield future))
                  new-candidates (sort new-candidates
                                       (lambda (a b)
                                         (> (fitness a) (fitness b)))))
            (when (> (fitness (first new-candidates)) target-fitness)
              (when visualize
                (draw-drawing candidate (+ 1 (width (surface ref))))
                (draw-surface bg (+ 2 (* 2 (width (surface ref)))))
                (update-display))
              (format t "~D: target fitness reached...~%" gen)
              (return-from main (append result-genomes (genome candidate))))
            (when (> (fitness (first new-candidates)) (fitness candidate))
              (setf candidate (first new-candidates))
              (when visualize
                (draw-drawing candidate (+ 1 (width (surface ref))))
                (draw-surface bg (+ 2 (* 2 (width (surface ref)))))
                (update-display))
              (format t "[~D] f=~,8E len=~D r=~,4F~%" gen (fitness candidate)
                      (length (genome candidate)) radius)
              (setf last-gen-update gen))))))))
