;;;; victory-boogie-woogie-genetic-programming.lisp
;;;;
;;;; The painting has been turned 45 degrees clockwise!
;;;;
;;;; - use SUBTYPEP to check for subtypes, f.e. (subtypep 'integer 'number)

;;; Packages

(in-package :cl)

(asdf:oos 'asdf:load-op :png-read)
(asdf:oos 'asdf:load-op :zpng)

;(push (merge-pathnames "3rd-party/baby-steps/" *default-pathname-defaults*)
;  asdf:*central-registry*)
;(asdf:oos 'asdf:load-op :baby-steps)


(defpackage :victory-boogie-woogie
  (:nicknames :vbw)
  (:use :cl))

(in-package :vbw)


;;; Functions

;; SUBTYPEP!
(defun get-items-of-type (items type)
  (loop for item in items
        when (equal (getf item :type) type) collect item
        when (equal type t) collect item))


(defun random-elt (sequence)
  "Returns a random element from SEQUENCE."
  (let ((length (length sequence)))
    (when (> length 0)
      (elt sequence (random length)))))


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


(defun create-program-full (functions terminals
                            &key (fn-type t) (max-arity 4) (max-depth 4))
  (let ((fn (random-elt (get-items-of-type functions fn-type))))
    (append (list (getf fn :function))
            (loop with args = (getf fn :args)
                  repeat (if (equal (elt args 0) '&rest)
                             ;; use (+ (random max-arity) 1) for :full as well?
                             max-arity
                             (length args))
                  for i from 0
                  for item-type = (cond ((equal (elt args 0) '&rest)
                                         (elt args 1))
                                        (t (elt args i)))
                  for items = (get-items-of-type
                               (cond ((and (listp item-type)
                                           (equal (elt item-type 0) 'terminal))
                                      (append functions terminals))
                                     ((> max-depth 0) functions)
                                     (t terminals))
                               (if (and (listp item-type)
                                        (equal (elt item-type 0) 'terminal))
                                   (elt item-type 1)
                                   item-type))
                  collect (let ((item (random-elt items)))
                            (if (getf item :function)
                                (create-program-full functions terminals
                                                    :fn-type item-type
                                                    :max-arity max-arity
                                                    :max-depth (- max-depth 1))
                                (getf item :terminal)))))))


(defun create-program-grow (functions terminals
                            &key (fn-type t) (max-arity 4) (max-depth 4))
  (let ((fn (random-elt (get-items-of-type functions fn-type))))
    (append (list (getf fn :function))
            (loop with args = (getf fn :args)
                  repeat (if (equal (elt args 0) '&rest)
                             ;(+ (random max-arity) 1)
                             max-arity
                             (length args))
                  for i from 0
                  for item-type = (cond ((equal (elt args 0) '&rest)
                                         (elt args 1))
                                        (t (elt args i)))
                  for items = (get-items-of-type
                               (if (> max-depth 0)
                                   (append functions terminals)
                                   terminals)
                               ;item-type)
                               (if (and (listp item-type)
                                        (equal (elt item-type 0) 'terminal))
                                   (elt item-type 1)
                                   item-type))
                   collect (let ((item (random-elt items)))
                            (if (getf item :function)
                                (create-program-grow functions terminals
                                                    :fn-type item-type
                                                    :max-arity max-arity
                                                    :max-depth (- max-depth 1))
                                (getf item :terminal)))))))


(defun create-program (functions terminals
                       &key (fn-type t) (max-arity 4) (max-depth 4)
                            (type :full))
  (cond ((equal type :full)
         (create-program-full functions terminals :fn-type fn-type
                              :max-arity max-arity :max-depth max-depth))
        ((equal type :grow)
         (create-program-grow functions terminals :fn-type fn-type
                              :max-arity max-arity :max-depth max-depth))
        (t (error "Unknown program creation process: ~S" type))))


(defun create-population (functions terminals
                          &key (max-arity 4) (max-depth 4) (size 16)
                               (type :ramped-half-half))
  (loop with vec = (make-array (list size))
        for i from 0 below size
        for program = (cond ((equal type :full)
                             (create-program functions terminals :type :full
                                             :max-arity max-arity
                                             :max-depth max-depth))
                            ((equal type :grow)
                             (create-program functions terminals :type :grow
                                             :max-arity max-arity
                                             :max-depth max-depth))
                            ((and (equal type :ramped-half-half)
                                  (< i (ceiling (/ size 2))))
                             (create-program functions terminals :type :full
                                             :max-arity max-arity
                                             :max-depth max-depth))
                            ((equal type :ramped-half-half)
                             (create-program functions terminals :type :grow
                                             :max-arity max-arity
                                             :max-depth max-depth))
                            (t (error "Unknown population creation process: ~S"
                                      type)))
        do (setf (elt vec i) program)
        finally (return vec)))


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


(defun draw-filled-square (png x y width/2 r g b &optional (a 255))
  (when (= width/2 0)
    (set-pixel-unsafe png x y r g b a)
    (return-from draw-filled-square))
  (loop for y0 from (- y width/2) to (+ y width/2)
        do (draw-horizontal-line png (- x width/2) (+ x width/2) y0 r g b a)))


(defun random255 ()
  (random 256))


(defun empty-png (width height)
  (make-instance 'zpng:png :color-type :truecolor :width width :height height))


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


;(defun make-function (tree terminals)
;  "Turns TREE into a function object."
;  (let ((*error-output* (make-broadcast-stream))  ; thanks stassats!
;        (input-args (loop for terminal in terminals
;                          when (getf terminal :input)
;                            collect (getf terminal :terminal))))
;    ;(eval (append1 '(lambda (=input=)) tree))))
;    (eval (append1 `(lambda ,input-args) tree))))

(defun make-function (body)
  (eval (append '(lambda (=png=)) (list body))))


(defun run-program (body &optional (width 256) (height 256))
  (let ((png (empty-png width height)))
    (funcall (make-function body) png)
    png))


;;; Main Program

(defun main (&key (max-arity 4) (max-depth 4) (size 16)
                  (type :ramped-half-half))
  (let* ((functions  '((:function +     :type integer :args (&rest integer))
                       (:function -     :type integer :args (&rest integer))
                       (:function *     :type integer :args (&rest integer))
                       (:function progn :type t       :args (&rest t))
                       ;; more progns!
                       (:function progn :type t       :args (&rest t))
                       (:function draw-filled-circle :type t
                        :args ((terminal png) integer integer integer integer
                               integer integer integer))
                       ))
         ;; =foo= denotes external inputs
         (terminals  '(;; these should be replaced by #'random10
                       (:terminal 0 :type integer) (:terminal 1 :type integer)
                       (:terminal 1 :type integer) (:terminal 2 :type integer)
                       (:terminal 3 :type integer) (:terminal 4 :type integer)
                       (:terminal 5 :type integer) (:terminal 6 :type integer)
                       (:terminal 7 :type integer) (:terminal 8 :type integer)
                       (:terminal 9 :type integer)
                       ;(:terminal (random255) :type integer)
                       (:terminal =png= :type png))))
    (create-population functions terminals :max-arity max-arity
                       :max-depth max-depth :size size :type type)))
