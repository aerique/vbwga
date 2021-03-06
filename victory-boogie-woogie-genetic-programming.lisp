;;;; victory-boogie-woogie-genetic-programming.lisp
;;;;
;;;; This was supposed to become my second entry for the competition but
;;;; the current approach doesn't work.  It needs a big rework and I might
;;;; continue with it for fun in the future.
;;;;
;;;; The painting has been turned 45 degrees clockwise!
;;;;
;;;; - use SUBTYPEP to check for subtypes, f.e. (subtypep 'integer 'number)
;;;; - a :size about 1/2 to 1/4 of :max-generations seem to be ok

;;; Packages

(in-package :cl)

(asdf:oos 'asdf:load-op :cl-pdf)  ; the end result should be in PDF
(asdf:oos 'asdf:load-op :png-read)
(asdf:oos 'asdf:load-op :zpng)

(asdf:oos 'asdf:load-op :eager-future2)
(rename-package :eager-future2 :eager-future2 '(:ef))

(defpackage :victory-boogie-woogie
  (:nicknames :vbw)
  (:use :cl))

(in-package :vbw)


;;; Stubs to prevent compile warnings.

(defun calculate-n-nodes (tree) (declare (ignore tree)))


;;; Globals

(defparameter *functions*
              '((:function +        :type integer :args (&rest integer))
                (:function -        :type integer :args (&rest integer))
                (:function *        :type integer :args (&rest integer))
                (:function progn    :type progn   :args (&rest drawfn))
                (:function progn    :type drawfn  :args (&rest drawfn))
                (:function progn    :type t       :args (&rest drawfn))
                ;(:function progn    :type t       :args (t t t t t t t t t t))
                (:function repeater :type t       :args (integer integer t))
                (:function draw-line :type drawfn
                 :args ((terminal png) integer integer integer integer
                        integer integer integer integer))
                (:function draw-filled-circle :type drawfn
                 :args ((terminal png) integer integer integer integer
                        integer integer integer))
                (:function draw-filled-square :type drawfn
                 :args ((terminal png) integer integer integer integer
                        integer integer integer))))

;; =foo= denotes external inputs
(defparameter *terminals*
              '(;; ":eval t" means the result of #'random255 should be placed
                ;; in the tree, not #'random255 itself
                (:terminal (random255) :type integer :eval t)
                (:terminal (random511) :type integer :eval t)
                ;; 'free' variables, default to 1... see make-function
                (:terminal =a= :type integer) (:terminal =b= :type integer)
                (:terminal =png= :type png)))

(defparameter *population* nil)


;;; Classes

(defclass program ()
  ((fitness :accessor fitness :initarg :fitness)
   (tree    :accessor tree    :initarg :tree)))


;;; Methods

(defmethod print-object ((obj program) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "fitness:~,5E nodes:~D"
            (fitness obj) (calculate-n-nodes (tree obj)))))


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


(defun calculate-fitness (reference png tree)
  "Both REFERENCE and PNG are ZPNG objects."
  (declare (ignore tree))
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
                            &key (fn-type t) (max-arity 4) (max-depth 2))
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
                                ;(getf item :terminal)))))))
                                (if (getf item :eval)
                                    (eval (getf item :terminal))
                                    (getf item :terminal))))))))


(defun create-program-grow (functions terminals
                            &key (fn-type t) (max-arity 4) (max-depth 2))
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
                                ;(getf item :terminal)))))))
                                (if (getf item :eval)
                                    (eval (getf item :terminal))
                                    (getf item :terminal))))))))


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


(defun empty-png (width height)
  (make-instance 'zpng:png :color-type :truecolor :width width :height height))


(defun make-function (body)
  (let (;; suppress compilation warnings and errors
        (*error-output* (make-broadcast-stream)))  ; thanks stassats!
    (eval (append '(lambda (=png= &optional (=a= 1) (=b= 1))) (list body)))))


(defun run-program (reference body)
  (let ((png (empty-png (zpng:width reference) (zpng:height reference))))
    (handler-case ;; make-function hangs sometimes, not sure why
                  ;; seems to be on bignums / big calculations or something
                  ;; not sure if it is a hang or extreme slowness
                  ;(cl-user::with-timeout 3 (funcall (make-function body) png))
                  ;(cl-user::with-timeout 0.1
                  (cl-user::with-timeout 0.5
                    (funcall (make-function body) png))
      (error () (return-from run-program nil))
      (sb-ext:timeout ()
        ;(break "Timeout occured")))
        (return-from run-program nil)))
    png))


(defun create-population (reference functions terminals
                          &key (debug nil) (max-arity 4) (max-depth 2)
                               (size 16) (type :ramped-half-half))
  (loop with vec = (make-array (list size))
        with i = 0
        until (>= i size)
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
        for png = (run-program reference program)
        for fitness = (if png
                          (calculate-fitness reference png program)
                          nil)
        do (when fitness
             (let ((obj (make-instance 'program :fitness fitness
                                       :tree program)))
               (setf (elt vec i) obj)
               (when debug (format t "[~D/~D] ~S~%" (1+ i) size obj))
               (incf i)))
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


;; Hehe, grabbed this from my own CL-SDL check-in on SourceForge from 2002.
(defun draw-line (png x1 y1 x2 y2 r g b &optional (a 255))
  (when (or (< x1 0) (>= x1 (zpng:width png))
            (< y1 0) (>= y1 (zpng:height png))
            (< x2 0) (>= x2 (zpng:width png))
            (< y2 0) (>= y2 (zpng:height png)))
    (return-from draw-line))
  (let* ((dx (abs (- x1 x2)))
         (dy (abs (- y1 y2)))
         (cx dx)
         (cy dy)
         (xi (if (> x2 x1) 1 -1))
         (yi (if (> y2 y1) 1 -1)))
    (if (> dx dy)
        ;; apparently I had not discovered LOOP yet, or I was in some stupid
        ;; must-not-use-loop-unpure mindset
        (dotimes (condom dx) ; condom is not used
          (when (> cy cx)
            (incf cx dx)
            (incf y1 yi))
          ;(put-pixel x1 y1 color)
          (set-pixel-unsafe png x1 y1 r g b a)
          (incf x1 xi)
          (incf cy dy))
        (dotimes (toothbrush dy) ; toothbrush is not used
          (when (> cx cy)
            (incf cy dy)
            (incf x1 xi))
          ;(put-pixel x1 y1 color)
          (set-pixel-unsafe png x1 y1 r g b a)
          (incf y1 yi)
          (incf cx dx)))))


(defun random255 ()
  "Returns an integer between 0 and 255 (inclusive)."
  (random 256))


(defun random511 ()
  "Returns an integer between 0 and 511 (inclusive)."
  (random 512))


(defun repeater (=a= =b= body)
    (loop for x from 0 to =a=
          for y from 0 to =b=
          do (progn body)))


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
  (when png
    (zpng:write-png png path)))


(defun calculate-n-nodes (tree)
  "Returns the number of nodes in TREE, including the root node and leaves."
  (let ((nodes 1))
    (labels ((traverse-nodes (subtree)
               (loop for node in subtree
                     do (incf nodes)
                        (when (listp node)
                          (traverse-nodes node)))))
      (traverse-nodes tree))
    nodes))


(defun random-node (tree)
  "Returns a random node from TREE."
  (let* ((index 1)
         (nodes-1 (- (calculate-n-nodes tree) 1))
         (random-node (+ (random nodes-1) 1)))
    (labels ((traverse-nodes (subtree)
               (loop for node in subtree
                     do (when (= index random-node)
                          (return-from random-node (list :index index
                                                         :node node)))
                        (incf index)
                        (when (listp node)
                          (traverse-nodes node)))))
      (traverse-nodes tree))))


(defun replace-node (tree node-index new-node)
  "Returns a new tree with NEW-NODE at NODE-INDEX of TREE."
  (let ((index 0))
    (labels ((traverse-nodes (subtree)
               (loop for node in subtree
                     do (incf index)
                     when (= index node-index)
                       collect new-node
                     when (and (/= index node-index)
                               (not (listp node)))
                       collect node
                     when (and (/= index node-index)
                               (listp node))
                       collect (traverse-nodes node))))
      (traverse-nodes tree))))


(defun crossover (tree1 tree2 &key (debug nil))
  "Returns a new tree similar to TREE1 but with a random node replaced by a
  random node from TREE2."
  (let ((rnode1 (random-node tree1))
        (rnode2 (random-node tree2)))
    (when debug
      (format t "tree1: ~S~%tree2: ~S~%rnode1: ~S~%rnode2: ~S~%"
              tree1 tree2 rnode1 rnode2))
    (replace-node tree1 (getf rnode1 :index) (getf rnode2 :node))))


(defun mutate (tree functions terminals &key (debug nil))
  "Replaces a random node in TREE with a random tree.
  Currently always calls CREATE-PROGRAM with :TYPE :GROW internally."
  (let ((rtree (create-program functions terminals :type :grow))
        (rnode (random-node tree)))
    (when debug
      (format t "tree: ~S~%rtree: ~S~%rnode: ~S~%" tree rtree rnode))
    (replace-node tree (getf rnode :index) rtree)))


(defun tournament-selection (population &optional (n-participants 4))
  (loop with len = (length population)
        with indexes = nil
        with i = 0
        until (>= i n-participants)
        for random-nr = (random len)
        do (unless (member random-nr indexes)
             (push random-nr indexes)
             (incf i))
        finally (return (elt (sort (loop for index in indexes
                                         collect (elt population index))
                                   (lambda (a b)
                                     (> (fitness a) (fitness b))))
                             0))))


(defun evolve-population (reference population functions terminals
                          &optional (n-mates 8))
  "N-MATES must be even!"
  (let ((p (make-array (list (length population))
                       :fill-pointer (length population)
                       :initial-contents population))
        (trees (loop with mates = (loop repeat n-mates
                                        collect (tournament-selection
                                                 population))
                     for i from 0 below n-mates by 2
                     for a = (elt mates i)
                     for b = (elt mates (1+ i))
                     for parent = (if (> (fitness a) (fitness b)) a b)
                     for mate   = (if (> (fitness a) (fitness b)) b a)
                     collect (if (< (random 100) 90)
                     ;collect (if (< (random 100) 50)
                                 (crossover (tree parent) (tree mate))
                                 (mutate (tree parent) functions terminals)))))
    (loop for tree in trees
          for png = (run-program reference tree)
          for fitness = (if png
                            (calculate-fitness reference png tree)
                            0)
          do (vector-push-extend (make-instance 'program :fitness fitness
                                                :tree tree)
                                 p))
    (subseq (sort p (lambda (a b) (> (fitness a) (fitness b))))
            0 (length population))))


(defun draw-program (reference program &optional (path "tmp.png"))
  (write-png (run-program reference (tree program)) path))


;;; Main Program

(defun main (reference-path &key (debug nil) (max-arity 5) (max-depth 2)
                                 (max-dgen 384) (size 256)
                                 (type :ramped-half-half)
                                 (target-fitness 1.0e-9)
                                 (png-out-path "tmp.png"))
  (let* ((*random-state* (make-random-state t))
         (ref (read-png reference-path))
         (functions *functions*)
         (terminals *terminals*)
         population)
    (setf population (create-population ref functions terminals :debug debug
                                      :max-arity max-arity :max-depth max-depth
                                      :size size :type type))
    (loop with best-program = (make-instance 'program :fitness 0 :tree nil)
          with dgen = 0
          ;repeat max-generations
          until (> (fitness best-program) target-fitness)
          for i from 1
          for new-population = (evolve-population ref population functions
                                                  terminals)
          for best-new = (elt new-population 0)
          do (when (> (fitness best-new) (fitness best-program))
               (draw-program ref best-new png-out-path)
               (format t "[~8D/~3D] ~S~%" i dgen best-new)
               (setf best-program best-new
                     dgen         0))
             (setf population new-population)
             (incf dgen)
             (when (> dgen max-dgen)
               ;; reinitialize population but keep best program
               (format t "*** dgen > ~D, reinitializing population...~%"
                       max-dgen)
               (format t "[~8D/~3D] ~S~%" i dgen (elt population 0))
               (setf population (create-population ref functions terminals
                                  :debug debug :max-arity max-arity
                                  :max-depth max-depth :size size :type type))
               (setf population
                     (sort population (lambda (a b)
                                        (> (fitness a) (fitness b)))))
               (setf (elt population (- (length population) 1)) best-program)
               (setf population
                     (sort population (lambda (a b)
                                        (> (fitness a) (fitness b)))))
               (format t "[~8D/~3D] ~S~%" i dgen (elt population 0))
               (setf dgen 0)
               (format t "*** done.~%")))
    (setf *population* population)
    (elt population 0)))
