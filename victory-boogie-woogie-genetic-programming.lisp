;;;; victory-boogie-woogie-genetic-programming.lisp
;;;;
;;;; The painting has been turned 45 degrees clockwise!

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

(defun get-items-of-type (items type)
  (loop for item in items
        when (equal (getf item :type) type) collect item
        when (equal type t) collect item))


(defun random-elt (sequence)
  "Returns a random element from SEQUENCE."
  (let ((length (length sequence)))
    (when (> length 0)
      (elt sequence (random length)))))


(defun create-program-full (functions terminals &key (fn-type t) (max-arity 4)
                                                     (max-depth 4))
  (let ((fn (random-elt (get-items-of-type functions fn-type))))
    (append (list (getf fn :function))
            (loop with arity = (getf fn :arity)
                  with fn-type = (getf fn :type)
                  with items = (get-items-of-type (if (> max-depth 0)
                                                      functions
                                                      terminals)
                                                  fn-type)
                  repeat (cond ((typep arity 'number) arity)
                               ((null arity)          0)
                               (t                     (+ max-arity 1)))
                  collect (let ((item (random-elt items)))
                            (if (getf item :function)
                                (create-program-full functions terminals
                                                    :fn-type (getf item :type)
                                                    :max-arity max-arity
                                                    :max-depth (- max-depth 1))
                                (getf item :terminal)))))))


(defun create-program-grow (functions terminals &key (fn-type t) (max-arity 4)
                                                     (max-depth 4))
  (let ((fn (random-elt (get-items-of-type functions fn-type))))
    (append (list (getf fn :function))
            (loop with arity = (getf fn :arity)
                  with fn-type = (getf fn :type)
                  with items = (get-items-of-type (if (> max-depth 0)
                                                      (append functions
                                                              terminals)
                                                      terminals)
                                                  fn-type)
                  repeat (cond ((typep arity 'number) arity)
                               ((null arity)          0)
                               (t                     (+ max-arity 1)))
                  collect (let ((item (random-elt items)))
                            (if (getf item :function)
                                (create-program-grow functions terminals
                                                    :fn-type (getf item :type)
                                                    :max-arity max-arity
                                                    :max-depth (- max-depth 1))
                                (getf item :terminal)))))))


(defun create-program (functions terminals &key (fn-type t) (max-arity 4)
                                                (max-depth 4) (type :full))
  (cond ((equal type :full)
         (create-program-full functions terminals :fn-type fn-type
                              :max-arity max-arity :max-depth max-depth))
        ((equal type :grow)
         (create-program-grow functions terminals :fn-type fn-type
                              :max-arity max-arity :max-depth max-depth))
        (t (error "Unknown program creation process: ~S" type))))


;(defun create-program (functions terminals
;                       &key (max-depth 4) (max-function-arity 4) (type :full))
;  "TYPE can be either :FULL or :GROW."
;  (let ((fn (random-elt (get-items-of-type functions
;    (append (list (getf fn :value))
;            (loop with arity = (getf fn :arity)
;                  with fn-type = (getf fn :type)
;                  with items = (get-items-of-type
;                                (cond ((equal type :full) (if (> max-depth 0)
;                                                              functions
;                                                              terminals))
;                                      ((equal type :grow) (append functions
;                                                                  terminals))
;                                      (t (error "Unknown type: ~S" type)))
;                                fn-type)
;                  repeat (cond ((typep arity 'number) arity)
;                               ((null arity)         0)
;                               (t                    max-function-arity))
;                  collect (getf (random-elt items) :value)))))


;(defun create-population (functions terminals)
;  )


;;; Main Program

(defun main (&key (max-arity 4) (max-depth 4) (type :full))
  (let* (;(fitness-fn nil)
         (functions  '((:function + :arity t :type number)
                       (:function - :arity t :type number)
                       (:function * :arity t :type number)))
         (terminals  '((:terminal 0 :type number) (:terminal 1 :type number)
                       (:terminal 1 :type number) (:terminal 2 :type number)
                       (:terminal 3 :type number) (:terminal 4 :type number)
                       (:terminal 5 :type number) (:terminal 6 :type number)
                       (:terminal 7 :type number) (:terminal 8 :type number)
                       (:terminal 9 :type number)))
         ;(population (create-population functions terminals))
         )
    (create-program functions terminals :max-arity max-arity
                    :max-depth max-depth :type type)
    ))
