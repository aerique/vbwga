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
                  for items = (get-items-of-type (if (> max-depth 0)
                                                     functions
                                                     terminals)
                                                 item-type)
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
                             (+ (random max-arity) 1)
                             (length args))
                  for i from 0
                  for item-type = (cond ((equal (elt args 0) '&rest)
                                         (elt args 1))
                                        (t (elt args i)))
                  for items = (get-items-of-type (if (> max-depth 0)
                                                   (append functions terminals)
                                                   terminals)
                                                 item-type)
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


;;; Main Program

(defun main (&key (max-arity 4) (max-depth 4) (size 16)
                  (type :ramped-half-half))
  (let* ((functions  '((:function + :type number :args (&rest number))
                       (:function - :type number :args (&rest number))
                       (:function * :type number :args (&rest number))))
         (terminals  '((:terminal 0 :type number) (:terminal 1 :type number)
                       (:terminal 1 :type number) (:terminal 2 :type number)
                       (:terminal 3 :type number) (:terminal 4 :type number)
                       (:terminal 5 :type number) (:terminal 6 :type number)
                       (:terminal 7 :type number) (:terminal 8 :type number)
                       (:terminal 9 :type number))))
    (create-population functions terminals :max-arity max-arity
                       :max-depth max-depth :size size :type type)))
