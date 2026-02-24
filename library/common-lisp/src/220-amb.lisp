;;; amb
;;;
(defpackage amb
  (:use :cl)
  (:export :*failed*
           :amb-reset
           :amb
           :amb-bind
           ))
(in-package :amb)

(defvar *stack* nil)
(defvar *failed* nil)

(defun amb-reset () (setf *stack* nil))

(defun fail ()
  (if (null *stack*)
      *failed*
      (funcall (pop *stack*))))

(defmacro amb (&rest options)
  (if (null options)
      `(fail)
      `(progn ,@(mapcar #'(lambda (option)
                            `(push #'(lambda () ,option) *stack*))
                        (reverse (cdr options)))
              ,(car options))))

(defmacro amb-bind (var options &body body)
  `(amb-bind-fn (lambda (,var) ,@body) ,options))

(defun amb-bind-fn (cont options)
  #-sbcl (error "Unsupported implementation.")
  (when (sb-sequence:emptyp options)
    (return-from amb-bind-fn (fail)))
  (sb-sequence:with-sequence-iterator (iterator limit from-end-p step endp element set-element index copy)
      (options)
    (declare (ignore set-element index copy))
    (labels ((rec (iterator)
               (if (not (funcall endp options iterator limit from-end-p))
                   (progn (push #'(lambda ()
                                    (rec (funcall step options iterator from-end-p)))
                                *stack*)
                          (funcall cont (funcall element options iterator)))
                   (fail))))
      (rec iterator))))

;;;
