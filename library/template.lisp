(in-package :cl-user)

;;;
;;; utility
;;;
(defpackage :utility
  (:use :cl)
  (:export :nlet
           :aif
           :it
           :dvector
           :delay
           :force
           :symbol-intern
           ))
(in-package :utility)

(defmacro nlet (name binds &body body)
  `(labels ((,name ,(mapcar #'car binds) ,@body))
     (,name ,@(mapcar #'cadr binds))))

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defun dvector (&rest contents)
  (make-array (length contents)
              :initial-contents contents
              :adjustable t
              :fill-pointer t))

(defstruct promise (value nil) thunk)

(defmacro delay (expr)
  `(make-promise :thunk (lambda () ,expr)))

(defun force (ps)
  (when (promise-thunk ps)
    (setf (promise-value ps) (funcall (promise-thunk ps))
          (promise-thunk ps) nil))
  (promise-value ps))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symbol-intern (sym &optional (package *package*))
    (values (intern (symbol-name sym) package))))

;;;
;;; input
;;;
(defpackage :input
  (:use :cl
        :utility)
  (:export :input*
           :def-input-reader))
(in-package :input)

; (input* ((n fixnum)
;          (v (vector (list fixnum1 fixnum1 n))))
;   (list n v))
; 3
; 1 2
; 3 4
; 5 6
; => (3 #((0 1) (2 3) (4 5)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *input-reader-table* (make-hash-table :test #'eq))

  (defun set-input-reader (marker reader)
    (setf (gethash marker *input-reader-table*) reader))

  (defun get-input-reader (marker)
    (gethash marker *input-reader-table*))

  (defun input-typespec-marker (typespec)
    (symbol-intern (if (listp typespec)
                       (car typespec)
                       typespec)
                   :input))

  (defun input-typespec-reader (typespec)
    (format t "typespec: ~S~%" typespec)
    (let ((marker (input-typespec-marker typespec)))
      (if (null marker)
          nil
          (funcall (get-input-reader marker) typespec))))

  (defun input-expand (forms)
    (if (null forms)
        nil
        (mapcar (lambda (form)
                  (list (car form)
                        (input-typespec-reader (cadr form))))
                forms)))
  )


(defmacro input* (forms &body body)
  `(let* ,(input-expand forms) ,@body))

(defmacro def-input-reader (marker params &body body)
  `(progn (set-input-reader
           ',marker
           (macrolet ((reader (typespec)
                        `(input-typespec-reader ,typespec)))
             (lambda (,@params &optional arg)
               (declare (ignore arg))
               ,@body)))
          ',marker))

(eval-when (:compile-toplevel :execute)
  (def-input-reader fixnum ()
    '(read))

  (def-input-reader fixnum1 ()
    '(1- (read)))

  (def-input-reader double ()
    '(let ((*read-default-float-format* 'double-float))
      (read)))

  (def-input-reader list (typespec)
    (let ((elems (cdr typespec)))
      (if (null elems)
          nil
          `(cons ,(reader (car elems))
                 ,(reader (cons 'list (cdr elems)))))))

  (def-input-reader vector (typespec)
    (let ((vec (gensym "VEC"))
          (index (gensym "INDEX"))
          (elem (cadr typespec))
          (len (caddr typespec)))
      `(let ((,vec (make-array ,len)))
         (dotimes (,index ,len ,vec)
           (setf (aref ,vec ,index) ,(reader elem))))))
  )

;;;
;;; algorithm
;;;
(defpackage :algorithm
  (:nicknames :algo)
  (:use :cl)
  (:export :meguru-method
           ))

(defun meguru-method (ok ng pred)
  (if (<= (abs (- ok ng)) 1)
      ok
      (let ((mid (floor (+ ok ng) 2)))
        (if (funcall pred mid)
            (meguru-method mid ng pred)
            (meguru-method ok mid pred)))))

;;;
;;; atcoder
;;;
(defpackage :atcoder
  (:nicknames :ac)
  (:use :cl
        :utility
        :input))
(in-package :atcoder)

(defun main ()
  (input* ((n (list fixnum fixnum)))
    (format t "~A~%" n)))

(main)
