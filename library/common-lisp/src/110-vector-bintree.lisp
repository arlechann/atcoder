;;; vector-bintree
;;;
(defpackage vector-bintree
  (:use :cl :utility)
  (:export :make-vector-bintree
           :make-extensible-vector-bintree
           :bintree-size
           :bintree-capacity
           :bintree-ref
           :bintree-push
           :bintree-pop
           :bintree-print
           :bintree-root-index-p
           :bintree-left-index
           :bintree-right-index
           :bintree-parent-index
           ))
(in-package vector-bintree)

(defun make-vector-bintree (size &rest args &key element-type initial-element initial-contents)
  (declare (ignore element-type initial-element initial-contents))
  (apply #'make-array size args))

(defun make-extensible-vector-bintree (size &rest args &key element-type initial-element initial-contents)
  (declare (ignore element-type initial-element))
  (let ((default-fill-pointer (if initial-contents
                                  (length initial-contents)
                                  0)))
    (apply #'make-array
           size
           :adjustable t
           :fill-pointer default-fill-pointer
           args)))

(defun bintree-size (bt)
  (if (array-has-fill-pointer-p bt) (fill-pointer bt) (length bt)))

(defun bintree-capacity (bt)
  (array-total-size bt))

(defun bintree-ref (bt index) (aref bt index))
(defun (setf bintree-ref) (value bt index) (setf (aref bt index) value))

(defun bintree-push (value bintree)
  #-atcoder
  (unless (array-has-fill-pointer-p bintree)
    (error "BINTREE-PUSH requires an array with fill-pointer."))
  (vector-push-extend value bintree))

(defun bintree-pop (bintree)
  #-atcoder
  (unless (array-has-fill-pointer-p bintree)
    (error "BINTREE-POP requires an array with fill-pointer."))
  (vector-pop bintree))

(defun bintree-root-index-p (index) (zerop index))
(defun bintree-left-index (index) (1+ (* index 2)))
(defun bintree-right-index (index) (+ 2 (* index 2)))
(defun bintree-parent-index (index) (values (floor (1- index) 2)))

(defun %bintree-print (bt index level)
  (let ((size (bintree-size bt))
        (left-index (bintree-left-index index))
        (right-index (bintree-right-index index)))
    (when (>= index size)
      (return-from %bintree-print))
    (%bintree-print bt right-index (1+ level))
    (dotimes (i level)
      (format t "    "))
    (format t "~A~%" (bintree-ref bt index))
    (%bintree-print bt left-index (1+ level))))

(defun bintree-print (bt) (%bintree-print bt 0 0))

;;;
