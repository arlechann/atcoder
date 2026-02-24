;;; linalg.vector
;;;
(defpackage linalg.vector
  (:use :cl :utility)
  (:export :make-vector
           :vector
           :copy-vector
           :vector-ref
           :vector-x
           :vector-y
           :vector-z
           :vector-w
           :vector-dimension
           :vector-dimension=
           :make-zero-vector
           :vector+
           :vector-
           :vector*
           :vector/
           :vector-add!
           :vector-sub!
           :vector-scale!
           :vector-dot
           :vector-cross
           :vector-squared-length
           :vector-length
           :vector-normalize
           :vector-atan
           :vector-angle
           :zero-vector-p
           ))
(in-package linalg.vector)

(defun make-vector (dimension &rest args &key element-type initial-element initial-contents)
  (declare (ignore element-type initial-element initial-contents))
  (apply #'make-array dimension args))

(defun copy-vector (vector) (copy-seq vector))
(defun vector-ref (vector dimension) (aref vector dimension))
(defun (setf vector-ref) (value vector dimension) (setf (aref vector dimension) value))

(macrolet ((%def-vector-ref (char index)
             `(defun ,(symb 'vector- char) (vector) (vector-ref vector ,index)))
           (%def-setf-vector-ref (char index)
             `(defun (setf ,(symb 'vector- char)) (value vector)
                (setf (vector-ref vector ,index) value))))
  (%def-vector-ref x 0)
  (%def-vector-ref y 1)
  (%def-vector-ref z 2)
  (%def-vector-ref w 3)
  (%def-setf-vector-ref x 0)
  (%def-setf-vector-ref y 1)
  (%def-setf-vector-ref z 2)
  (%def-setf-vector-ref w 3))

(defun vector-dimension (vector) (array-dimension vector 0))
(defun vector-dimension= (&rest vectors)
  (or (null vectors)
      (null (cdr vectors))
      (let ((dimension (vector-dimension (car vectors))))
        (every (lambda (vector)
                 (= dimension (vector-dimension vector)))
               (cdr vectors)))))
(defun make-zero-vector (dimension) (make-vector dimension :initial-element 0))

(defun assert-same-dimension (v1 v2 op-name)
  (let ((d1 (vector-dimension v1))
        (d2 (vector-dimension v2)))
    #-atcoder
    (unless (= d1 d2)
      (error "~A: dimension mismatch (~S vs ~S)." op-name d1 d2))
    d1))

(defun zero-vector-p (vector &key (eps 1d-12))
  (dotimes (i (vector-dimension vector) t)
    (unless (approx= (vector-ref vector i) 0 :eps eps)
      (return-from zero-vector-p nil))))

(defun vector+ (vector &rest more-vectors)
  (let ((dimension (vector-dimension vector))
        (ret (copy-vector vector)))
    (dolist (vec more-vectors ret)
      (assert-same-dimension vector vec "VECTOR+")
      (dotimes (i dimension)
        (incf (vector-ref ret i) (vector-ref vec i))))))

(defun vector* (vector &rest numbers)
  (let ((dimension (vector-dimension vector))
        (ret (copy-vector vector)))
    (dolist (n numbers ret)
      (dotimes (i dimension)
        (setf (vector-ref ret i)
              (* (vector-ref ret i) n))))))

(defun vector- (vector &rest more-vectors)
  (if (null more-vectors)
      (vector* vector -1)
      (let ((dimension (vector-dimension vector))
            (ret (copy-vector vector)))
        (dolist (vec more-vectors ret)
          (assert-same-dimension vector vec "VECTOR-")
          (dotimes (i dimension)
            (decf (vector-ref ret i) (vector-ref vec i)))))))

(defun vector/ (vector &rest numbers)
  (let ((dimension (vector-dimension vector))
        (ret (copy-vector vector)))
    (dolist (n numbers ret)
      (dotimes (i dimension)
        (setf (vector-ref ret i)
              (/ (vector-ref ret i) n))))))

(defun vector-add! (vector &rest more-vectors)
  (let ((dimension (vector-dimension vector)))
    (dolist (vec more-vectors vector)
      (assert-same-dimension vector vec "VECTOR-ADD!")
      (dotimes (i dimension)
        (incf (vector-ref vector i) (vector-ref vec i))))))

(defun vector-sub! (vector &rest more-vectors)
  (let ((dimension (vector-dimension vector)))
    (dolist (vec more-vectors vector)
      (assert-same-dimension vector vec "VECTOR-SUB!")
      (dotimes (i dimension)
        (decf (vector-ref vector i) (vector-ref vec i))))))

(defun vector-scale! (vector &rest numbers)
  (let ((dimension (vector-dimension vector)))
    (dolist (n numbers vector)
      (dotimes (i dimension)
        (setf (vector-ref vector i)
              (* (vector-ref vector i) n))))))

(defun vector-dot (v1 v2)
  (let ((dimension (assert-same-dimension v1 v2 "VECTOR-DOT"))
        (ret 0))
    (dotimes (i dimension ret)
      (incf ret (* (vector-ref v1 i) (vector-ref v2 i))))))

(defun vector-cross (v1 v2)
  (let ((dimension (assert-same-dimension v1 v2 "VECTOR-CROSS")))
    (cond ((= dimension 2)
           (- (* (vector-x v1) (vector-y v2))
              (* (vector-y v1) (vector-x v2))))
          ((= dimension 3)
           (vector (- (* (vector-y v1) (vector-z v2))
                      (* (vector-z v1) (vector-y v2)))
                   (- (* (vector-z v1) (vector-x v2))
                      (* (vector-x v1) (vector-z v2)))
                   (- (* (vector-x v1) (vector-y v2))
                      (* (vector-y v1) (vector-x v2)))))
          #-atcoder
          (t (error "VECTOR-CROSS: Not implemented on the dimension.")))))

(defun vector-squared-length (vector) (vector-dot vector vector))
(defun vector-length (vector) (sqrt (vector-squared-length vector)))
(defun vector-normalize (vector)
  (let ((length (vector-length vector)))
    #-atcoder
    (when (zerop length)
      (error "VECTOR-NORMALIZE: zero vector cannot be normalized."))
    (vector/ vector length)))
(defun vector-atan (vector) (atan (vector-y vector) (vector-x vector)))

(defun vector-angle (v1 v2)
  (let ((denominator (* (vector-length v1) (vector-length v2))))
    #-atcoder
    (when (zerop denominator)
      (error "VECTOR-ANGLE: zero vector is not allowed."))
    (let ((cosine (/ (vector-dot v1 v2) denominator)))
      (acos (max -1 (min 1 cosine))))))

;;;
