;;; linalg.matrix
;;;
(defpackage linalg.matrix
  (:use :cl :utility :linalg.vector)
  (:export :make-matrix
           :matrix
           :matrixp
           :matrix-shape
           :matrix-shape=
           :matrix-ref
           :matrix-row-major-ref
           :matrix-element-type
           :matrix-row
           :matrix-column
           :matrix-element-count
           :matrix-row-vector
           :matrix-column-vector
           :make-row-vector
           :vector->row-matrix
           :vector->column-matrix
           :make-zero-matrix
           :make-identity-matrix
           :matrix+
           :matrix-
           :matrix-add!
           :matrix-sub!
           :matrix-scalar-multiply
           :matrix-scale!
           :matrix-transpose
           :matrix-product
           :matrix-vector-product
           :vector-matrix-product
           :outer-product
           :linear-map
           :matrix-trace
           :matrix-diagonal
           :matrix-frobenius-norm
           :identity-matrix-p
           :matrix-power
           :zero-matrix-p
           ))
(in-package linalg.matrix)

(defun make-matrix (row column &rest args &key element-type initial-element initial-contents)
  (declare (ignore element-type initial-element initial-contents))
  (apply #'make-array (list row column) args))
              
(defun matrix (&rest rows)
  (let ((row (length rows))
        (col (length (car rows))))
    (make-matrix row col :initial-contents rows)))

(defun matrixp (matrix) (= (array-rank matrix) 2))
(defun matrix-element-type (matrix) (array-element-type matrix))
(defun matrix-row (matrix) (array-dimension matrix 0))
(defun matrix-column (matrix) (array-dimension matrix 1))
(defun matrix-element-count (matrix) (array-total-size matrix))

(defun matrix-shape (matrix) (list (matrix-row matrix) (matrix-column matrix)))
(defun matrix-shape= (&rest matrices)
  (or (null matrices)
      (null (cdr matrices))
      (let ((row (matrix-row (car matrices)))
            (column (matrix-column (car matrices))))
        (every (lambda (matrix)
                 (and (= row (matrix-row matrix))
                      (= column (matrix-column matrix))))
               (cdr matrices)))))

(defun assert-same-shape (m1 m2 op-name)
  #-atcoder
  (unless (and (= (matrix-row m1) (matrix-row m2))
               (= (matrix-column m1) (matrix-column m2)))
    (error "~A: shape mismatch (~Sx~S) vs (~Sx~S)."
           op-name
           (matrix-row m1) (matrix-column m1)
           (matrix-row m2) (matrix-column m2))))

(defun matrix-ref (matrix y x) (aref matrix y x))
(defun matrix-row-major-ref (matrix index) (row-major-aref matrix index))
(defun (setf matrix-ref) (value matrix y x) (setf (aref matrix y x) value))

(defun (setf matrix-row-major-ref) (value matrix index)
  (setf (row-major-aref matrix index) value))

(defun make-row-vector (matrix row)
  (make-array (matrix-column matrix)
              :element-type (matrix-element-type matrix)
              :displaced-to matrix
              :displaced-index-offset (array-row-major-index matrix row 0)))

(defun matrix-row-vector (matrix row)
  (make-row-vector matrix row))

(defun matrix-column-vector (matrix column)
  (let ((row (matrix-row matrix))
        (ret (make-vector (matrix-row matrix))))
    (dotimes (i row ret)
      (setf (vector-ref ret i) (matrix-ref matrix i column)))))

(defun vector->row-matrix (vector)
  (make-matrix 1
               (vector-dimension vector)
               :initial-contents (list (coerce vector 'list))))

(defun vector->column-matrix (vector)
  (make-matrix (vector-dimension vector)
               1
               :initial-contents
               (loop for i below (vector-dimension vector)
                     collect (list (vector-ref vector i)))))

(defun make-zero-matrix (row column) (make-matrix row column :initial-element 0))

(defun make-identity-matrix (size)
  (let ((m (make-zero-matrix size size)))
    (loop for i below size
          do (setf (matrix-ref m i i) 1)
          finally (return m))))

(defun matrix-binary+ (m1 m2)
  (assert-same-shape m1 m2 "MATRIX+")
  (let* ((row (matrix-row m1))
         (col (matrix-column m1))
         (m (make-zero-matrix row col)))
    (dotimes (i (* row col) m)
      (setf (matrix-row-major-ref m i)
            (+ (matrix-row-major-ref m1 i)
               (matrix-row-major-ref m2 i))))))

(defun matrix+ (matrix &rest more-matrices)
  (reduce #'matrix-binary+
          more-matrices
          :initial-value matrix))

(defun matrix-binary- (m1 m2)
  (assert-same-shape m1 m2 "MATRIX-")
  (let* ((row (matrix-row m1))
         (col (matrix-column m1))
         (m (make-zero-matrix row col)))
    (dotimes (i (* row col) m)
      (setf (matrix-row-major-ref m i)
            (- (matrix-row-major-ref m1 i)
               (matrix-row-major-ref m2 i))))))

(defun matrix- (matrix &rest more-matrices)
  (if (null more-matrices)
      (matrix-binary- (make-zero-matrix (matrix-row matrix)
                                        (matrix-column matrix))
                      matrix)
      (reduce #'matrix-binary-
              more-matrices
              :initial-value matrix)))

(defun matrix-binary-scalar-multiply (matrix scalar)
  (let* ((row (matrix-row matrix))
         (col (matrix-column matrix))
         (m (make-zero-matrix row col)))
    (dotimes (i (* row col) m)
      (setf (matrix-row-major-ref m i)
            (* (matrix-row-major-ref matrix i) scalar)))))

(defun matrix-scalar-multiply (matrix &rest scalars)
  (reduce #'matrix-binary-scalar-multiply
          scalars
          :initial-value matrix))

(defun matrix-add! (matrix &rest more-matrices)
  (let* ((row (matrix-row matrix))
         (col (matrix-column matrix))
         (size (* row col)))
    (dolist (m more-matrices matrix)
      (assert-same-shape matrix m "MATRIX-ADD!")
      (dotimes (i size)
        (incf (matrix-row-major-ref matrix i)
              (matrix-row-major-ref m i))))))

(defun matrix-sub! (matrix &rest more-matrices)
  (let* ((row (matrix-row matrix))
         (col (matrix-column matrix))
         (size (* row col)))
    (dolist (m more-matrices matrix)
      (assert-same-shape matrix m "MATRIX-SUB!")
      (dotimes (i size)
        (decf (matrix-row-major-ref matrix i)
              (matrix-row-major-ref m i))))))

(defun matrix-scale! (matrix &rest scalars)
  (let* ((row (matrix-row matrix))
         (col (matrix-column matrix))
         (size (* row col)))
    (dolist (scalar scalars matrix)
      (dotimes (i size)
        (setf (matrix-row-major-ref matrix i)
              (* (matrix-row-major-ref matrix i) scalar))))))

(defun matrix-transpose (matrix)
  (let* ((row (matrix-row matrix))
         (col (matrix-column matrix))
         (ret (make-matrix col row)))
    (dotimes (i row ret)
      (dotimes (j col)
        (setf (matrix-ref ret j i) (matrix-ref matrix i j))))))

(defun matrix-binary-product (m1 m2)
  #-atcoder
  (unless (= (matrix-column m1) (matrix-row m2))
    (error "MATRIX-PRODUCT: dimension mismatch (~Sx~S) * (~Sx~S)."
           (matrix-row m1) (matrix-column m1) (matrix-row m2) (matrix-column m2)))
  (let* ((row (matrix-row m1))
         (col (matrix-column m2))
         (m (make-zero-matrix row col)))
    (dotimes (i row m)
      (dotimes (j col)
        (dotimes (k (matrix-column m1))
          (incf (matrix-ref m i j)
                (* (matrix-ref m1 i k)
                   (matrix-ref m2 k j))))))))

(defun matrix-product (matrix &rest more-matrices)
  (reduce #'matrix-binary-product
          more-matrices
          :initial-value matrix))

(defun linear-map (matrix vector)
  #-atcoder
  (unless (= (matrix-column matrix) (vector-dimension vector))
    (error "LINEAR-MAP: dimension mismatch matrix-column=~S vector-dimension=~S."
           (matrix-column matrix) (vector-dimension vector)))
  (let* ((row (matrix-row matrix))
         (v (make-zero-vector row)))
    (dotimes (i row v)
      (setf (vector-ref v i)
            (vector-dot vector (make-row-vector matrix i))))))

(defun matrix-vector-product (matrix vector)
  (linear-map matrix vector))

(defun vector-matrix-product (vector matrix)
  #-atcoder
  (unless (= (vector-dimension vector) (matrix-row matrix))
    (error "VECTOR-MATRIX-PRODUCT: dimension mismatch vector=~S matrix-row=~S."
           (vector-dimension vector) (matrix-row matrix)))
  (let* ((col (matrix-column matrix))
         (ret (make-zero-vector col)))
    (dotimes (j col ret)
      (let ((acc 0))
        (dotimes (i (matrix-row matrix))
          (incf acc (* (vector-ref vector i)
                       (matrix-ref matrix i j))))
        (setf (vector-ref ret j) acc)))))

(defun outer-product (v1 v2)
  (let* ((row (vector-dimension v1))
         (col (vector-dimension v2))
         (ret (make-zero-matrix row col)))
    (dotimes (i row ret)
      (dotimes (j col)
        (setf (matrix-ref ret i j)
              (* (vector-ref v1 i) (vector-ref v2 j)))))))

(defun matrix-trace (matrix)
  #-atcoder
  (unless (= (matrix-row matrix) (matrix-column matrix))
    (error "MATRIX-TRACE: matrix must be square, got (~Sx~S)."
           (matrix-row matrix) (matrix-column matrix)))
  (let ((acc 0))
    (dotimes (i (matrix-row matrix) acc)
      (incf acc (matrix-ref matrix i i)))))

(defun matrix-diagonal (matrix)
  (let* ((n (min (matrix-row matrix) (matrix-column matrix)))
         (ret (make-vector n)))
    (dotimes (i n ret)
      (setf (vector-ref ret i) (matrix-ref matrix i i)))))

(defun (setf matrix-diagonal) (value matrix)
  (let ((n (min (matrix-row matrix) (matrix-column matrix))))
    #-atcoder
    (unless (= (vector-dimension value) n)
      (error "(SETF MATRIX-DIAGONAL): dimension mismatch vector=~S diagonal-size=~S."
             (vector-dimension value) n))
    (dotimes (i n value)
      (setf (matrix-ref matrix i i) (vector-ref value i)))))

(defun matrix-frobenius-norm (matrix)
  (sqrt
   (let ((acc 0))
     (dotimes (i (matrix-element-count matrix) acc)
       (incf acc (square (matrix-row-major-ref matrix i)))))))

(defun identity-matrix-p (matrix &key (eps 1d-12))
  (let ((row (matrix-row matrix))
        (col (matrix-column matrix)))
    (and (= row col)
         (dotimes (i row t)
           (dotimes (j col)
             (unless (approx= (matrix-ref matrix i j)
                              (if (= i j) 1 0)
                              :eps eps)
               (return-from identity-matrix-p nil)))))))

(defun zero-matrix-p (matrix &key (eps 1d-12))
  (dotimes (i (matrix-element-count matrix) t)
    (unless (approx= (matrix-row-major-ref matrix i) 0 :eps eps)
      (return-from zero-matrix-p nil))))

(defun matrix-power (matrix exponent)
  #-atcoder
  (unless (and (integerp exponent) (<= 0 exponent))
    (error "MATRIX-POWER: exponent must be a non-negative integer, got ~S." exponent))
  #-atcoder
  (unless (= (matrix-row matrix) (matrix-column matrix))
    (error "MATRIX-POWER: matrix must be square, got (~Sx~S)."
           (matrix-row matrix) (matrix-column matrix)))
  (pow matrix
       exponent
       :op #'matrix-binary-product
       :identity (make-identity-matrix (matrix-row matrix))))

;;;
