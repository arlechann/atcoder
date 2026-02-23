(in-package :cl-user)

(defpackage :linalg-vector-test
  (:use :cl :rove))
(in-package :linalg-vector-test)

(defun vec (&rest xs)
  (apply #'linalg.vector:vector xs))

(defun vec->list (v)
  (loop for i from 0 below (linalg.vector:vector-dimension v)
        collect (linalg.vector:vector-ref v i)))

(deftest linalg-vector-basic-access
  (let ((v (linalg.vector:make-vector 4 :initial-element 0)))
    (ok (= 4 (linalg.vector:vector-dimension v)))
    (setf (linalg.vector:vector-x v) 1
          (linalg.vector:vector-y v) 2
          (linalg.vector:vector-z v) 3
          (linalg.vector:vector-w v) 4)
    (ok (equal '(1 2 3 4) (vec->list v)))
    (ok (linalg.vector:vector-dimension= v (vec 9 8 7 6)))
    (ok (not (linalg.vector:vector-dimension= v (vec 1 2 3))))))

(deftest linalg-vector-arithmetic
  (let ((a (vec 1 2 3))
        (b (vec 4 5 6)))
    (ok (equal '(5 7 9) (vec->list (linalg.vector:vector+ a b))))
    (ok (equal '(-3 -3 -3) (vec->list (linalg.vector:vector- a b))))
    (ok (equal '(-1 -2 -3) (vec->list (linalg.vector:vector- a))))
    (ok (equal '(2 4 6) (vec->list (linalg.vector:vector* a 2))))
    (ok (equal '(1 2 3) (vec->list (linalg.vector:vector/ (vec 2 4 6) 2))))
    (ok (= 32 (linalg.vector:vector-dot a b)))))

(deftest linalg-vector-inplace-ops
  (let ((v (vec 1 2 3)))
    (linalg.vector:vector-add! v (vec 4 5 6))
    (ok (equal '(5 7 9) (vec->list v)))
    (linalg.vector:vector-sub! v (vec 1 2 3))
    (ok (equal '(4 5 6) (vec->list v)))
    (linalg.vector:vector-scale! v 2 3)
    (ok (equal '(24 30 36) (vec->list v)))))

(deftest linalg-vector-geometry
  (let ((v2a (vec 1 0))
        (v2b (vec 0 1))
        (v3a (vec 1 0 0))
        (v3b (vec 0 1 0)))
    (ok (= 1 (linalg.vector:vector-cross v2a v2b)))
    (ok (equal '(0 0 1) (vec->list (linalg.vector:vector-cross v3a v3b))))
    (ok (= 25 (linalg.vector:vector-squared-length (vec 3 4))))
    (ok (utility.number:approx= 5d0 (linalg.vector:vector-length (vec 3 4))))
    (ok (equal '(0.6d0 0.8d0)
               (vec->list (linalg.vector:vector-normalize (vec 3d0 4d0)))))
    (ok (utility.number:approx= (/ pi 2) (linalg.vector:vector-atan (vec 0 1))
                                :eps 1e-6))
    (ok (utility.number:approx= (/ pi 2) (linalg.vector:vector-angle v2a v2b)
                                :eps 1e-6))))

(deftest linalg-vector-zero-and-errors
  (ok (linalg.vector:zero-vector-p (linalg.vector:make-zero-vector 3)))
  (ok (not (linalg.vector:zero-vector-p (vec 0 0 1))))
  (ok (handler-case (progn (linalg.vector:vector+ (vec 1 2) (vec 1 2 3)) nil)
        (error () t)))
  (ok (handler-case (progn (linalg.vector:vector-dot (vec 1 2) (vec 1 2 3)) nil)
        (error () t)))
  (ok (handler-case (progn (linalg.vector:vector-normalize (vec 0 0)) nil)
        (error () t)))
  (ok (handler-case (progn (linalg.vector:vector-angle (vec 1 0) (vec 0 0)) nil)
        (error () t))))
