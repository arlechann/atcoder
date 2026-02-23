(in-package :cl-user)

(defpackage :linalg-geometry-test
  (:use :cl :rove))
(in-package :linalg-geometry-test)

(defun vec2 (x y)
  (linalg.vector:vector x y))

(defun approxv2= (v x y &key (eps 1e-6))
  (and (utility.number:approx= (linalg.vector:vector-x v) x :eps eps)
       (utility.number:approx= (linalg.vector:vector-y v) y :eps eps)))

(deftest geometry-angle-conversion
  (ok (utility.number:approx= 180d0 (linalg.geometry:radian-to-degree pi)
                              :eps 1e-9))
  (ok (utility.number:approx= pi (linalg.geometry:degree-to-radian 180d0)
                              :eps 1e-9))
  (ok (utility.number:approx=
       (/ pi 2)
       (linalg.geometry:degree-to-radian
        (linalg.geometry:radian-to-degree (/ pi 2)))
       :eps 1e-9)))

(deftest geometry-distance
  (let ((a (vec2 0 0))
        (b (vec2 3 4)))
    (ok (= 25 (linalg.geometry:vector2-squared-distance a b)))
    (ok (utility.number:approx= 5d0 (linalg.geometry:vector2-distance a b)
                                :eps 1e-9))
    (ok (= 7 (linalg.geometry:manhattan-distance a b)))))

(deftest geometry-upper-half
  (ok (linalg.geometry:vector2-upper-half-p (vec2 1 0)))
  (ok (linalg.geometry:vector2-upper-half-p (vec2 0 1)))
  (ok (not (linalg.geometry:vector2-upper-half-p (vec2 -1 0))))
  (ok (not (linalg.geometry:vector2-upper-half-p (vec2 0 -1)))))

(deftest geometry-cross-and-collinear
  (let ((x (vec2 1 0))
        (y (vec2 0 1))
        (a (vec2 2 2))
        (b (vec2 3 3)))
    (ok (= 1 (linalg.geometry:vector2-cross x y)))
    (ok (linalg.geometry:collinear-p a b))
    (ok (not (linalg.geometry:collinear-p x y)))))

(deftest geometry-argument-order
  (let ((x (vec2 1 0))
        (y (vec2 0 1))
        (nx (vec2 -1 0))
        (ny (vec2 0 -1)))
    (ok (linalg.geometry:vector2-argument< x y))
    (ok (linalg.geometry:vector2-argument< y nx))
    (ok (linalg.geometry:vector2-argument< nx ny))
    (ok (not (linalg.geometry:vector2-argument< ny x)))))

(deftest geometry-rotate
  (let ((v (vec2 1 2)))
    (ok (approxv2= (linalg.geometry:vector2-rotate90 v) -2 1))
    (ok (approxv2= (linalg.geometry:vector2-rotate (vec2 1 0) (/ pi 2))
                   0d0 1d0
                   :eps 1e-6))
    (ok (approxv2= (linalg.geometry:vector2-rotate (vec2 3 4) 0)
                   3 4))))
