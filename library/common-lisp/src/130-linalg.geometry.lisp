;;; linalg.geometry
;;;
(defpackage linalg.geometry
  (:use :cl :utility :linalg.vector :linalg.matrix)
  (:export :radian-to-degree
           :degree-to-radian
           :vector2-squared-distance
           :vector2-distance
           :manhattan-distance
           :vector2-upper-half-p
           :vector2-argument<
           :vector2-cross
           :collinear-p
           :vector2-rotate90
           :vector2-rotate
           ))
(in-package linalg.geometry)

;;; angle

(defun radian-to-degree (radian) (/ (* radian 180) PI))
(defun degree-to-radian (degree) (/ (* degree PI) 180))

(defun vector2-squared-distance (v1 v2)
  (let ((dx (- (vector-x v1) (vector-x v2)))
        (dy (- (vector-y v1) (vector-y v2))))
    (+ (square dx) (square dy))))

(defun vector2-distance (v1 v2)
  (sqrt (vector2-squared-distance v1 v2)))

(defun manhattan-distance (v1 v2)
  (+ (diff (vector-x v1) (vector-x v2))
     (diff (vector-y v1) (vector-y v2))))

(defun vector2-upper-half-p (vector)
  (let ((x (vector-x vector))
        (y (vector-y vector)))
    (or (> y 0)
        (and (zerop y)
             (>= x 0)))))

(defun vector2-cross (v1 v2)
  (vector-cross v1 v2))

(defun vector2-argument< (v1 v2)
  (let ((u1 (vector2-upper-half-p v1))
        (u2 (vector2-upper-half-p v2)))
    (cond ((and u1 (not u2)) t)
          ((and (not u1) u2) nil)
          (t (> (vector-cross v1 v2) 0)))))

(defun collinear-p (v1 v2)
  (zerop (vector2-cross v1 v2)))

;;; matrix

(defun make-rotation90-matrix ()
  (matrix '(0 -1)
          '(1 0)))

(defun make-rotation-matrix (radian)
  (let* ((sin (sin radian))
         (cos (cos radian)))
    (matrix (list cos (- sin))
            (list sin cos))))

(defun vector2-rotate90 (vector)
  "Rotate vector counterclockwise in right-handed coordinates system."
  (linear-map (make-rotation90-matrix) vector))

(defun vector2-rotate (vector angle)
  "Rotate vector counterclockwise on right-handed coordinates system."
  (linear-map (make-rotation-matrix angle) vector))

;;;
