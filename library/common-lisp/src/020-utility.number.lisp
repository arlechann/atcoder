;;; utility.number
;;;
(defpackage utility.number
  (:use :cl :utility.syntax)
  (:export ;; number
           :2*
           :/2
           :square
           :cube
           :cuber
           :pow
           :to-integer
           :exgcd
           :diff
           :triangular-number
           :next-pow2
           :repunit
           :maxp
           :minp
           :maxf
           :minf
           :logipop
           :logmsb
           :approx=
           :approx-zero-p
           :approx<=
           :approx>=
           :range-intersect-p
           ))
(in-package :utility.number)

(declaim (inline onep 2* /2 square cube cuber
                 diff triangular-number repunit maxp minp logmsb
                 approx= approx-zero-p approx<= approx>=))

(declaim (ftype (function (number) t) onep))
(defun onep (x) (= x 1))
(declaim (ftype (function (number) number) 2*))
(defun 2* (x) (* x 2))
(declaim (ftype (function (real) integer) /2))
(defun /2 (x) (values (floor x 2)))

(declaim (ftype (function (t &optional (function (t t) t)) t) square))
(defun square (x &optional (op #'*)) (funcall op x x))
(declaim (ftype (function (t &optional (function (t t) t)) t) cube))
(defun cube (x &optional (op #'*)) (funcall op (funcall op x x) x))
(declaim (ftype (function (t (function (t t) t)) t) cuber))
(defun cuber (x op) (funcall op x (funcall op x x)))

(defgeneric to-integer (x)
  (:documentation "Convert X to an integer value."))

(defmethod to-integer ((x integer))
  x)

(declaim (ftype (function (t unsigned-byte &key (:op (function (t t) t)) (:identity t)) t) pow))
(defun pow (base power &key (op #'*) (identity 1))
  (nlet rec ((base base) (power power) (ret identity))
    (cond ((zerop power) ret)
          ((oddp power)
           (rec (square base op) (floor power 2) (funcall op ret base)))
          (t (rec (square base op) (floor power 2) ret)))))

(declaim (ftype (function (unsigned-byte unsigned-byte)
                          (values unsigned-byte integer integer))
                exgcd))
(defun exgcd (a b)
  (declare (type unsigned-byte a b))
  (labels ((rec (x y)
             (if (zerop y)
                 (values x 1 0)
                 (multiple-value-bind (g s u) (rec y (mod x y))
                   (values g
                           u
                           (- s (* (floor x y) u)))))))
    (rec a b)))

(declaim (ftype (function (number number) (real 0 *)) diff))
(defun diff (a b) (abs (- a b)))

(declaim (ftype (function (unsigned-byte) unsigned-byte) triangular-number))
(defun triangular-number (n)
  (values (floor (* n (1+ n)) 2)))

(declaim (ftype (function (unsigned-byte) unsigned-byte) next-pow2))
(defun next-pow2 (n)
  (declare (type unsigned-byte n))
  (if (zerop (logand n (1- n))) n
      (nlet rec ((n n) (acc 1))
        (if (zerop n)
            acc
            (rec (ash n -1) (ash acc 1))))))

(declaim (ftype (function (unsigned-byte &optional unsigned-byte) unsigned-byte) repunit))
(defun repunit (n &optional (base 10))
  (declare (type unsigned-byte n base))
  (nlet rec ((n n) (acc 0))
    (if (zerop n) acc
        (rec (1- n) (+ (* base acc) 1)))))

(declaim (ftype (function (real &rest real) boolean) maxp))
(defun maxp (x &rest args)
  (or (null args)
      (> x (apply #'max args))))
(declaim (ftype (function (real &rest real) boolean) minp))
(defun minp (x &rest args)
  (or (null args)
      (< x (apply #'min args))))
(defmacro maxf (place &rest args) `(setf ,place (max ,place ,@args)))
(defmacro minf (place &rest args) `(setf ,place (min ,place ,@args)))

(declaim (ftype (function (unsigned-byte &rest unsigned-byte) unsigned-byte) logipop))
(defun logipop (n &rest indexes)
  (declare (type unsigned-byte n))
  (dolist (index indexes n)
    (declare (type unsigned-byte index))
    (setf n (logior n (ash 1 index)))))

(declaim (ftype (function (unsigned-byte) (integer -1 *)) logmsb))
(defun logmsb (n)
  (nlet rec ((i 0) (k 1))
    (cond ((= k n) i)
          ((> k n) (1- i))
          (t (rec (1+ i) (ash k 1))))))

(declaim (ftype (function (real real &key (:eps real)) boolean) approx=))
(defun approx= (x y &key (eps 1d-12))
  (<= (abs (- x y)) eps))

(declaim (ftype (function (real &key (:eps real)) boolean) approx-zero-p))
(defun approx-zero-p (x &key (eps 1d-12))
  (<= (abs x) eps))

(declaim (ftype (function (real real &key (:eps real)) boolean) approx<=))
(defun approx<= (x y &key (eps 1d-12))
  (or (< x y)
      (approx= x y :eps eps)))

(declaim (ftype (function (real real &key (:eps real)) boolean) approx>=))
(defun approx>= (x y &key (eps 1d-12))
  (or (> x y)
      (approx= x y :eps eps)))

(declaim (ftype (function (real real real real &key (:touchp t)) boolean)))
(defun range-intersect-p (a1 a2 b1 b2 &key touchp)
  (let ((comp (if touchp #'< #'<=)))
    (not (or (funcall comp (max a1 a2) (min b1 b2))
             (funcall comp (max b1 b2) (min a1 a2))))))

(let* ((dp-combination-max 0)
       (memo (make-array (list dp-combination-max dp-combination-max)
                         :element-type 'unsigned-byte
                         :initial-element 0)))
  (defun set-dp-combination-max (max)
    (setf dp-combination-max max)
    (setf memo (make-array (list dp-combination-max dp-combination-max)
                           :element-type 'unsigned-byte
                           :initial-element 0))
    (proclaim `(ftype (function ((mod ,dp-combination-max) (mod ,dp-combination-max))
                                unsigned-byte)
                      dp-combination)))
  (set-dp-combination-max 2501)
  (defun dp-combination (n k)
    (cond ((< n k) 0)
          ((= n k) 1)
          ((zerop k) 1)
          (t (let ((memo-value (aref memo n k)))
               (if (> memo-value 0)
                   memo-value
                   (setf (aref memo n k)
                         (+ (dp-combination (1- n) (1- k))
                            (dp-combination (1- n) k)))))))))

;;;
