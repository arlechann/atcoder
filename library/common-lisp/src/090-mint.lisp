;;; mint
;;;
(defpackage :mint
  (:use :cl :utility.number :math)
  (:export :*mint-modulus*
           :with-mint-modulus
           :mint
           :mint-p
           :make-mint
           :to-mint
           :mint-value
           :mint+
           :mint-
           :mint*
           :mint/
           :mint-inv
           :mint-pow
           :make-mint-combinatorics
           :mint-fact
           :mint-ifact
           :mint-nck
           :mint-npk))
(in-package :mint)

(defparameter *mint-modulus* 998244353)

(defstruct (mint
            (:constructor %make-mint (value)))
  (value 0 :type fixnum))

(declaim (inline normalize-mint-value))
(defun normalize-mod (value modulus)
  #-atcoder
  (unless (and (integerp modulus) (> modulus 0))
    (error "NORMALIZE-MOD: modulus must be a positive integer, got ~S." modulus))
  (mod value modulus))

(defun normalize-mint-value (x)
  (the fixnum (normalize-mod x *mint-modulus*)))

(defun mod-inv (value modulus)
  (multiple-value-bind (g x _y)
      (exgcd (normalize-mod value modulus) modulus)
    (declare (ignore _y))
    (when (= (abs g) 1)
      (normalize-mod x modulus))))

(defmethod to-integer ((x mint))
  (mint-value x))

(defgeneric to-mint (x)
  (:documentation "Convert X to mint."))

(defmethod to-mint ((x mint))
  x)

(defmethod to-mint ((x integer))
  (%make-mint (normalize-mint-value x)))

(defmethod to-mint ((x rational))
  (let* ((num (numerator x))
         (den (denominator x))
         (inv (mod-inv den *mint-modulus*)))
    #-atcoder
    (unless inv
      (error "TO-MINT: denominator ~S has no inverse under modulus ~S." den *mint-modulus*))
    (make-mint (* num (or inv 0)))))

(defun make-mint (x)
  (%make-mint (normalize-mint-value (to-integer x))))

(defun mint+ (&rest xs)
  (make-mint (reduce #'+ xs :initial-value 0 :key #'to-integer)))

(defun mint- (x &rest more)
  (if (null more)
      (make-mint (- (to-integer x)))
      (make-mint (reduce #'- more :initial-value (to-integer x) :key #'to-integer))))

(defun mint* (&rest xs)
  (make-mint (reduce #'* xs :initial-value 1 :key #'to-integer)))

(defun mint-inv (x)
  (let* ((a (normalize-mint-value (to-integer x)))
         (m *mint-modulus*)
         (inv (mod-inv a m)))
      #-atcoder
      (unless inv
        (error "MINT-INV: inverse does not exist for ~S under modulus ~S." a m))
      (make-mint (or inv 0))))

(defun mint/ (x &rest ys)
  (reduce (lambda (acc y) (mint* acc (mint-inv y)))
          ys
          :initial-value (to-mint x)))

(defun mint-pow (x n)
  #-atcoder
  (unless (and (integerp n) (<= 0 n))
    (error "MINT-POW: exponent must be a non-negative integer, got ~S." n))
  (pow (to-mint x) n :op #'mint* :identity (make-mint 1)))

(defun make-mint-combinatorics (max-n)
  (make-combinatorics-table
   max-n
   :one (make-mint 1)
   :zero (make-mint 0)
   :mul #'mint*
   :inv #'mint-inv
   :from-integer #'make-mint))

(defun mint-fact (table n)
  (combinatorics-fact table n))

(defun mint-ifact (table n)
  (combinatorics-ifact table n))

(defun mint-nck (table n k)
  (combinatorics-nck table n k))

(defun mint-npk (table n k)
  (combinatorics-npk table n k))

(defmacro with-mint-modulus ((modulus) &body body)
  `(let ((*mint-modulus* ,modulus))
     #-atcoder
     (unless (and (typep *mint-modulus* 'fixnum) (> *mint-modulus* 0))
       (error "WITH-MINT-MODULUS: modulus must be a positive fixnum, got ~S." *mint-modulus*))
     ,@body))

;;;
