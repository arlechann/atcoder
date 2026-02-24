(in-package :cl-user)

(defpackage :math-test
  (:use :cl :rove))
(in-package :math-test)

(deftest modular-arithmetic-helpers
  (multiple-value-bind (g x y) (math:exgcd 30 18)
    (ok (typep g 'unsigned-byte))
    (ok (= 6 g))
    (ok (= g (+ (* 30 x) (* 18 y))))))

(deftest crt
  (multiple-value-bind (x m) (math:crt '(2 3 2) '(3 5 7))
    (ok (= 23 x))
    (ok (= 105 m)))
  (multiple-value-bind (x m) (math:crt '(1 3) '(4 6))
    (ok (= 9 x))
    (ok (= 12 m)))
  (multiple-value-bind (x m) (math:crt '(1 2) '(4 6))
    (ok (null x))
    (ok (null m)))
  (multiple-value-bind (x m) (math:crt '(-1 2) '(5 3))
    (ok (= 14 x))
    (ok (= 15 m))))

(deftest combinatorics-table-generic
  (let ((table (math:make-combinatorics-table
                10
                :one 1
                :zero 0
                :mul #'* 
                :inv (lambda (x) (/ 1 x)))))
    (ok (= 120 (math:combinatorics-fact table 5)))
    (ok (= 1/120 (math:combinatorics-ifact table 5)))
    (ok (= 10 (math:combinatorics-nck table 5 2)))
    (ok (= 20 (math:combinatorics-npk table 5 2)))
    (ok (= 0 (math:combinatorics-nck table 3 5)))))

(deftest pascal-nck
  (math:init-pascal-nck 20)
  (ok (= 1 (math:pascal-nck 0 0)))
  (ok (= 10 (math:pascal-nck 5 2)))
  (ok (= 184756 (math:pascal-nck 20 10)))
  (ok (= 0 (math:pascal-nck 3 5))))

(deftest pascal-nck-reinit
  (math:init-pascal-nck 30)
  (ok (= 155117520 (math:pascal-nck 30 15)))
  (math:init-pascal-nck 10)
  (ok (= 252 (math:pascal-nck 10 5)))
  (ok (handler-case
          (progn (math:pascal-nck 30 15) nil)
        (error () t)))
  (math:init-pascal-nck 25)
  (ok (= 5200300 (math:pascal-nck 25 12))))
