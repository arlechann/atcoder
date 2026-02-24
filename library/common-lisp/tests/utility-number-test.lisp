(in-package :cl-user)

(defpackage :utility-number-test
  (:use :cl :rove))
(in-package :utility-number-test)

(deftest arithmetic-helpers
  (ok (= 8 (utility.number:2* 4)))
  (ok (= 4 (utility.number:/2 9)))
  (ok (= 25 (utility.number:square 5)))
  (ok (= 125 (utility.number:cube 5)))
  (ok (= 64 (utility.number:cuber 4 #'*)))
  (ok (= 5 (utility.number:diff 3 8))))

(deftest pow-and-sequences
  (ok (= 1024 (utility.number:pow 2 10)))
  (ok (= 32 (utility.number:pow 2 5 :op #'* :identity 1)))
  (ok (= 15 (utility.number:triangular-number 5)))
  (ok (= 8 (utility.number:next-pow2 5)))
  (ok (= 8 (utility.number:next-pow2 8)))
  (ok (= 0 (utility.number:next-pow2 0)))
  (ok (= 11111 (utility.number:repunit 5)))
  (ok (= #b1111 (utility.number:repunit 4 2))))

(deftest modular-arithmetic-helpers
  (multiple-value-bind (g x y) (utility.number:exgcd 30 18)
    (ok (typep g 'unsigned-byte))
    (ok (= 6 g))
    (ok (= g (+ (* 30 x) (* 18 y))))))

(deftest predicate-and-update-macros
  (ok (utility.number:maxp 10 1 2 3))
  (ok (not (utility.number:maxp 3 1 2 3)))
  (ok (utility.number:minp -1 3 2 0))
  (ok (not (utility.number:minp 0 3 2 0)))
  (let ((x 5))
    (utility.number:maxf x 10 3)
    (ok (= 10 x)))
  (let ((x 5))
    (utility.number:minf x 10 3)
    (ok (= 3 x))))

(deftest bit-helpers
  (ok (= #b10101 (utility.number:logipop 0 0 2 4)))
  (ok (= 0 (utility.number:logmsb 1)))
  (ok (= 3 (utility.number:logmsb 8)))
  (ok (= 3 (utility.number:logmsb 10)))
  (ok (= -1 (utility.number:logmsb 0))))

(deftest approx-helpers
  (ok (utility.number:approx= 1d0 (+ 1d0 1d-13)))
  (ok (not (utility.number:approx= 1d0 (+ 1d0 1d-6) :eps 1d-12)))
  (ok (utility.number:approx-zero-p 1d-13))
  (ok (not (utility.number:approx-zero-p 1d-6 :eps 1d-12)))
  (ok (utility.number:approx<= 1d0 (+ 1d0 1d-13)))
  (ok (utility.number:approx>= 1d0 (- 1d0 1d-13))))

(deftest range-intersection
  (ok (utility.number:range-intersect-p 1 3 3 5 :touchp t))
  (ok (not (utility.number:range-intersect-p 1 3 3 5 :touchp nil)))
  (ok (utility.number:range-intersect-p 5 1 4 2 :touchp t))
  (ok (not (utility.number:range-intersect-p 1 2 3 4 :touchp t))))
