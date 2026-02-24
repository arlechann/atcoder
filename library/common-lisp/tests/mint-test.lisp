(in-package :cl-user)

(defpackage :mint-test
  (:use :cl :rove))
(in-package :mint-test)

(deftest mint-basic
  (mint:with-mint-modulus (13)
    (ok (= 0 (mint:mint-value (mint:make-mint 13))))
    (ok (= 12 (mint:mint-value (mint:make-mint -1))))
    (ok (= 2 (mint:mint-value (mint:mint+ 10 5))))
    (ok (= 8 (mint:mint-value (mint:mint- 10 2))))
    (ok (= 11 (mint:mint-value (mint:mint- 2 4))))
    (ok (= 11 (mint:mint-value (mint:mint* 8 3))))
    (ok (= 5 (mint:mint-value (mint:to-mint 2/3))))
    (ok (= 8 (mint:mint-value (mint:to-mint -2/3))))
    (ok (= 3 (mint:mint-value (mint:mint-pow 3 4))))))

(deftest mint-inv-div
  (mint:with-mint-modulus (998244353)
    (ok (= 1 (mint:mint-value (mint:mint* 3 (mint:mint-inv 3)))))
    (ok (= 1 (mint:mint-value (mint:mint* -3 (mint:mint-inv -3)))))
    (ok (= 5 (mint:mint-value (mint:mint/ 10 2))))
    (ok (= 2 (mint:mint-value (mint:mint/ (mint:make-mint 10) (mint:make-mint 5)))))))

(deftest mint-div-non-coprime-denominator
  (mint:with-mint-modulus (12)
    (ok (handler-case
            (progn (mint:mint/ 5 6) nil)
          (error () t)))))

(deftest mint-combinatorics
  (mint:with-mint-modulus (998244353)
    (let ((table (mint:make-mint-combinatorics 10)))
      (ok (= 120 (mint:mint-value (mint:mint-fact table 5))))
      (ok (= 856826403 (mint:mint-value (mint:mint-ifact table 5))))
      (ok (= 120 (mint:mint-value (mint:mint-nck table 10 3))))
      (ok (= 720 (mint:mint-value (mint:mint-npk table 10 3))))
      (ok (= 0 (mint:mint-value (mint:mint-nck table 3 5)))))))
