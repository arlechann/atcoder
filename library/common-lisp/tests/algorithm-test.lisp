(in-package :cl-user)

(defpackage :algorithm-test
  (:use :cl :rove))
(in-package :algorithm-test)

(defun sorted-copy (xs)
  (sort (copy-list xs) #'<))

(deftest algorithm-sieve-and-primes
  (let ((s (algorithm:sieve-of-eratosthenes 20)))
    (ok (aref s 2))
    (ok (aref s 3))
    (ok (not (aref s 4)))
    (ok (aref s 19))
    (ok (not (aref s 20))))
  (multiple-value-bind (ps lpf) (algorithm:linear-sieve 20)
    (ok (equal '(2 3 5 7 11 13 17 19) (coerce ps 'list)))
    (ok (= 2 (aref lpf 2)))
    (ok (= 2 (aref lpf 12)))
    (ok (= 3 (aref lpf 15))))
  (ok (equal '(2 3 5 7 11 13 17 19)
             (algorithm:primes 20))))

(deftest algorithm-factorization-and-divisors
  (let* ((n 360)
         (lpf (algorithm:least-prime-factors n))
         (t-fac (algorithm:trivial-factorize n))
         (f-fac (algorithm:fast-factorize n lpf)))
    (ok (equal '((2 . 3) (3 . 2) (5 . 1))
               (sort t-fac #'< :key #'car)))
    (ok (equal '((2 . 3) (3 . 2) (5 . 1))
               (sort f-fac #'< :key #'car))))
  (let* ((n 72)
         (lpf (algorithm:least-prime-factors n))
         (td (sorted-copy (algorithm:trivial-divisors n)))
         (fd (sorted-copy (algorithm:fast-divisors n lpf))))
    (ok (equal td fd))
    (ok (equal '(1 2 3 4 6 8 9 12 18 24 36 72) td))))

(deftest algorithm-binary-search
  (let ((v #(1 2 2 2 4 7)))
    (ok (= 1 (algorithm:lower-bound v 2)))
    (ok (= 4 (algorithm:upper-bound v 2)))
    (ok (= 4 (algorithm:lower-bound v 3)))
    (ok (= 6 (algorithm:lower-bound v 9)))
    (ok (= 0 (algorithm:lower-bound v 0))))
  ;; meguru-method: predicate x^2 <= 30 となる最大整数
  (ok (= 5
         (algorithm:meguru-method 0 10 (lambda (x) (<= (* x x) 30))))))

(deftest algorithm-cumulate
  (ok (equal '(0 1 3 6 10)
             (coerce (algorithm:cumulate '(1 2 3 4)) 'list)))
  (ok (equal '(1 1 2 6 24)
             (coerce (algorithm:cumulate '(1 2 3 4) :op #'* :id 1) 'list))))

(deftest algorithm-dp-macros
  (let ((fib (algorithm:dp fib (n)
               (if (<= n 1)
                   n
                   (+ (fib (1- n))
                      (fib (- n 2)))))))
    (ok (= 0 (funcall fib 0)))
    (ok (= 1 (funcall fib 1)))
    (ok (= 55 (funcall fib 10))))
  (let ((paths (algorithm:array-dp paths ((y 4) (x 4))
                 (if (or (= y 0) (= x 0))
                     1
                     (+ (paths (1- y) x)
                        (paths y (1- x)))))))
    (ok (= 20 (funcall paths 3 3)))
    (ok (= 6 (funcall paths 2 2)))))
