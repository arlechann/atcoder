(in-package :cl-user)

(defpackage :input-test
  (:use :cl :rove))
(in-package :input-test)

(deftest input-basic-readers
  (with-input-from-string (*standard-input* "2 3 4/5 foo")
    (input:input* ((a fixnum)
                   (b fixnum1)
                   (c rational)
                   (d symbol))
      (ok (= 2 a))
      (ok (= 2 b))
      (ok (= 4/5 c))
      (ok (eq 'foo d)))))

(deftest input-compound-readers
  (with-input-from-string (*standard-input* "10 20 3 1 2 3 2 9 8 1 2 3 4")
    (input:input* ((p (cons fixnum fixnum))
                   (n fixnum)
                   (lst (list fixnum n))
                   (m fixnum)
                   (vec (vector fixnum m))
                   (arr (array fixnum (2 2))))
      (ok (equal '(10 . 20) p))
      (ok (= 3 n))
      (ok (equal '(1 2 3) lst))
      (ok (= 2 m))
      (ok (equal '(9 8) (coerce vec 'list)))
      (ok (equal '((1 2) (3 4))
                 (loop for i below 2
                       collect (loop for j below 2
                                     collect (aref arr i j))))))))

(deftest input-string-reader
  (with-input-from-string (*standard-input* (format nil "hello world~%"))
    (input:input* ((s string))
      (ok (equal "hello world" s)))))

(deftest input-macro-expansion
  (let ((exp (macroexpand-1 '(input:input* ((x fixnum) (y fixnum)) (+ x y)))))
    (ok (eq 'let* (car exp)))
    (ok (equal '(declare (ignorable x y)) (caddr exp)))))

(deftest input-unknown-typespec-error
  (ok (handler-case
          (progn (macroexpand-1 '(input:input* ((x unknown-typespec)) x))
                 nil)
        (error () t))))

(deftest input-custom-reader-by-def-input-reader
  (input:def-input-reader input::pair2-test (typespec read-of)
    `(cons ,(read-of (cadr typespec))
           ,(read-of (cadr typespec))))
  (with-input-from-string (*standard-input* "11 22")
    (ok (equal '(11 . 22)
               (eval '(input:input* ((p (input::pair2-test fixnum)))
                        p))))))
