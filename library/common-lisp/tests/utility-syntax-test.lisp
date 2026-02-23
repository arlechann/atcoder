(in-package :cl-user)

(defpackage :utility-syntax-test
  (:use :cl :rove))
(in-package :utility-syntax-test)

(deftest named-let-and-nlet
  (ok (= 120
         (utility.syntax:named-let fact ((n 5))
           (if (<= n 1)
               1
               (* n (fact (1- n)))))))
  (ok (= 45
         (utility.syntax:nlet rec ((i 0) (acc 0))
           (if (= i 10)
               acc
               (rec (1+ i) (+ acc i)))))))

(deftest anaphoric-macros
  (ok (= 4 (utility.syntax:aif (+ 2 2) utility.syntax:it -1)))
  (ok (null (utility.syntax:aif nil utility.syntax:it nil)))
  (ok (= 6 (utility.syntax:aand 1 (+ utility.syntax:it 2) (+ utility.syntax:it 3))))
  (ok (null (utility.syntax:aand 1 nil (+ utility.syntax:it 3))))
  (ok (= 10
         (utility.syntax:aprog1 (+ 7 3)
           (+ utility.syntax:it 100)))))

(deftest let-family
  (ok (= 3
         (utility.syntax:if-let* ((a 1) (b 2))
           (+ a b)
           -1)))
  (ok (= -1
         (utility.syntax:if-let* ((a 1) (b nil))
           (+ a b)
           -1)))
  (ok (= 6
         (utility.syntax:when-let ((a 1) (b 2) (c 3))
           (+ a b c))))
  (ok (null
       (utility.syntax:when-let* ((a 1) (b nil))
         (+ a b)))))

(deftest iteration-macros
  (let ((sum 0))
    (ok (= 6
           (utility.syntax:do-array (x #(1 2 3) sum)
             (incf sum x)))))
  (let ((sum 0))
    (ok (= 21
           (utility.syntax:do-array* ((x y) (#(1 2 3) #(4 5 6 7)) sum)
             (incf sum (+ x y))))))
  (let ((sum 0))
    (ok (= 6
           (utility.syntax:do-seq (x '(1 2 3) sum)
             (incf sum x)))))
  (let ((sum 0))
    (ok (= 21
           (utility.syntax:do-seq* ((x y) ((list 1 2 3) (list 4 5 6)) sum)
             (incf sum (+ x y))))))
  (let ((positions nil))
    (utility.syntax:do-bit (i b 13 (setf positions (nreverse positions)))
      (when b (push i positions)))
    (ok (equal '(0 2 3) positions))))

(deftest neighbor-macros
  (let ((points nil))
    (utility.syntax:do-4neighbors ((y x) (2 3))
      (push (list y x) points))
    (ok (= 4 (length points)))
    (ok (every (lambda (p)
                 (= 1 (+ (abs (- (first p) 2))
                         (abs (- (second p) 3)))))
               points)))
  (let ((points nil))
    (utility.syntax:do-8neighbors ((y x) (2 3))
      (push (list y x) points))
    (ok (= 8 (length points)))
    (ok (member '(1 2) points :test #'equal))
    (ok (member '(3 4) points :test #'equal))))

(deftest misc-syntax
  (ok (= 5
         (utility.syntax:let-dyn ((x 2) (y 3))
           (+ x y))))
  (let ((cell (list 10)))
    (utility.syntax:flet-accessor ((value (obj) (car obj)))
      (ok (= 10 (value cell)))
      (setf (value cell) 42)
      (ok (= 42 (car cell)))))
  (multiple-value-bind (sym pkg) (utility.syntax:symb "AB" 12 "CD")
    (ok (eq *package* pkg))
    (ok (eq sym (find-symbol "AB12CD" *package*))))
  (ok (= 1
         (eval '(progn
                  (utility.syntax:defabbrev my-when when)
                  (let ((v 0))
                    (my-when t (setf v 1))
                    v)))))
  (unwind-protect
       (progn
         (eval '(utility.syntax:defalias syntax-test-1+ #'1+))
         (ok (= 11 (funcall (symbol-function 'syntax-test-1+) 10))))
    (fmakunbound 'syntax-test-1+)))
