(in-package :cl-user)

(defpackage :amb-test
  (:use :cl :rove))
(in-package :amb-test)

(deftest amb-basic-choices
  (amb:amb-reset)
  (setf amb:*failed* :failed)
  (ok (= 1 (amb:amb 1 2 3)))
  (ok (= 2 (amb:amb)))
  (ok (= 3 (amb:amb)))
  (ok (eq :failed (amb:amb)))
  ;; no continuation should remain
  (ok (eq :failed (amb:amb))))

(deftest amb-bind-basic
  (amb:amb-reset)
  (setf amb:*failed* :failed)
  (ok (= 10 (amb:amb-bind x '(10 20 30) x)))
  (ok (= 20 (amb:amb)))
  (ok (= 30 (amb:amb)))
  (ok (eq :failed (amb:amb))))

(deftest amb-bind-empty-options
  (amb:amb-reset)
  (setf amb:*failed* :failed)
  (ok (eq :failed (amb:amb-bind x '() x))))

(deftest amb-nested-search
  (amb:amb-reset)
  (setf amb:*failed* :failed)
  ;; x in (1 2 3), y in (1 2 3), x+y=4
  (ok (equal '(1 3)
             (amb:amb-bind x '(1 2 3)
               (amb:amb-bind y '(1 2 3)
                 (if (= (+ x y) 4)
                     (list x y)
                     (amb:amb))))))
  (ok (equal '(2 2) (amb:amb)))
  (ok (equal '(3 1) (amb:amb)))
  (ok (eq :failed (amb:amb))))
