(in-package :cl-user)

(defpackage :list-queue-test
  (:use :cl :rove))
(in-package :list-queue-test)

(deftest list-queue-basic
  (let ((q (list-queue:make-list-queue)))
    (ok (list-queue:list-queue-empty-p q))
    (ok (equal nil (list-queue:list-queue-raw q)))
    (list-queue:list-queue-enqueue q 10)
    (ok (not (list-queue:list-queue-empty-p q)))
    (ok (= 10 (list-queue:list-queue-peek q)))
    (ok (= 10 (list-queue:list-queue-dequeue q)))
    (ok (list-queue:list-queue-empty-p q))
    (ok (equal nil (list-queue:list-queue-raw q)))))

(deftest list-queue-fifo-order
  (let ((q (list-queue:make-list-queue)))
    (dolist (x '(1 2 3 4 5))
      (list-queue:list-queue-enqueue q x))
    (ok (equal '(1 2 3 4 5) (list-queue:list-queue-raw q)))
    (ok (equal '(1 2 3 4 5)
               (loop repeat 5 collect (list-queue:list-queue-dequeue q))))
    (ok (list-queue:list-queue-empty-p q))))

(deftest list-queue-reuse-after-empty
  (let ((q (list-queue:make-list-queue)))
    (list-queue:list-queue-enqueue q :a)
    (list-queue:list-queue-enqueue q :b)
    (ok (eq :a (list-queue:list-queue-dequeue q)))
    (ok (eq :b (list-queue:list-queue-dequeue q)))
    (ok (list-queue:list-queue-empty-p q))
    (list-queue:list-queue-enqueue q :c)
    (ok (eq :c (list-queue:list-queue-peek q)))
    (ok (eq :c (list-queue:list-queue-dequeue q)))
    (ok (list-queue:list-queue-empty-p q))))

(deftest list-queue-empty-errors
  (let ((q (list-queue:make-list-queue)))
    #-atcoder
    (ok (handler-case
            (progn (list-queue:list-queue-peek q) nil)
          (error () t)))
    #-atcoder
    (ok (handler-case
            (progn (list-queue:list-queue-dequeue q) nil)
          (error () t)))
    #+atcoder
    (ok (null (list-queue:list-queue-peek q)))
    #+atcoder
    (ok (null (list-queue:list-queue-dequeue q)))))
