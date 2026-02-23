(in-package :cl-user)

(defpackage :deque-test
  (:use :cl :rove))
(in-package :deque-test)

(defun deque->list (dq)
  (loop for i from 0 below (deque:deque-size dq)
        collect (deque:deque-ref dq i)))

(deftest deque-basic-push-peek-pop
  (let ((dq (deque:make-deque)))
    (ok (deque:deque-empty-p dq))
    (ok (= 0 (deque:deque-size dq)))

    (deque:deque-push-back dq 1)
    (deque:deque-push-back dq 2)
    (deque:deque-push-front dq 0)

    (ok (= 3 (deque:deque-size dq)))
    (ok (equal '(0 1 2) (deque->list dq)))
    (ok (= 0 (deque:deque-peek-front dq)))
    (ok (= 2 (deque:deque-peek-back dq)))

    (deque:deque-pop-front dq)
    (ok (equal '(1 2) (deque->list dq)))
    (deque:deque-pop-back dq)
    (ok (equal '(1) (deque->list dq)))
    (deque:deque-pop-front dq)
    (ok (deque:deque-empty-p dq))
    (ok (= 0 (deque:deque-size dq)))))

(deftest deque-ref-and-setf
  (let ((dq (deque:make-deque)))
    (loop for x in '(10 20 30 40) do (deque:deque-push-back dq x))
    (ok (= 10 (deque:deque-ref dq 0)))
    (ok (= 30 (deque:deque-ref dq 2)))
    (setf (deque:deque-ref dq 1) 99)
    (ok (equal '(10 99 30 40) (deque->list dq)))
    (setf (deque:deque-ref dq 3) -1)
    (ok (equal '(10 99 30 -1) (deque->list dq)))))

(deftest deque-boundary-errors
  (let ((dq (deque:make-deque)))
    (ok (handler-case (progn (deque:deque-pop-front dq) nil)
          (error () t)))
    (ok (handler-case (progn (deque:deque-pop-back dq) nil)
          (error () t)))
    (ok (handler-case (progn (deque:deque-peek-front dq) nil)
          (error () t)))
    (ok (handler-case (progn (deque:deque-peek-back dq) nil)
          (error () t)))
    (ok (handler-case (progn (deque:deque-ref dq 0) nil)
          (error () t)))))

(deftest deque-extend-buffer-order
  (let ((dq (deque:make-deque)))
    ;; default capacity is 64; push 100 elements to force resize.
    (loop for i from 0 below 100 do (deque:deque-push-back dq i))
    (ok (= 100 (deque:deque-size dq)))
    (ok (= 0 (deque:deque-peek-front dq)))
    (ok (= 99 (deque:deque-peek-back dq)))
    (ok (equal (loop for i from 0 below 100 collect i)
               (deque->list dq)))
    (loop repeat 30 do (deque:deque-pop-front dq))
    (loop for i from 100 below 130 do (deque:deque-push-back dq i))
    (ok (= 100 (deque:deque-size dq)))
    (ok (equal (append (loop for i from 30 below 100 collect i)
                       (loop for i from 100 below 130 collect i))
               (deque->list dq)))))
