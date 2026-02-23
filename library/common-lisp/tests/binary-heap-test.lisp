(in-package :cl-user)

(defpackage :binary-heap-test
  (:use :cl :rove))
(in-package :binary-heap-test)

(defun pop-all (heap)
  (loop until (binary-heap:binary-heap-empty-p heap)
        collect (binary-heap:binary-heap-top heap)
        do (binary-heap:binary-heap-pop heap)))

(deftest binary-heap-min-basic
  (let ((heap (binary-heap:make-binary-heap #'<)))
    (ok (binary-heap:binary-heap-empty-p heap))
    (ok (= 0 (binary-heap:binary-heap-size heap)))

    (dolist (x '(5 1 4 2 3))
      (binary-heap:binary-heap-push x heap))

    (ok (= 5 (binary-heap:binary-heap-size heap)))
    (ok (= 1 (binary-heap:binary-heap-top heap)))
    (ok (equal '(1 2 3 4 5) (pop-all heap)))
    (ok (binary-heap:binary-heap-empty-p heap))))

(deftest binary-heap-max-basic
  (let ((heap (binary-heap:make-binary-heap #'>)))
    (dolist (x '(5 1 4 2 3))
      (binary-heap:binary-heap-push x heap))
    (ok (= 5 (binary-heap:binary-heap-top heap)))
    (ok (equal '(5 4 3 2 1) (pop-all heap)))))

(deftest binary-heap-initial-contents-heapify
  (let ((heap (binary-heap:make-binary-heap #'< :initial-contents '(7 2 9 1 5 3))))
    (ok (= 6 (binary-heap:binary-heap-size heap)))
    (ok (= 1 (binary-heap:binary-heap-top heap)))
    (ok (equal '(1 2 3 5 7 9) (pop-all heap)))))

(deftest binary-heap-setf-top-downheap
  (let ((heap (binary-heap:make-binary-heap #'< :initial-contents '(1 2 3 4 5))))
    (ok (= 1 (binary-heap:binary-heap-top heap)))
    (setf (binary-heap:binary-heap-top heap) 10)
    (ok (= 2 (binary-heap:binary-heap-top heap)))
    (ok (equal '(2 3 4 5 10) (pop-all heap)))))

(deftest binary-heap-duplicates
  (let ((heap (binary-heap:make-binary-heap #'<)))
    (dolist (x '(2 1 2 1 3 3))
      (binary-heap:binary-heap-push x heap))
    (ok (equal '(1 1 2 2 3 3) (pop-all heap)))))
