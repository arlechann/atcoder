(in-package :cl-user)

(defpackage :segment-tree-test
  (:use :cl :rove))
(in-package :segment-tree-test)

(deftest segment-tree-basic-fold
  (let ((st (segment-tree:make-segment-tree 5 #'+ 0 :initial-contents '(1 2 3 4 5))))
    (ok (= 1 (segment-tree:segment-tree-ref st 0)))
    (ok (= 5 (segment-tree:segment-tree-ref st 4)))
    (ok (= 15 (segment-tree:segment-tree-fold st 0 5)))
    (ok (= 9 (segment-tree:segment-tree-fold st 1 4)))
    (ok (= 0 (segment-tree:segment-tree-fold st 2 2)))))

(deftest segment-tree-setf-update
  (let ((st (segment-tree:make-segment-tree 6 #'+ 0 :initial-contents '(1 1 1 1 1 1))))
    (ok (= 6 (segment-tree:segment-tree-fold st 0 6)))
    (setf (segment-tree:segment-tree-ref st 2) 10)
    (ok (= 10 (segment-tree:segment-tree-ref st 2)))
    (ok (= 15 (segment-tree:segment-tree-fold st 0 6)))
    (setf (segment-tree:segment-tree-ref st 5) -3)
    (ok (= 11 (segment-tree:segment-tree-fold st 0 6)))
    (ok (= 9 (segment-tree:segment-tree-fold st 2 6)))))

(deftest segment-tree-min-query
  (let ((st (segment-tree:make-segment-tree 5 #'min most-positive-fixnum
                                            :initial-contents '(8 6 7 5 3))))
    (ok (= 3 (segment-tree:segment-tree-fold st 0 5)))
    (ok (= 5 (segment-tree:segment-tree-fold st 1 4)))
    (setf (segment-tree:segment-tree-ref st 4) 9)
    (ok (= 5 (segment-tree:segment-tree-fold st 0 5)))))

(deftest segment-tree-boundary-errors
  (let ((st (segment-tree:make-segment-tree 4 #'+ 0 :initial-contents '(1 2 3 4))))
    (ok (handler-case (progn (segment-tree:segment-tree-ref st -1) nil)
          (error () t)))
    (ok (handler-case (progn (segment-tree:segment-tree-ref st 4) nil)
          (error () t)))
    (ok (handler-case (progn (setf (segment-tree:segment-tree-ref st 4) 10) nil)
          (error () t)))
    (ok (handler-case (progn (segment-tree:segment-tree-fold st -1 2) nil)
          (error () t)))
    (ok (handler-case (progn (segment-tree:segment-tree-fold st 3 2) nil)
          (error () t)))
    (ok (handler-case (progn (segment-tree:segment-tree-fold st 0 5) nil)
          (error () t)))))
