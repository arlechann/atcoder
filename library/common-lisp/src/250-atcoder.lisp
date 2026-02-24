;;; atcoder
;;;
(defpackage atcoder
  (:use :cl
        :utility
        :input
        :list-queue
        :deque
        :binary-heap
        :ordered-map
        :linalg.vector
        :linalg.matrix
        :linalg.geometry
        :union-find
        :segment-tree
        :trie
        :graph
        :algorithm
        :amb
        :atcoder.test
        )
  (:export :main))
(in-package atcoder)

(defun main ()
  (input* ((a fixnum)
           (b fixnum))
    (format t "~A~%" (+ a b))))

#+atcoder (main)
