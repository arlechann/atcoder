(in-package :cl-user)

(defpackage :ordered-map-test
  (:use :cl :rove))
(in-package :ordered-map-test)

(defun make-int-tree ()
  (ordered-map:make-rbtree :key #'identity :key-eq-p #'= :key-less-p #'<))

(defun sorted-copy (xs)
  (sort (copy-list xs) #'<))

(defun lb-ref (xs k)
  (find-if (lambda (x) (>= x k)) (sorted-copy xs)))

(defun ub-ref (xs k)
  (find-if (lambda (x) (> x k)) (sorted-copy xs)))

(deftest ordered-map-basic-insert-search
  (let ((tree (make-int-tree)))
    (ordered-map:rbtree-insert tree 3)
    (ordered-map:rbtree-insert tree 1)
    (ordered-map:rbtree-insert tree 4)
    (ordered-map:rbtree-insert tree 1)
    (ok (equal '(1 3 4) (ordered-map:rbtree-to-list tree)))
    (ok (= 3 (ordered-map:rbtree-search tree 3 :default -1)))
    (ok (= -1 (ordered-map:rbtree-search tree 2 :default -1)))
    (ok (= 1 (ordered-map:rbtree-first tree)))
    (ok (= 4 (ordered-map:rbtree-last tree)))))

(deftest ordered-map-lower-upper-bound
  (let ((tree (make-int-tree)))
    (dolist (x '(10 20 30 40 50))
      (ordered-map:rbtree-insert tree x))
    (ok (= 10 (ordered-map:rbtree-lower-bound tree 5 :end -1)))
    (ok (= 20 (ordered-map:rbtree-lower-bound tree 20 :end -1)))
    (ok (= 30 (ordered-map:rbtree-upper-bound tree 20 :end -1)))
    (ok (= -1 (ordered-map:rbtree-lower-bound tree 60 :end -1)))
    (ok (= -1 (ordered-map:rbtree-upper-bound tree 50 :end -1)))))

(deftest ordered-map-remove
  (let ((tree (make-int-tree)))
    (dolist (x '(5 2 8 1 3 7 9))
      (ordered-map:rbtree-insert tree x))
    (ordered-map:rbtree-remove tree 5)
    (ordered-map:rbtree-remove tree 1)
    (ordered-map:rbtree-remove tree 9)
    (ok (equal '(2 3 7 8) (ordered-map:rbtree-to-list tree)))
    (ok (= -1 (ordered-map:rbtree-search tree 5 :default -1)))
    (ok (= 2 (ordered-map:rbtree-first tree)))
    (ok (= 8 (ordered-map:rbtree-last tree)))))

(deftest ordered-map-randomized-regression
  (let ((tree (make-int-tree))
        (ref nil)
        (seed 2463534242))
    (labels ((next-rand ()
               (setf seed (ldb (byte 31 0) (+ (* 1103515245 seed) 12345)))
               seed)
             (insert-ref (x)
               (pushnew x ref :test #'=))
             (remove-ref (x)
               (setf ref (remove x ref :test #'=))))
      (dotimes (_ 600)
        (let ((x (mod (next-rand) 80)))
          (if (zerop (mod (next-rand) 3))
              (progn
                (ordered-map:rbtree-remove tree x)
                (remove-ref x))
              (progn
                (ordered-map:rbtree-insert tree x)
                (insert-ref x)))))
      (let ((sorted (sorted-copy ref)))
        (ok (equal sorted (ordered-map:rbtree-to-list tree)))
        (dotimes (k 85)
          (ok (eql (lb-ref sorted k)
                   (ordered-map:rbtree-lower-bound tree k :end nil)))
          (ok (eql (ub-ref sorted k)
                   (ordered-map:rbtree-upper-bound tree k :end nil))))))))
