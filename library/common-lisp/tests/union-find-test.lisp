(in-package :cl-user)

(defpackage :union-find-test
  (:use :cl :rove))
(in-package :union-find-test)

(defun normalize-groups (groups)
  (sort (mapcar (lambda (g) (sort (copy-list g) #'<)) groups)
        (lambda (a b)
          (or (< (car a) (car b))
              (and (= (car a) (car b))
                   (< (length a) (length b)))))))

(deftest union-find-basic
  (let ((uf (union-find:make-union-find 6)))
    (ok (= 6 (union-find:union-find-size uf)))
    (ok (not (union-find:union-find-unite-p uf 0 1)))
    (ok (union-find:union-find-merge uf 0 1))
    (ok (union-find:union-find-unite-p uf 0 1))
    (ok (not (union-find:union-find-merge uf 1 0)))
    (ok (union-find:union-find-merge uf 1 2))
    (ok (union-find:union-find-unite-p uf 0 2))
    (ok (not (union-find:union-find-unite-p uf 0 3)))))

(deftest union-find-groups
  (let ((uf (union-find:make-union-find 7)))
    (union-find:union-find-merge uf 0 1)
    (union-find:union-find-merge uf 1 2)
    (union-find:union-find-merge uf 4 5)
    (let ((groups (normalize-groups (union-find:union-find-groups uf))))
      (ok (equal '((0 1 2) (3) (4 5) (6)) groups)))))

(deftest union-find-chain-compression-behavior
  (let ((uf (union-find:make-union-find 8)))
    (loop for i from 0 below 7
          do (union-find:union-find-merge uf i (1+ i)))
    (dotimes (i 8)
      (ok (union-find:union-find-unite-p uf 0 i)))
    (ok (equal '((0 1 2 3 4 5 6 7))
               (normalize-groups (union-find:union-find-groups uf))))))
