(in-package :cl-user)

(defpackage :vector-bintree-test
  (:use :cl :rove))
(in-package :vector-bintree-test)

(deftest vector-bintree-make-and-access
  (let ((bt (vector-bintree:make-vector-bintree 4 :initial-element 0)))
    (ok (= 4 (vector-bintree:bintree-size bt)))
    (ok (= 4 (vector-bintree:bintree-capacity bt)))
    (setf (vector-bintree:bintree-ref bt 2) 99)
    (ok (= 99 (vector-bintree:bintree-ref bt 2)))))

(deftest vector-bintree-extensible-push-pop
  (let ((bt (vector-bintree:make-extensible-vector-bintree 2)))
    (ok (= 0 (vector-bintree:bintree-size bt)))
    (ok (= 2 (vector-bintree:bintree-capacity bt)))
    (ok (= 0 (vector-bintree:bintree-push 10 bt)))
    (ok (= 1 (vector-bintree:bintree-push 20 bt)))
    (ok (= 2 (vector-bintree:bintree-size bt)))
    (ok (>= (vector-bintree:bintree-capacity bt) 2))
    (ok (= 20 (vector-bintree:bintree-pop bt)))
    (ok (= 10 (vector-bintree:bintree-pop bt)))
    (ok (= 0 (vector-bintree:bintree-size bt)))))

(deftest vector-bintree-extensible-initial-contents
  (let ((bt (vector-bintree:make-extensible-vector-bintree
             4 :initial-contents '(1 2 3 4))))
    (ok (= 4 (vector-bintree:bintree-size bt)))
    (ok (= 4 (vector-bintree:bintree-capacity bt)))
    (ok (equal '(1 2 3 4)
               (loop for i from 0 below (vector-bintree:bintree-size bt)
                     collect (vector-bintree:bintree-ref bt i))))))

(deftest vector-bintree-index-helpers
  (ok (vector-bintree:bintree-root-index-p 0))
  (ok (not (vector-bintree:bintree-root-index-p 1)))
  (ok (= 1 (vector-bintree:bintree-left-index 0)))
  (ok (= 2 (vector-bintree:bintree-right-index 0)))
  (ok (= 3 (vector-bintree:bintree-left-index 1)))
  (ok (= 4 (vector-bintree:bintree-right-index 1)))
  (ok (= 0 (vector-bintree:bintree-parent-index 1)))
  (ok (= 0 (vector-bintree:bintree-parent-index 2)))
  (ok (= 1 (vector-bintree:bintree-parent-index 3))))

(deftest vector-bintree-print
  (let* ((bt (vector-bintree:make-extensible-vector-bintree
              3 :initial-contents '(10 5 20)))
         (out (with-output-to-string (*standard-output*)
                (vector-bintree:bintree-print bt))))
    (ok (search "10" out))
    (ok (search "5" out))
    (ok (search "20" out))))

(deftest vector-bintree-errors-on-non-extensible
  (let ((bt (vector-bintree:make-vector-bintree 3 :initial-element 0)))
    (ok (handler-case (progn (vector-bintree:bintree-push 1 bt) nil)
          (error () t)))
    (ok (handler-case (progn (vector-bintree:bintree-pop bt) nil)
          (error () t)))))
