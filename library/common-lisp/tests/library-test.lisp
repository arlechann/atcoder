(in-package :cl-user)

(defpackage :library-test
  (:use :cl :rove))
(in-package :library-test)

(deftest if-match-basic
  (ok (equal '(1 3)
             (match:if-match (list a 'z c)
                 '(1 z 3)
               (list a c)
               'unmatched)))
  (ok (eq 'unmatched
          (match:if-match (list a 'z c)
              '(1 q 3)
            (list a c)
            'unmatched))))

(deftest if-match-cons/list*
  (ok (= 3 (match:if-match (cons x y)
                '(1 2)
              (+ x (car y))
              -1)))
  (ok (equal '(1 2 (3 4))
             (match:if-match (list* a b tail)
                 '(1 2 3 4)
               (list a b tail)
               :ng))))

(deftest if-match-guard
  (ok (eq :ok
          (match:if-match (match::guard (list x y) (> x y))
              '(3 1)
            :ok
            :ng)))
  (ok (eq :ng
          (match:if-match (match::guard (list x y) (> x y))
              '(1 3)
            :ok
            :ng))))

(deftest if-match-repeated-variable
  (ok (eq :ok
          (match:if-match (list x x)
              '(5 5)
            :ok
            :ng)))
  (ok (eq :ng
          (match:if-match (list x x)
              '(5 6)
            :ok
            :ng))))

(deftest match-basic
  (ok (= 6
         (match:match '(1 2 3)
           ((list x y z) (+ x y z))
           (otherwise -1))))
  (ok (eq :other
          (match:match '(1 2)
            ((list _ _ _) :triple)
            (otherwise :other)))))

(defun classify-match (x)
  (match:match x
    ((list 0 _) :zero-first)
    ((match::guard (list a b) (> a b)) :descending-pair)
    ((list _ _) :pair)
    (otherwise :other)))

(defun classify-cond (x)
  (cond ((and (consp x)
              (consp (cdr x))
              (null (cddr x))
              (eql (car x) 0))
         :zero-first)
        ((and (consp x)
              (consp (cdr x))
              (null (cddr x))
              (> (car x) (cadr x)))
         :descending-pair)
        ((and (consp x)
              (consp (cdr x))
              (null (cddr x)))
         :pair)
        (t :other)))

(deftest match-cond-equivalence
  (let ((mismatch 0))
    (loop for i from -50 to 50
          do (loop for j from -50 to 50
                   for datum = (list i j)
                   unless (eql (classify-match datum)
                               (classify-cond datum))
                     do (incf mismatch)))
    (ok (zerop mismatch)))
  (ok (eql (classify-match '(1 2 3))
           (classify-cond '(1 2 3)))))
