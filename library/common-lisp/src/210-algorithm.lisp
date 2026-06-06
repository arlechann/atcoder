;;; algorithm
;;;
(defpackage algorithm
  (:use :cl :utility)
  (:export :*default-sieve-max*
           :sieve-of-eratosthenes
           :linear-sieve
           :primes
           :least-prime-factors
           :trivial-factorize
           :fast-factorize
           :trivial-divisors
           :fast-divisors
           :meguru-method
           :lower-bound
           :upper-bound
           :cumulate
           :dp
           :dp-labels
           ))
(in-package algorithm)

(defparameter *default-sieve-max* 200000)

(defun sieve-of-eratosthenes (&optional (max *default-sieve-max*))
  "0..MAX の素数判定表（真偽ベクタ）をエラトステネス法で返す。"
  (let ((sieve (make-array (1+ max) :initial-element t)))
    (setf (aref sieve 0) nil
          (aref sieve 1) nil)
    (loop for k from 4 to max by 2
          do (setf (aref sieve k) nil))
    (loop for i from 3 by 2
          while (<= (* i i) max)
          when (aref sieve i)
            do (loop for k from (* i i) to max by i
                     do (setf (aref sieve k) nil)))
    sieve))

(defun linear-sieve (&optional (max *default-sieve-max*))
  "線形篩を実行し、(values primes least-prime-factors) を返す。"
  (let ((least-prime-factors (make-array (1+ max) :initial-element nil))
        (primes (make-array 0 :adjustable t :fill-pointer t)))
    (loop for i from 2 to max
          when (null (aref least-prime-factors i))
            do (setf (aref least-prime-factors i) i)
               (vector-push-extend i primes)
          do (loop for p across primes
                   until (or (> (* p i) max)
                             (> p (aref least-prime-factors i)))
                   do (setf (aref least-prime-factors (* p i)) p)))
    (values (coerce primes 'simple-vector)
            least-prime-factors)))

(defun primes (&optional (max *default-sieve-max*))
  "MAX 以下の素数リストを返す。"
  (coerce (linear-sieve max) 'list))

(defun least-prime-factors (&optional (max *default-sieve-max*))
  "各 n (0..MAX) の最小素因数テーブルを返す。"
  (multiple-value-bind (primes least-prime-factors) (linear-sieve max)
    (declare (ignore primes)) least-prime-factors))

(defun trivial-factorize (n)
  "試し割りで N を素因数分解し、(prime . exponent) のリストを返す。"
  (loop with ret = nil
        for i from 2
        while (<= (square i) n)
        do (loop with count = 0
                 while (zerop (mod n i))
                 do (setf n (floor n i))
                    (incf count)
                 finally (when (not (zerop count))
                           (push (cons i count) ret)))
        finally (unless (= n 1)
                  (push (cons n 1) ret))
                (return ret)))

(defun fast-factorize (n least-prime-factors)
  "最小素因数テーブルを使って N を高速に素因数分解する。"
  (loop with ret = nil
        while (> n 1)
        do (loop with lps = (aref least-prime-factors n)
                 and count = 0
                 while (eq lps (aref least-prime-factors n))
                 do (setf n (floor n lps))
                    (incf count)
                 finally (push (cons lps count) ret))
        finally (return ret)))

(defun trivial-divisors (n)
  "試し割りで N の約数リストを返す。"
  (loop with divisors = nil
        for i from 1
        while (< (* i i) n)
        when (zerop (mod n i))
          do (push i divisors)
             (push (floor n i) divisors)
        finally (when (= (* i i) n)
                  (push i divisors))
                (return divisors)))

(defun fast-divisors (n least-prime-factors)
  "素因数分解結果から N の約数リストを生成して返す。"
  (loop with divisors = (list 1)
        and factors = (fast-factorize n least-prime-factors)
        for (factor . exp) in factors
        do (loop for d in divisors
                 for m = 1 then 1
                 do (loop repeat exp
                          do (setf m (* m factor))
                             (push (* d m) divisors)))
        finally (return divisors)))

(defun meguru-method (ok ng pred)
  "めぐる式二分探索。PRED が真となる境界側の値 OK を返す。"
  (loop until (<= (abs (- ok ng)) 1)
        do (let ((mid (floor (+ ok ng) 2)))
             (if (funcall pred mid)
                 (setf ok mid)
                 (setf ng mid)))
        finally (return ok)))

(defun find-bound (vector element &key (compare #'<=) (start 0) end)
  "二分探索で ELEMENT を挿入できる境界インデックスを返す内部関数。"
  (meguru-method (or end (length vector)) (1- start)
                 (lambda (index) (funcall compare element (aref vector index)))))

(defun lower-bound (vector element &key (start 0) end)
  "昇順 VECTOR の lower_bound（最初の >= ELEMENT の位置）を返す。"
  (find-bound vector element :start start :end (or end (length vector))))

(defun upper-bound (vector element &key (start 0) end)
  "昇順 VECTOR の upper_bound（最初の > ELEMENT の位置）を返す。"
  (find-bound vector element :compare #'< :start start :end (or end (length vector))))

(defun cumulate (sequence &key (op #'+) (id 0) (element-type 't))
  "長さ+1 の累積配列を返す。先頭は ID、以降は OP で畳み込む。"
  (aprog1 (make-array (1+ (length sequence))
                         :element-type element-type
                         :initial-element id)
    (map-with-index nil
                    (lambda (i x)
                      (setf (aref it (1+ i))
                            (funcall op (aref it i) x)))
                    sequence)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-dp-definition (definition)
    (let ((for nil)
          (uncomputed nil)
          (uncomputed-p nil)
          (cases nil))
      (loop for (key value) on definition by #'cddr
            do (ecase key
                 (:for (setf for value))
                 (:uncomputed (setf uncomputed value
                                    uncomputed-p t))
                 (:case (setf cases value))))
      (unless for
        (error "DP requires :FOR."))
      (unless cases
        (error "DP requires :CASE."))
      (values for cases uncomputed uncomputed-p)))
  (defun dp-for-array-p (for)
    (and (listp for)
         (every (lambda (entry)
                  (and (consp entry)
                       (symbolp (car entry))
                       (consp (cdr entry))
                       (null (cddr entry))))
                for)))
  (defun make-array-dp-components (name for cases uncomputed uncomputed-p)
    (let ((sentinel (gensym))
          (memo (gensym))
          (memo-value (gensym))
          (params (mapcar #'car for))
          (maxs (mapcar #'cadr for)))
      (values
       `((,sentinel ,(if uncomputed-p uncomputed '(gensym "UNCOMPUTED-")))
         (,memo (make-array (list ,@maxs) :initial-element ,sentinel)))
       `(,name ,params
          (let ((,memo-value (aref ,memo ,@params)))
            (if (eq ,memo-value ,sentinel)
                (setf (aref ,memo ,@params)
                      (block ,name
                        (cond ,@cases)))
                ,memo-value))))))
  (defun make-hash-dp-components (name for cases uncomputed uncomputed-p)
    (let ((sentinel (gensym))
          (memo (gensym))
          (key (gensym))
          (memo-value (gensym)))
      (values
       `((,sentinel ,(if uncomputed-p uncomputed '(gensym "UNCOMPUTED-")))
         (,memo (make-hash-table :test 'equal)))
       `(,name ,for
          (let* ((,key (list ,@for))
                 (,memo-value (gethash ,key ,memo ,sentinel)))
            (if (eq ,memo-value ,sentinel)
                (setf (gethash ,key ,memo)
                      (block ,name
                        (cond ,@cases)))
                ,memo-value))))))
  (defun make-dp-label-components (binding)
    (destructuring-bind (name &rest definition) binding
      (multiple-value-bind (for cases uncomputed uncomputed-p)
          (parse-dp-definition definition)
        (if (dp-for-array-p for)
            (make-array-dp-components
             name for cases uncomputed uncomputed-p)
            (make-hash-dp-components
             name for cases uncomputed uncomputed-p)))))
  )

(defmacro dp (name &rest definition)
  "メモ化付き再帰関数を生成して返す。:FOR で状態を、:CASE で再帰式を指定し、:UNCOMPUTED を与えると未計算値を上書きする。

例:
  (let ((fib (dp fib
               :for (n)
               :case (((<= n 1) n)
                      (t (+ (fib (1- n))
                            (fib (- n 2))))))))
    (funcall fib 10))
  => 55

  (let ((paths (dp paths
                 :for ((y 4) (x 4))
                 :uncomputed -1
                 :case (((or (= y 0) (= x 0)) 1)
                        (t (+ (paths (1- y) x)
                              (paths y (1- x))))))))
    (funcall paths 3 3))
  => 20"
  `(dp-labels ((,name ,@definition))
     #',name))

(defmacro dp-labels (bindings &body body)
  "DP 関数群をローカル関数として束縛する。各定義は DP と同じ DSL を使う。"
  (let ((let-bindings nil)
        (label-bindings nil))
    (dolist (binding bindings)
      (multiple-value-bind (new-let-bindings label-binding)
          (make-dp-label-components binding)
        (setf let-bindings (append let-bindings new-let-bindings))
        (push label-binding label-bindings)))
    `(let* ,let-bindings
       (labels ,(nreverse label-bindings)
         ,@body))))

;;;
