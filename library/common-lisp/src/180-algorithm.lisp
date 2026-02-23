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
           :array-dp
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

(defmacro dp (name params &body body)
  "ハッシュ表メモ化付き再帰関数を生成して返す。"
  (let ((tmp-name (gensym))
        (args (mapcar #'(lambda (param)
                          (declare (ignore param))
                          (gensym))
                      params))
        (memo (gensym))
        (key (gensym))
        (memo-value (gensym))
        (none-value (gensym)))
    `(macrolet ((,name ,args `(,',tmp-name ,@(list ,@args))))
       (let ((,memo (make-hash-table :test 'equal)))
          (labels ((,tmp-name ,params
                     (let* ((,key (list ,@params))
                            (,memo-value (gethash ,key ,memo ',none-value)))
                       (if (not (eq ,memo-value ',none-value))
                           ,memo-value
                           (setf (gethash ,key ,memo)
                                 (block ,name ,@body))))))
            #',tmp-name)))))

(defmacro array-dp (name param-and-maxs &body body)
  "配列メモ化付き再帰関数を生成して返す。各引数の上限を指定する。"
  (let* ((tmp-name (gensym))
         (memo (gensym))
         (none-value (gensym))
         (memo-value (gensym))
         (params (mapcar #'car param-and-maxs))
         (maxs (mapcar #'cadr param-and-maxs))
         (args (mapcar #'(lambda (param)
                           (declare (ignore param))
                           (gensym))
                       params)))
    `(macrolet ((,name ,args `(,',tmp-name ,@(list ,@args))))
       (let ((,memo (make-array (list ,@maxs) :initial-element ',none-value)))
         (labels ((,tmp-name ,params
                    (let ((,memo-value (aref ,memo ,@params)))
                      (if (eq ,memo-value ',none-value)
                          (setf (aref ,memo ,@params)
                                (block ,name ,@body))
                          ,memo-value))))
           #',tmp-name)))))

;;;
