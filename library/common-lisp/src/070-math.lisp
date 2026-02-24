;;; utility.number math
;;;
(defpackage :math
  (:use :cl :utility.syntax :utility.number)
  (:export :exgcd
           :crt
           :combinatorics-table
           :make-combinatorics-table
           :combinatorics-fact
           :combinatorics-ifact
           :combinatorics-nck
           :combinatorics-npk
           :init-pascal-nck
           :pascal-nck))
(in-package :math)

(declaim (ftype (function (unsigned-byte unsigned-byte)
                          (values unsigned-byte integer integer))
                exgcd))
(defun exgcd (a b)
  (declare (type unsigned-byte a b))
  (labels ((rec (x y)
             (if (zerop y)
                 (values x 1 0)
                 (multiple-value-bind (g s u) (rec y (mod x y))
                   (values g
                           u
                           (- s (* (floor x y) u)))))))
    (rec a b)))

(defun crt (remainders moduli)
  "一般化中国剰余定理で同時合同式を解く。

引数:
  REMAINDERS: 整数 a_i のリスト
  MODULI:     正整数 m_i のリスト（REMAINDERS と同じ長さ）
  解く対象は次の連立合同式:
    x ≡ a_i (mod m_i)  (全ての i)

返り値:
  1) x : 統合後合同式の最小非負代表
  2) m : 統合後の法（統合した法の最小公倍数）
  したがって解集合は x + k*m（k は整数）。

連立が不整合な場合は:
  (values nil nil)

備考:
  - MODULI が互いに素である必要はない。
  - 内部では EXGCD を使って条件を1本ずつマージする。
  - 各マージは (a_i - 現在の x) が gcd(現在の m, m_i) で割り切れるときに限り可能。"
  #-atcoder
  (unless (and (listp remainders) (listp moduli) (= (length remainders) (length moduli)))
    (error "CRT: REMAINDERS and MODULI must be lists with same length."))
  (nlet rec ((rs remainders) (ms moduli) (x 0) (m 1))
    (when (null rs)
      (return-from crt (values x m)))
    (let ((a (car rs))
          (mi (car ms)))
      #-atcoder
      (unless (and (integerp a) (integerp mi) (> mi 0))
        (error "CRT: each remainder must be integer and each modulus must be positive integer, got (~S mod ~S)." a mi))
      (let ((ai (mod a mi)))
        (multiple-value-bind (g p q) (exgcd m mi)
          (declare (ignore q))
          ;; Merge two congruences:
          ;;   x ≡ current-x (mod m)
          ;;   x ≡ ai        (mod mi)
          ;; m*p + mi*q = g を使うと、
          ;;   current-x + m*t ≡ ai (mod mi)
          ;; の t を求められる。解があるのは (ai-current-x) が g で割り切れるときだけ。
          (let ((delta (- ai x)))
            (when (not (zerop (mod delta g)))
              (return-from crt (values nil nil)))
            (let* ((m2 (the unsigned-byte (floor mi g)))
                   (step (mod (* (floor delta g) p) m2))
                   (lcm (* m m2))
                   (new-x (mod (+ x (* m step)) lcm)))
              (rec (cdr rs) (cdr ms) new-x lcm))))))))

(defstruct (combinatorics-table
            (:constructor %make-combinatorics-table
                (max-n facts ifacts mul zero from-integer)))
  (max-n 0 :type fixnum :read-only t)
  (facts #() :type simple-vector :read-only t)
  (ifacts #() :type simple-vector :read-only t)
  (mul #'* :type function :read-only t)
  (zero 0 :type t :read-only t)
  (from-integer #'identity :type function :read-only t))

(defun make-combinatorics-table (max-n &key (one 1) (zero 0) (mul #'*)
                                         (from-integer #'identity) inv)
  #-atcoder
  (unless (and (integerp max-n) (<= 0 max-n))
    (error "MAKE-COMBINATORICS-TABLE: MAX-N must be a non-negative integer, got ~S." max-n))
  #-atcoder
  (unless inv
    (error "MAKE-COMBINATORICS-TABLE: :INV is required."))
  (let* ((n (the fixnum max-n))
         (facts (make-array (1+ n) :element-type t :initial-element one))
         (ifacts (make-array (1+ n) :element-type t :initial-element zero)))
    (loop for i fixnum from 1 to n
          do (setf (svref facts i)
                   (funcall mul (svref facts (1- i))
                            (funcall from-integer i))))
    (setf (svref ifacts n) (funcall inv (svref facts n)))
    (loop for i fixnum from n downto 1
          do (setf (svref ifacts (1- i))
                   (funcall mul (svref ifacts i)
                            (funcall from-integer i))))
    (%make-combinatorics-table n facts ifacts mul zero from-integer)))

(declaim (inline %combinatorics-check-index))
(defun %combinatorics-check-index (table n)
  #-atcoder
  (unless (and (integerp n) (<= 0 n (combinatorics-table-max-n table)))
    (error "COMBINATORICS index out of range: ~S (max ~S)"
           n (combinatorics-table-max-n table))))

(defun combinatorics-fact (table n)
  (%combinatorics-check-index table n)
  (svref (combinatorics-table-facts table) n))

(defun combinatorics-ifact (table n)
  (%combinatorics-check-index table n)
  (svref (combinatorics-table-ifacts table) n))

(defun combinatorics-nck (table n k)
  (%combinatorics-check-index table n)
  (if (or (< k 0) (> k n))
      (combinatorics-table-zero table)
      (let ((mul (combinatorics-table-mul table))
            (facts (combinatorics-table-facts table))
            (ifacts (combinatorics-table-ifacts table)))
        (funcall mul
                 (svref facts n)
                 (funcall mul
                          (svref ifacts k)
                          (svref ifacts (- n k)))))))

(defun combinatorics-npk (table n k)
  (%combinatorics-check-index table n)
  (if (or (< k 0) (> k n))
      (combinatorics-table-zero table)
      (let ((mul (combinatorics-table-mul table))
            (facts (combinatorics-table-facts table))
            (ifacts (combinatorics-table-ifacts table)))
        (funcall mul (svref facts n) (svref ifacts (- n k))))))

(defparameter *pascal-max-n* 0)
(defparameter *pascal-table*
  (make-array '(1 1) :element-type t :initial-element 1))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *pascal-expand-cache* nil))

(defun %make-pascal-table (n)
  (let ((table (make-array (list (1+ n) (1+ n))
                           :element-type t
                           :initial-element 0)))
    (loop for i fixnum from 0 to n
          do (setf (aref table i 0) 1
                   (aref table i i) 1)
             (loop for j fixnum from 1 below i
                   do (setf (aref table i j)
                            (+ (aref table (1- i) (1- j))
                               (aref table (1- i) j)))))
    table))

(defmacro init-pascal-nck (max-n)
  #-atcoder
  (unless (and (constantp max-n) (integerp max-n) (<= 0 max-n))
    (error "INIT-PASCAL-NCK: MAX-N must be a compile-time non-negative integer constant, got ~S."
           max-n))
  (let* ((n (the fixnum (eval max-n)))
         (cache-n (and *pascal-expand-cache* (car *pascal-expand-cache*)))
         (cache-table (and *pascal-expand-cache* (cdr *pascal-expand-cache*)))
         (table (if (and cache-n (>= cache-n n))
                    cache-table
                    (let ((new-table (%make-pascal-table n)))
                      (setf *pascal-expand-cache* (cons n new-table))
                      new-table))))
    `(progn
       (setf *pascal-max-n* ,n
             *pascal-table* ',table)
       ,n)))

(defun pascal-nck (n k)
  (cond ((or (< k 0) (> k n)) 0)
        ((or (zerop k) (= k n)) 1)
        (t
         #-atcoder
         (unless (and (integerp n) (<= 0 n *pascal-max-n*))
           (error "PASCAL-NCK: N out of range ~S (max ~S)."
                  n *pascal-max-n*))
         #-atcoder
         (unless (and (integerp k) (<= 0 k n))
           (error "PASCAL-NCK: K out of range ~S for N=~S." k n))
         (aref *pascal-table* n k))))

;;;
