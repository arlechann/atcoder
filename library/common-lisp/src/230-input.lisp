;;; input
;;;
(defpackage :input
  (:use :cl :utility)
  (:export :input*
           :def-input-reader))
(in-package :input)

(eval-always (defvar *input-reader-table* (make-hash-table :test #'eq)))

(defun-always set-input-reader (marker reader)
  (setf (gethash marker *input-reader-table*) reader))

(defun-always get-input-reader (marker) (gethash marker *input-reader-table*))

(defun-always input-typespec-marker (typespec)
  (let ((*package* #.*package*))
    (symb (if (listp typespec)
              (car typespec)
              typespec))))

(defun-always input-typespec-reader (typespec)
  (let* ((marker (input-typespec-marker typespec))
         (reader (and marker (get-input-reader marker))))
    (if reader
        (funcall reader typespec)
        (error "Unknown input typespec: ~S (marker: ~S)" typespec marker))))

(defun-always input-expand (forms)
  (if (null forms)
      nil
      (mapcar (lambda (form)
                (list (car form)
                      (input-typespec-reader (cadr form))))
              forms)))

(defmacro input* (forms &body body)
  "競技プログラミング向けの入力束縛マクロ。

形式:
  (input* ((var1 typespec1)
           (var2 typespec2)
           ...)
    body...)

概要:
  - 各 (var typespec) を入力式に展開し、LET* で順に束縛する。
  - 後続の typespec から先行変数を参照できる。
    例: (list fixnum n), (vector fixnum m), (array fixnum (h w))
  - 未使用変数警告を避けるため、展開後に (declare (ignorable ...)) を付与する。

標準で使える主な typespec:
  - fixnum / fixnum1 / double / double-float / rational / string / symbol
  - (cons A B), (cons* A B ...), nil
  - (list ELEM LEN), (vector ELEM LEN), (array ELEM DIMS)

拡張:
  - `def-input-reader` で marker ごとの読み取り規則を追加できる。
  - 未登録 marker を指定するとエラーになる。

例:
  (input* ((n fixnum)
           (m fixnum1)
           (f double)
           (s string)
           (p (cons* fixnum fixnum nil))
           (l (list fixnum n))
           (v (vector fixnum m)))
    (list n m f s p l v))

入力:
  2 3 10.5
  input
  1000000007 -1
  10 20
  30 40

結果:
  (2 2 10.5d0 \"input\" (1000000007 -1) (10 20) #(30 40))

多次元 array の例:
  (input* ((h fixnum)
           (w fixnum)
           (a (array fixnum (h w))))
    a)

入力:
  2 3
  1 2 3
  4 5 6

結果:
  #2A((1 2 3) (4 5 6))"
    (let ((vars (mapcar #'car forms)))
      `(let* ,(input-expand forms)
         (declare (ignorable ,@vars))
         ,@body)))

(defmacro def-input-reader (marker (typespec read-of) &body body)
  "新しい input typespec reader を登録する。

形式:
  (def-input-reader marker (typespec read-of)
    expansion-form)

概要:
  - MARKER は typespec 先頭シンボル（例: list, vector, my-type）。
  - BODY は「入力式そのもの」を返す。実行結果ではなく式を返す点に注意。
  - READ-OF はサブ typespec を再帰的に入力式へ変換する補助関数。

例:
  (def-input-reader pair (typespec read-of)
    `(cons ,(read-of 'fixnum) ,(read-of 'fixnum)))"
  `(eval-always
     (set-input-reader ',marker
                       (lambda (,typespec)
                         (declare (ignorable ,typespec))
                         (flet ((,read-of (sub-typespec)
                                  (input-typespec-reader sub-typespec)))
                           (declare (ignorable (function ,read-of)))
                           ,@body)))
     ',marker))

(def-input-reader fixnum (typespec read-of)
  '(read))
(def-input-reader fixnum1 (typespec read-of)
  '(1- (read)))
(def-input-reader double (typespec read-of)
  '(let ((*read-default-float-format* 'double-float)) (read)))

(def-input-reader double-float (typespec read-of)
  '(let ((*read-default-float-format* 'double-float)) (read)))

(def-input-reader rational (typespec read-of)
  '(let ((*read-default-float-format* 'rational)) (read)))
(def-input-reader string (typespec read-of)
  '(read-line))
(def-input-reader symbol (typespec read-of)
  '(read))

(def-input-reader cons (typespec read-of)
  `(cons ,(read-of (cadr typespec))
         ,(read-of (caddr typespec))))

(def-input-reader cons* (typespec read-of)
  (let ((elems (cdr typespec)))
    (if (null (cdr elems))
        (read-of (car elems))
        `(cons ,(read-of (car elems))
               ,(read-of (cons 'cons* (cdr elems)))))))

(def-input-reader null (typespec read-of)
  nil)

(def-input-reader list (typespec read-of)
  (let ((elem (cadr typespec))
        (len (caddr typespec)))
    `(loop repeat ,len
           collect ,(read-of elem))))

(def-input-reader vector (typespec read-of)
  (let ((vec (gensym))
        (index (gensym))
        (elem (cadr typespec))
        (len (caddr typespec)))
    `(let ((,vec (make-array ,len)))
       (dotimes (,index ,len ,vec)
         (setf (aref ,vec ,index) ,(read-of elem))))))

(def-input-reader array (typespec read-of)
  (let ((arr (gensym))
        (index (gensym))
        (dimensions (gensym))
        (dims (gensym))
        (indexes (gensym))
        (elem (cadr typespec))
        (size (ensure-list (caddr typespec))))
    `(let* ((,dimensions (list ,@size))
            (,arr (make-array ,dimensions)))
       (labels ((rec (,dims ,indexes)
                  (if (null (cdr ,dims))
                      (dotimes (,index (car ,dims))
                        (setf (apply #'aref ,arr (reverse (cons ,index ,indexes)))
                              ,(read-of elem)))
                      (dotimes (,index (car ,dims))
                        (rec (cdr ,dims) (cons ,index ,indexes))))))
         (rec ,dimensions nil)
         ,arr))))

;;;
