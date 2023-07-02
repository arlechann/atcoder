(in-package :cl-user)
#-swank (declaim (optimize (speed 3) (safety 0) (debug 0)))

;;;
;;; cl-proconio
;;;
(in-package :cl-user)
(defpackage cl-proconio/marker/core
  (:use :cl)
  (:export :type-marker
           :put-marker-table
           :get-marker-table
           :apply-marker
           :reader
           :type-specifier
           :default-value
           :defmarker
           ))
(in-package :cl-proconio/marker/core)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun type-marker (ty)
    (if (atom ty)
        ty
        (car ty)))
  (defvar *marker-table* (make-hash-table :test #'equal))
  (defun get-marker-table (op ty)
    (gethash (list op ty) *marker-table*))
  (defun put-marker-table (op ty item)
    (setf (gethash (list op ty) *marker-table*) item))
  (defun apply-marker (op &rest args)
    (let* ((markers (mapcar #'type-marker args))
           (func (get-marker-table op markers)))
      (if func
          (apply func args)
          (error (format nil "Undefined makers: ~A" markers)))))
  (defun reader (ty &rest args) (apply #'apply-marker :reader ty args))
  (defun type-specifier (ty) (apply-marker :type-specifier ty))
  (defun default-value (ty) (apply-marker :default-value ty))
  (defmacro defmarker (marker name args &body body)
    (let ((name-sym (intern (format nil "~A-~A" marker name))))
      `(progn (defun  ,name-sym ,args
                ,@body)
              (put-marker-table ,(intern (symbol-name name) "KEYWORD")
                                '(,(intern (symbol-name marker) "KEYWORD"))
                                #',name-sym)))))

(in-package :cl-user)
(defpackage cl-proconio/marker/any
  (:use :cl-proconio/marker/core :cl))
(in-package :cl-proconio/marker/any)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmarker :* any-reader (ty)
    (declare (ignore ty))
    '(read))
  (defmarker :* type-specifier (ty)
    (declare (ignore ty))
    '*)
  (defmarker :* default-value (ty)
    (declare (ignore ty))
    nil))

(in-package :cl-user)
(defpackage cl-proconio/marker/fixnum
  (:use :cl-proconio/marker/core :cl))
(in-package :cl-proconio/marker/fixnum)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmarker :fixnum reader (ty)
    (declare (ignore ty))
    '(the fixnum (read)))
  (defmarker :fixnum type-specifier (ty)
    (declare (ignore ty))
    'fixnum)
  (defmarker :fixnum default-value (ty)
    (declare (ignore ty))
    0))

(in-package :cl-user)
(defpackage cl-proconio/marker/fixnum1
  (:use :cl-proconio/marker/core :cl))
(in-package :cl-proconio/marker/fixnum1)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmarker :fixnum1 reader (ty)
    (declare (ignore ty))
    '(the fixnum (1- (read))))
  (defmarker :fixnum1 type-specifier (ty)
    (declare (ignore ty))
    'fixnum)
  (defmarker :fixnum1 default-value (ty)
    (declare (ignore ty))
    0))

(in-package :cl-user)
(defpackage cl-proconio/marker/integer
  (:use :cl-proconio/marker/core :cl))
(in-package :cl-proconio/marker/integer)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmarker :integer reader (ty)
    (declare (ignore ty))
    '(the integer (read)))
  (defmarker :integer type-specifier (ty)
    (declare (ignore ty))
    'integer)
  (defmarker :integer default-value (ty)
    (declare (ignore ty))
    0))

(in-package :cl-user)
(defpackage cl-proconio/marker/float
  (:use :cl-proconio/marker/core :cl))
(in-package :cl-proconio/marker/float)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmarker :float reader (ty)
    (declare (ignore ty))
    '(let ((*read-default-float-format* 'single-float))
       (the single-float (read))))
  (defmarker :float type-specifier (ty)
    (declare (ignore ty))
    'single-float)
  (defmarker :float default-value (ty)
    (declare (ignore ty))
    0.0))

(in-package :cl-user)
(defpackage cl-proconio/marker/double
  (:use :cl-proconio/marker/core :cl))
(in-package :cl-proconio/marker/double)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmarker :double reader (ty)
    (declare (ignore ty))
    '(let ((*read-default-float-format* 'double-float))
       (the double-float (read))))
  (defmarker :double type-specifier (ty)
    (declare (ignore ty))
    'double-float)
  (defmarker :double default-value (ty)
    (declare (ignore ty))
    0.0d0))

(in-package :cl-user)
(defpackage cl-proconio/marker/list
  (:use :cl-proconio/marker/core :cl))
(in-package :cl-proconio/marker/list)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun element (ty) (cadr ty))
  (defun size (ty) (caddr ty))
  (defmarker :list reader (ty)
    (labels ((rec (ls acc)
               (if (null ls)
                   (cons 'list (nreverse acc))
                   (rec (cdr ls) (cons (reader (car ls)) acc)))))
      (rec (cdr ty) nil)))
  (defmarker :list type-specifier (ty)
    (labels ((rec (ls)
               (if (null ls)
                   'null
                   (list 'or (list 'cons (type-specifier (car ls)) (rec (cdr ls))) 'null))))
      (rec (cdr ty))))
  (defmarker :list default-value (ty)
    (declare (ignore ty))
    nil))

(in-package :cl-user)
(defpackage cl-proconio/marker/string
  (:use :cl-proconio/marker/core :cl))
(in-package :cl-proconio/marker/string)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmarker :string reader (ty)
    (declare (ignore ty))
    '(the simple-string (read-line *standard-input* nil "")))
  (defmarker :string type-specifier (ty)
    (declare (ignore ty))
    'simple-string)
  (defmarker :string default-value (ty)
    (declare (ignore ty))
    (make-array 0 :element-type 'character)))

(in-package :cl-user)
(defpackage cl-proconio/marker/vector
  (:use :cl-proconio/marker/core :cl))
(in-package :cl-proconio/marker/vector)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun element (ty) (cadr ty))
  (defun size (ty) (caddr ty))
  (defmarker :vector reader (ty)
    (let ((vec (gensym "VEC"))
          (index (gensym "INDEX")))
      `(loop with ,vec = (make-array ,(size ty) :element-type ',(type-specifier (element ty))
                                                :initial-element ,(default-value (element ty)))
             for ,index from 0 below ,(size ty)
             do (setf (aref ,vec ,index) ,(reader (element ty)))
             finally (return ,vec))))
  (defmarker :vector type-specifier (ty)
    (list 'simple-array (type-specifier (element ty)) (list (size ty))))
  (defmarker :vector default-value (ty)
    (make-array (size ty) :element-type (type-specifier (element ty))
                          :initial-element (default-value (element ty)))))

(in-package :cl-user)
(defpackage cl-proconio/marker/array
  (:use :cl-proconio/marker/core :cl))
(in-package :cl-proconio/marker/array)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun element (ty) (cadr ty))
  (defun dimensions (ty) (caddr ty))
  (defmarker :array reader (ty)
    (let ((arr (gensym "ARR"))
          (index (gensym "INDEX")))
      `(loop with ,arr = (make-array ',(dimensions ty) :element-type ',(type-specifier (element ty))
                                                       :initial-element ,(default-value (element ty)))
             for ,index from 0 below (apply #'* ',(dimensions ty))
             do (setf (row-major-aref ,arr ,index) ,(reader (element ty)))
             finally (return ,arr))))
  (defmarker :array type-specifier (ty)
    (list 'simple-array (type-specifier (element ty)) (dimensions ty)))
  (defmarker :array default-value (ty)
    (make-array (dimensions ty) :element-type (type-specifier (element ty))
                                :initial-element (default-value (element ty)))))

(in-package :cl-user)
(defpackage cl-proconio
  (:use :cl-proconio/marker/core :cl)
  (:export :input*))
(in-package :cl-proconio)
(defmacro input* (form &body body)
  (labels ((input-form (form)
            (if (null form)
                nil
                (mapcar (lambda (ty)
                          (list (car ty)
                                (reader (cadr ty))))
                        form)))) 
    `(let* ,(input-form form) ,@body)))

;;;
;;; utility
;;;
(defpackage :utility
  (:use :cl)
  (:export :dbg
           :nlet
           :swap
           :div
           :diff
           :half
           :read-ascii-char
           :read-ascii-string
           ))
(in-package :utility)

(declaim (inline div half))

(defmacro dbg (body)
  (let ((ret (gensym)))
  `(let ((,ret ,body))
    (format t "*** debug *** ~S~%" ,ret)
    ,ret)))

(defmacro nlet (name binds &body body)
  `(labels ((,name ,(mapcar #'car binds) ,@body))
     (,name ,@(mapcar #'cadr binds))))

(defmacro swap (a b)
  (let ((tmp (gensym)))
    `(let ((,tmp ,a))
       (setf ,a ,b)
       (setf ,b ,tmp))))

(defun div (a b) 
  (declare (type fixnum a b))
  (the fixnum (floor a b)))

(defun diff (a b)
  (declare (type fixnum a b))
  (the fixnum (abs (- a b))))

(defun half (n)
  (declare (type fixnum n))
  (the fixnum (ash n -1)))

(declaim (inline whitespacep))
(defun whitespacep (c)
  (let ((code (char-code c)))
    (declare (type fixnum code))
    (or (<= 9 code 13) (= code 32))))

(declaim (inline read-ascii-char))
(defun read-ascii-char (&optional (s *standard-input*) (eof-error-p t) eof-value)
  #+swank (the standard-char (read-char s eof-error-p eof-value))
  #-swank (the standard-char (code-char (read-byte s eof-error-p eof-value))))

(let* ((buf-size 65536)
       (buf (make-array buf-size :element-type 'standard-char :initial-element #\.)))
  (defun read-ascii-string (&optional (s *standard-input*))
    (labels ((skip-char-p (c)
               (declare (type (or standard-char null) c))
               (or (null c) (whitespacep c)))
             (skip-whitespace ()
               (let ((c (read-ascii-char s nil nil)))
                 (declare (type standard-char c))
                 (if (skip-char-p c)
                     (skip-whitespace)
                     (write-buf c 0 nil))))
             (write-buf (c index acc)
               (declare (type (or standard-char null) c)
                        (type fixnum index)
                        (type (or (cons simple-string *) null) acc))
               (cond ((skip-char-p c)
                       (concat-list (nreverse (cons (copy-buf index) acc))))
                     ((= index buf-size)
                       (write-buf c 0 (cons (copy-buf buf-size) acc)))
                     (t (progn (setf (aref buf index) c)
                               (write-buf (read-ascii-char s nil nil) (1+ index) acc)))))
             (copy-buf (index)
               (declare (type fixnum index))
               (the simple-string (subseq buf 0 index)))
             (concat-list (ls)
               (declare (type (or (cons simple-string *) null) ls))
               (let* ((size (reduce (lambda (acc str) (+ acc (length str)))
                                    ls :initial-value 0))
                      (str (make-array size :element-type 'standard-char :initial-element #\.)))
                 (declare (type fixnum size) (type simple-string str))
                 (nlet rec ((i 0) (ls ls))
                   (declare (type fixnum i) (type (or (cons simple-string *) null) ls))
                   (if (null ls)
                       (the simple-string str)
                       (progn (replace str (car ls) :start1 i)
                              (rec (length (car ls)) (cdr ls))))))))
      (skip-whitespace))))

;;;
;;; binary-heap
;;;
(defpackage :binary-heap
  (:use :cl :utility)
  (:export :make-binary-heap
           :binary-heap-push
           :binary-heap-pop
           :binary-heap-peek
           :binary-heap-null-p
           ))
(in-package :binary-heap)

(declaim (inline binary-heap-push
                 binary-heap-pop
                 binary-heap-peek
                 binary-heap-null-p
                 ))

(defstruct binary-heap (data (make-array 256 :adjustable t :fill-pointer 0)) (comp #'<))

(defun up-heap (data comp index)
  (unless (zerop index)
          (let ((parent (half (1- index))))
            (when (funcall comp (aref data index) (aref data parent))
                  (swap (aref data index) (aref data parent))
                  (up-heap data comp parent)))))

(defun down-heap (data comp index)
  (let ((left (1+ (* index 2)))
        (right (+ 2 (* index 2))))
    (cond ((>= left (length data)) nil)
          ((>= right (length data))
            (when (funcall comp (aref data left) (aref data index))
                  (swap (aref data left) (aref data index))
                  (down-heap data comp left)))
          (t (let ((child (if (funcall comp (aref data left) (aref data right))
                              left
                              right)))
               (when (funcall comp (aref data child) (aref data index))
                     (swap (aref data child) (aref data index))
                     (down-heap data comp child)))))))

(defun binary-heap-push (heap x)
  (vector-push-extend x (binary-heap-data heap))
  (up-heap (binary-heap-data heap)
           (binary-heap-comp heap)
           (1- (length (binary-heap-data heap)))))

(defun binary-heap-pop (heap)
  (swap (aref (binary-heap-data heap) 0)
        (aref (binary-heap-data heap) (1- (length (binary-heap-data heap)))))
  (let ((value (vector-pop (binary-heap-data heap))))
    (down-heap (binary-heap-data heap)
               (binary-heap-comp heap)
               0)
    value))

(defun binary-heap-peek (heap)
  (aref (binary-heap-data heap) 0))

(defun binary-heap-null-p (heap)
  (zerop (length (binary-heap-data heap))))

;;;
;;; graph 
;;;
(defpackage :graph
  (:use :cl
        :utility
        :binary-heap
        )
  (:export :dijkstra
           ))
(in-package :graph)

(defun dijkstra (edges start goal)
  (let* ((n (length edges))
         (distances (make-array n :initial-element most-positive-fixnum))
         (used (make-array n :initial-element nil))
         (heap (make-binary-heap :comp (lambda (a b) (< (car a) (car b))))))
    (loop never (binary-heap-null-p heap)
          initially (setf (aref distances start) 0)
                    (binary-heap-push heap (cons 0 start))
          do (let* ((top (binary-heap-pop heap))
                    (d (car top))
                    (node (cdr top)))
               (cond ((= node goal) (return))
                     ((aref used node) nil)
                     (t (setf (aref used node) t)
                        (loop for edge across (aref edges node)
                              do (let* ((next (car edge))
                                        (cost (cdr edge)))
                                   (when (< (+ d cost) (aref distances next))
                                         (setf (aref distances next) (+ d cost))
                                         (binary-heap-push heap (cons (+ d cost) next)))))))))
    distances))

;;;
;;; atcoder
;;;
(defpackage :atcoder
  (:use :cl-proconio
        :cl-proconio/marker/core
        :utility
        :graph
        :cl 
        ))
(in-package :atcoder)

(declaim (inline sort))

(defconstant +inf+ (floor most-positive-fixnum 2))
(defvar *dydx* '((1 . 0) (0 . 1) (-1 . 0) (0 . -1)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmarker :string reader (ty)
    (declare (ignore ty))
    '(the simple-string (read-ascii-string *standard-input*))))

(declaim (ftype (function (fixnum fixnum (function (fixnum) t))
                          fixnum)
                meguru-method)
         (inline meguru-method))
(defun meguru-method (ok ng pred)
  (if (= (abs (- ok ng)) 1)
      ok
      (let ((m (ash (+ ok ng) -1)))
        (if (funcall pred m)
            (meguru-method m ng pred)
            (meguru-method ok m pred)))))

(declaim (ftype (function (fixnum fixnum)
                          (values fixnum fixnum))
                exgcd)
         (inline exgcd))
(defun exgcd (a b)
  (labels ((rec (a b x y nx ny)
             (multiple-value-bind (q r) (floor a b)
               (if (zerop r)
                   (values nx ny)
                   (rec b
                        r
                        nx
                        ny
                        (- x (the fixnum (* q nx)))
                        (- y (the fixnum (* q ny))))))))
    (declare (ftype (function (fixnum fixnum fixnum fixnum fixnum fixnum)
                              (values fixnum fixnum))
                    rec))
    (rec a b 1 0 0 1)))

(declaim (ftype (function (fixnum fixnum fixnum)
                          (values (or fixnum null) (or fixnum null)))
                bezout)
         (inline bezout))
(defun bezout (a b d)
  (let ((g (gcd a b)))
    (multiple-value-bind (k m) (floor d g)
      (if (not (zerop m))
          nil
          (multiple-value-bind (x y) (exgcd a b)
            (values (* x k) (* y k)))))))

(defconstant +MAX-USE+ (the fixnum 9999))

(defun solve (n a b c)
  (loop for i from 0 to +MAX-USE+
        minimize (loop for j from 0 to (- +MAX-USE+ i)
                       minimize (let ((nn (- n (+ (* a i)
                                                  (* b j)))))
                                  (if (< nn 0)
                                      +inf+
                                      (multiple-value-bind (q r) (floor nn c)
                                        (if (not (zerop r))
                                            +inf+
                                            (+ i j q))))))))

(defun main ()
  (input* ((n :fixnum)
           (a :fixnum)
           (b :fixnum)
           (c :fixnum))
    (format t "~D~%" (solve n a b c)))
  0)

#-swank (main)

(defun test ()
  (macrolet ((%test-case (input)
               `(with-input-from-string (*standard-input* ,input)
                 (main))))
    (%test-case "227
21 47 56")
    (%test-case "9999
1 5 10")
    (%test-case "998244353
314159 265358 97932")
    (%test-case "100000000
10001 10002 10003")))

