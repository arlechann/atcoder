(in-package :cl-user)

;;;
;;; utility
;;;
(defpackage :utility
  (:use :cl)
  (:export nlet
           swap
           div
           diff
           half
           read-ascii-char
           read-ascii-string
           ))
(in-package :utility)

(declaim (inline div half))

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
  "char code 9: Horizontal Tab
   char code 10: Line feed
   char code 11: Vertical Tab
   char code 12: Form feed
   char code 13: Carriage return"
  (let ((code (char-code c)))
    (declare (type fixnum code))
    (or (<= 9 code 13) (= code 32))))

(declaim (inline read-ascii-char))
(defun read-ascii-char (&optional (stream *standard-input*) (eof-error-p t) eof-value)
  #+swank (the standard-char (read-char stream eof-error-p eof-value))
  #-swank (the standard-char (code-char (read-byte stream eof-error-p eof-value))))

(let* ((buf-size 65536)
       (buf (make-array buf-size :element-type 'standard-char :initial-element #\.)))
  (defun read-ascii-string (&optional (steram *standard-input*))
    "Read a word of ascii char string"
    (labels ((skip-char-p (c)
               (declare (type (or standard-char null) c))
               (or (null c) (whitespacep c)))
             (skip-whitespace ()
               (let ((c (read-ascii-char nil nil)))
                 (declare (type standard-char c))
                 (if (skip-char-p c)
                     (skip-whitespace)
                     (write-buf c 0 nil))))
             (write-buf (c index acc)
               (declare (type standard-char c)
                        (type fixnum index)
                        (type (or (cons simple-string *) null) acc))
               (cond ((skip-char-p c)
                       (concat-list (nreverse (cons (copy-buf index) acc))))
                     ((= index buf-size)
                       (write-buf c 0 (cons (copy-buf buf-size) acc)))
                     (t (progn (setf (aref buf index) c)
                               (write-buf (read-ascii-char nil nil) (1+ index) acc)))))
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
;;; cl-proconio
;;;
(defpackage :cl-proconio
  (:use :cl
        :utility
        )
  (:export :input*))
(in-package :cl-proconio)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun input-primitive-type-specifier (prim)
    (cond ((or (null prim) (eq prim :*)) '*)
          ((eq prim :fixnum) 'fixnum)
          ((eq prim :fixnum1) 'fixnum)
          ((eq prim :float) 'float)
          ((eq prim :string) 'string)
          (t (error (format nil "input-primitive-type-specifier unimplement type: ~A" prim))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun input-primitive (prim)
    (cond ((or (null prim) (eq prim :*)) '(read))
          ((eq prim :fixnum) '(the fixnum (read)))
          ((eq prim :fixnum1) '(the fixnum (1- (read))))
          ((eq prim :float) '(the float (read)))
          ((eq prim :string) '(the string (read-ascii-string)))
          (t (error (format nil "input-primitive unimplement type: ~A" prim))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun input-vector-type-specifier (ty)
    (let ((elem (cadr ty))
          (size (caddr ty)))
      (list 'vector (input-type-specifier elem) size))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun input-vector (ty)
    (let ((elem (cadr ty))
          (size (caddr ty)))
      `(let ((vec (make-array ,size :element-type ',(input-type-specifier elem))))
        (loop for i from 0 below ,size
              do (setf (aref vec i) ,(input-type elem))
              finally (return vec))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun input-list-type-specifier (ty)
    (labels ((ts (ty)
               (if (null ty)
                   'null
                   (list 'cons (input-type-specifier (car ty)) (ts (cdr ty))))))
      (ts (cdr ty)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun input-list (ty)
    (nlet rec ((ls (cdr ty)) (acc nil))
      (if (null ls)
          (cons 'list (nreverse acc))
          (rec (cdr ls) (cons (input-type (car ls)) acc))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun input-type-specifier (ty)
    (cond ((atom ty) (input-primitive-type-specifier ty))
          ((eq (car ty) :vector) (input-vector-type-specifier ty))
          ((eq (car ty) :list) (input-list-type-specifier ty))
          (t (error (format nil "input-type-specifier unimplement type: ~A" ty))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun input-type (ty)
    (cond ((atom ty) (input-primitive ty))
          ((eq (car ty) :vector) (input-vector ty))
          ((eq (car ty) :list) (input-list ty))
          (t (error (format nil "input-type unimplement type: ~A" ty))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun input-form (form)
    (if (null form)
        nil
        (mapcar (lambda (ty)
                  (list (car ty)
                        (input-type (cadr ty))))
                form))))

(defmacro input* (form &body body)
  `(let* ,(input-form form) ,@body))

;;;
;;; atcoder
;;;
(defpackage :atcoder
  (:use :cl
        :utility
        :graph
        :cl-proconio
        ))
(in-package :atcoder)

(declaim (inline sort))

(defun solve (n a b)
  (setf a (sort a #'<)
        b (sort b #'<))
  (nlet rec ((i 0) (result 0))
    (if (< i n)
        (rec (1+ i) (+ result (diff (aref a i) (aref b i))))
        result)))

(defun main ()
  (declaim (optimize (speed 3) (safety 0) (debug 0)))
  (input* ((n :fixnum)
           (a (:vector :fixnum n))
           (b (:vector :fixnum n)))
    (let ((result (solve n a b)))
      (format t "~A~%" result))))

#-swank (main)

(defun test ()
  (let ((samples (list "1
869
120
" "6
8 6 9 1 2 0
1 5 7 2 3 9
" "10
31 41 59 26 53 58 97 93 23 84
17 32 5 8 7 56 88 77 29 35
" "20
804289382 846930886 681692776 714636914 957747792 424238335 719885386 649760491 596516649 189641420 25202361 350490026 783368690 102520058 44897761 967513925 365180539 540383425 304089172 303455735
35005211 521595368 294702567 726956428 336465782 861021530 278722862 233665123 145174065 468703135 101513928 801979801 315634021 635723058 369133068 125898166 59961392 89018454 628175011 656478041
")))
    (dolist (sample samples)
      (with-input-from-string (*standard-input* sample)
        (main)))))
