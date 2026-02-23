;;; utility.base
;;;
(defpackage utility.base
  (:use :cl :utility.syntax :utility.number)
  (:import-from :uiop
                :split-string
                :emptyp
                :string-prefix-p
                :string-suffix-p
                :strcat
                :println)
  (:export ;; function / memoize
           :do-nothing
           :compose
           :memoize-lambda
           :array-memoize-lambda
           ;; lazy
           :delay
           :force
           ;; sequence
           :sum
           :sortf
           :map-with-index
           :map-into-with-index
           :nmap
           :nmap-with-index
           :run-length-encode
           :next-permutation
           :do-permutations
           :emptyp
           ;; list
           :ensure-car
           :ensure-list
           :xcons
           :mapc-with-index
           :mapcar-with-index
           :mapcan-with-index
           :mapl-with-index
           :maplist-with-index
           :mapcon-with-index
           :length-n-p
           :length=
           :length<=
           :length<
           :singlep
           :last1
           :mklist
           :take
           :drop
           :longerp
           :longer
           :iota
           :reverse-nconc
           :unfold
           :unique
           :chunks
           :permutations
           :flatten
           :join
           ;; string / char
           :split-string
           :strjoin
           :trim-spaces
           :string-prefix-p
           :string-suffix-p
           :strcat
           :count-chars
           :count-alphabet
           :lower-to-index
           :upper-to-index
           :char-to-index
           :index-to-lower
           :index-to-upper
           :char-digit
           ;; io
           :println
           :print-boolean
           :print-double
           :print-sequence
           ))
(in-package utility.base)

;;; function

(defun do-nothing (&rest args) (declare (ignore args)))
(defun compose (&rest fns) (lambda (x) (reduce #'funcall fns :initial-value x :from-end t)))

(defmacro memoize-lambda (args &body body)
  (let ((memo (gensym))
        (original-args (gensym))
        (none-value (gensym))
        (memo-value (gensym)))
    `(let ((,memo (make-hash-table :test #'equal)))
       (lambda (&rest ,original-args)
         (let ((,memo-value (gethash ,original-args ,memo ',none-value)))
           (if (not (eq ,memo-value ',none-value))
               ,memo-value
               (setf (gethash ,original-args ,memo)
                     (destructuring-bind ,args ,original-args
                       ,@body))))))))

(defmacro array-memoize-lambda (arg-and-maxs &body body)
  (let ((memo (gensym))
        (none-value (gensym))
        (memo-value (gensym))
        (args (mapcar #'car arg-and-maxs))
        (maxs (mapcar #'cadr arg-and-maxs)))
    `(let ((,memo (make-array (list ,@maxs) :initial-element ',none-value)))
       (lambda ,args
         (let ((,memo-value (aref ,memo ,@args)))
           (if (eq ,memo-value ',none-value)
               (setf (aref ,memo ,@args)
                     (progn ,@body))
               ,memo-value))))))

;;; sequence

(declaim (inline sort))

(declaim (ftype (function (sequence) number) sum))
(defun sum (seq) (reduce #'+ seq :initial-value 0))

(define-modify-macro sortf (compare &rest args)
  (lambda (sequence compare &rest args &key key)
    (declare (ignore key))
    (apply #'sort sequence compare args)))

(declaim (ftype (function ((or cons symbol class)
                           (or (function (unsigned-byte t &rest t) t) symbol)
                           sequence &rest sequence)
                          sequence)
                map-with-index))
(defun map-with-index (result-type fn sequence &rest more-sequences)
  (let ((index 0))
    (apply #'map
           result-type
           #'(lambda (&rest args) (prog1 (apply fn index args) (incf index)))
           sequence
           more-sequences)))

(declaim (ftype (function (sequence (or (function (unsigned-byte t &rest t) t) symbol)
                                    &rest sequence))
                map-into-with-index))
(defun map-into-with-index (result-sequence fn &rest sequences)
  (let ((index 0))
    (apply #'map-into
           result-sequence
           #'(lambda (&rest args)
               (prog1 (apply fn index args)
                 (incf index)))
           sequences)))

(declaim (ftype (function ((or (function (t &rest t) t) symbol)
                           sequence &rest sequence)
                          sequence)
                nmap))
(defun nmap (fn sequence &rest more-sequences)
  (apply #'map-into sequence fn sequence more-sequences))

(declaim (ftype (function ((or (function (unsigned-byte t &rest t) t) symbol)
                           sequence &rest sequence)
                          sequence)
                nmap-with-index))
(defun nmap-with-index (fn sequence &rest more-sequences)
  (let ((index 0))
    (apply #'map-into
           sequence
           (lambda (&rest args)
             (prog1 (apply fn index args)
               (incf index)))
           sequence
           more-sequences)))

(declaim (ftype (function (sequence &key (:test (or symbol (function (t t) t))))
                          list)
                run-length-encode))
(defun run-length-encode (sequence &key (test #'eql))
  (let ((prev '#.(gensym))
        (acc nil))
    (map nil
         (lambda (e)
           (if (funcall test e prev)
               (incf (cdar acc))
               (push (cons e 1) acc))
           (setf prev e))
         sequence)
    (nreverse acc)))

(declaim (ftype (function (sequence &key (:compare (function (t t) t)))
                          sequence)
                next-permutation))
(defun next-permutation (sequence &key (compare #'<))
  (let* ((item nil)
         (index nil))
    (let ((prev nil))
      (map-with-index nil
                      #'(lambda (i x)
                          (when (and prev (funcall compare prev x))
                            (setf item prev
                                  index (1- i)))
                          (setf prev x))
                      sequence))
    (when (null index)
      (return-from next-permutation nil))
    (rotatef (elt sequence index)
             (elt sequence (position-if #'(lambda (x) (funcall compare item x))
                                        sequence
                                        :from-end t)))
    (setf (subseq sequence (1+ index)) (nreverse (subseq sequence (1+ index))))
    sequence))

(defmacro do-permutations ((var vector &optional result) &body body)
  `(do ((,var (sort (copy-seq ,vector) #'<) (next-permutation ,var)))
       ((null ,var) ,result)
     (declare (ignorable ,var))
     ,@body))

;;; list

(declaim (inline ensure-car ensure-list xcons singlep last1 mklist))

(defun-always ensure-car (x) (if (consp x) (car x) x))
(defun-always ensure-list (x) (if (listp x) x (list x)))

(declaim (ftype (function (t t) cons) xcons))
(defun xcons (a b) (cons b a))

(macrolet ((%def-map-list-with-index (map-function-name)
             (let ((name (symb map-function-name '-with-index))
                   (fn (gensym "FN"))
                   (lst (gensym "LST"))
                   (more-lst (gensym "MORE-LST"))
                   (index (gensym))
                   (args (gensym)))
               `(progn
                  (declaim (ftype (function ((or (function (unsigned-byte t &rest t) t) symbol)
                                             list &rest list)
                                            list)
                                  ,name))
                  (defun ,name (,fn ,lst &rest ,more-lst)
                    (let ((,index 0))
                      (apply #',map-function-name
                             #'(lambda (&rest ,args)
                                 (prog1 (apply ,fn ,index ,args) (incf ,index)))
                             ,lst ,more-lst)))))))
  (%def-map-list-with-index mapc)
  (%def-map-list-with-index mapcar)
  (%def-map-list-with-index mapcan)
  (%def-map-list-with-index mapl)
  (%def-map-list-with-index maplist)
  (%def-map-list-with-index mapcon))

(declaim (ftype (function (list unsigned-byte) boolean) length-n-p))
(defun length-n-p (lst n)
  (nlet rec ((lst lst) (n n))
    (cond ((zerop n) (null lst))
          ((null lst) nil)
          (t (rec (cdr lst) (1- n))))))

(declaim (ftype (function (list unsigned-byte) boolean) length=))
(defun length= (lst n) (length-n-p lst n))

(declaim (ftype (function (list unsigned-byte) boolean) length<))
(defun length< (lst n)
  (nlet rec ((lst lst) (n n))
    (cond ((zerop n) nil)
          ((null lst) t)
          (t (rec (cdr lst) (1- n))))))

(declaim (ftype (function (list unsigned-byte) boolean) length<=))
(defun length<= (lst n)
  (nlet rec ((lst lst) (n n))
    (cond ((zerop n) (null lst))
          ((null lst) t)
          (t (rec (cdr lst) (1- n))))))

(declaim (ftype (function (list) boolean) singlep))
(defun singlep (lst) (and lst (null (cdr lst))))

(declaim (ftype (function (list) t) last1))
(defun last1 (lst) (car (last lst)))

(declaim (ftype (function (t) list) mklist))
(defun mklist (obj) (if (listp obj) obj (list obj)))

(declaim (ftype (function (list unsigned-byte) list) take))
(defun take (lst n)
  (nlet rec ((lst lst) (n n) (acc nil))
    (if (or (null lst) (zerop n))
        (nreverse acc)
        (rec (cdr lst) (1- n) (cons (car lst) acc)))))

(declaim (ftype (function (list unsigned-byte) list) drop))
(defun drop (lst n)
  (nlet rec ((lst lst) (n n))
    (if (or (null lst) (zerop n)) lst (rec (cdr lst) (1- n)))))

(declaim (ftype (function (list list) list) longerp))
(defun longerp (lst1 lst2)
  (nlet rec ((lst1 lst1) (lst2 lst2))
    (cond ((null lst1) nil)
          ((null lst2) lst1)
          (t (rec (cdr lst1) (cdr lst2))))))

(declaim (ftype (function (list list) list) longer))  
(defun longer (lst1 lst2) (if (longerp lst1 lst2) lst1 lst2))

(declaim (ftype (function (list t) t) reverse-nconc))
(defun reverse-nconc (lst tail)
  (nlet rec ((lst lst) (tail tail))
    (if (null lst)
        tail
        (rec (cdr lst) (rplacd lst tail)))))

(declaim (ftype (function (unsigned-byte &key (:start number) (:step number)) list) iota))
(defun iota (n &key (start 0) (step 1))
  (nlet rec ((n n) (start start) (acc nil))
    (if (zerop n)
        (nreverse acc)
        (rec (1- n) (+ start step) (cons start acc)))))

(declaim (ftype (function ((function (t) t) (function (t) t) (function (t) t) t
                                            &optional (function (t) t))
                          list)
                unfold))
(defun unfold (predicate fn next-generator seed &optional tail)
  (nlet rec ((seed seed) (acc nil))
    (if (funcall predicate seed)
        (if (null tail)
            (nreverse acc)
            (reverse-nconc acc (funcall tail seed)))
        (rec (funcall next-generator seed) (cons (funcall fn seed) acc)))))

(declaim (ftype (function (list &key (:test (function (t t) t))) list) unique))
(defun unique (lst &key (test #'eql))
  (nreverse (reduce (lambda (acc x)  (if (funcall test x (car acc)) acc (cons x acc))) lst
                    :initial-value nil)))

(declaim (ftype (function (list (integer 1 *) &key (:fractionp t)) list) chunks))
(defun chunks (lst size &key (fractionp t))
  (nlet outer ((lst lst) (acc nil))
    (if (null lst)
        (nreverse acc)
        (nlet inner ((lst lst) (rest size) (chunk nil) (acc acc))
          (cond ((zerop rest) (outer lst (cons (nreverse chunk) acc)))
                ((null lst) (outer nil (if fractionp (cons (nreverse chunk) acc) acc)))
                (t (inner (cdr lst) (1- rest) (cons (car lst) chunk) acc)))))))

(declaim (ftype (function (list) list) permutations))
(defun permutations (lst)
  (let ((ret nil))
    (labels ((rec (lst acc)
               (if (null lst)
                   (push acc ret)
                   (dolist (item lst)
                     (rec (remove-if (lambda (x) (eql x (car item)))
                                     lst :key #'car)
                          (cons (cdr item) acc))))))
      (rec (mapcar-with-index #'cons lst) nil))
    ret))

(declaim (ftype (function (list) list) flatten))
(defun flatten (lst)
  (nreverse (named-let rec ((lst lst) (acc nil))
              (cond ((null lst) acc)
                    ((listp (car lst)) (rec (cdr lst) (rec (car lst) acc)))
                    (t (rec (cdr lst) (cons (car lst) acc)))))))

(declaim (ftype (function (list t) list) join))
(defun join (lst separator)
  (when (null lst)
    (return-from join nil))
  (nlet rec ((lst (cdr lst)) (acc (list (car lst))))
    (if (null lst)
        (nreverse acc)
        (rec (cdr lst) (cons (car lst) (cons separator acc))))))

;;; vector

(declaim (ftype (function (&rest list) vector) dvector))
(defun dvector (&rest contents)
  (make-array (length contents)
              :initial-contents contents
              :adjustable t
              :fill-pointer t))

(declaim (ftype (function (vector &key (:start unsigned-byte) (:end unsigned-byte))
                          vector)
                subvec/shared))
(defun subvec/shared (vector &key (start 0) end)
  (make-array (- (or end (length vector)) start)
              :element-type (array-element-type vector)
              :displaced-to vector
              :displaced-index-offset start))

;;; char

(declaim (inline count-alphabet lower-to-index upper-to-index char-to-index index-to-lower index-to-upper char-digit))

(defun count-alphabet () #.(1+ (- (char-code #\Z) (char-code #\A))))
(defun lower-to-index (char) (- (char-code char) #.(char-code #\a)))
(defun upper-to-index (char) (- (char-code char) #.(char-code #\A)))
(defun char-to-index (char) (if (char< char #\a) (upper-to-index char) (lower-to-index char)))
(defun index-to-lower (index) (code-char (+ index #.(char-code #\a))))
(defun index-to-upper (index) (code-char (+ index #.(char-code #\A))))
(defun char-digit (char) (- (char-code char) #.(char-code #\0)))

;;; string

(defun strjoin (strings &key (spacer (string #\Newline)))
  (with-output-to-string (out)
    (dolist (item (join strings spacer))
      (write-string item out))))

(defun trim-spaces (string)
  (string-trim '(#\Space #\Tab #\Newline #\Return) string))

(defun count-chars (string)
  (loop with count = (make-array (count-alphabet) :initial-element 0)
        for c across string
        do (incf (aref count (char-to-index c)))
        finally (return count)))

;;; io

(defun print-boolean (boolean &optional (stream *standard-output*))
  (write-line (if boolean "Yes" "No") stream)
  (values))

(defun print-double (double &optional (stream *standard-output*))
  (let ((*read-default-float-format* 'double-float))
    (princ double stream)
    (terpri stream)
    (values)))

(defun print-sequence (sequence
                       &optional (stream *standard-output*)
                       &key (element-type 'base-char) (spacer #\ ))
  #+sbcl (declare (sb-ext:muffle-conditions style-warning))
  (write-string
   (with-output-to-string (s nil :element-type element-type)
     (let ((headp t))
       (do-seq (elem sequence)
         (unless headp
           (write-char spacer s))
         (princ elem s)
         (setf headp nil))
       (terpri s)))
   stream)
  (values))

;;; lazy

(defstruct promise (value nil) thunk)

(defmacro delay (expr) `(make-promise :thunk (lambda () ,expr)))

(defun force (ps)
  (when (promise-thunk ps)
    (setf (promise-value ps) (funcall (promise-thunk ps))
          (promise-thunk ps) nil))
  (promise-value ps))

;;;
