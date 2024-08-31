(in-package :cl-user)

#-swank
(unless (member :child-sbcl *features*)
  (quit :recklessly-p t
        :unix-status
        (process-exit-code
         (run-program *runtime-pathname*
                      `("--control-stack-size" "256MB" "--noinform" "--disable-ldb"
                        "--lose-on-corruption" "--end-runtime-options" "--eval"
                        "(push :child-sbcl *features*)" "--script" ,(namestring *load-pathname*))
                      :output t :error t :input t))))

;;;
;;; utility
;;;
(defpackage utility
  (:use :cl)
  (:import-from :uiop
                :split-string
                :if-let
                :emptyp
                :string-prefix-p
                :string-suffix-p
                :strcat
                :println)
  (:export :it
           :self
           ;; definition
           :defun-always
           :defalias
           ;; control
           :eval-always
           :nlet
           :alambda
           :if-let
           :if-let*
           :when-let
           :when-let*
           :aif
           :do-array
           :do-array*
           :do-seq
           :do-seq*
           :do-combination
           :do-popbit
           :do-neighbors
           :let-dyn
           ;; number
           :2*
           :/2
           :square
           :cube
           :pow
           :diff
           :next-pow2
           :repunit
           :maxp
           :minp
           :maxf
           :minf
           :logipop
           :logmsb
           :range-intersect-p
           ;; function
           :do-nothing
           :compose
           :memoize-lambda
           :array-memoize-lambda
           ;; sequence
           :sum
           :sortf
           :emptyp
           :make-iterator
           :copy-iterator
           :iterator-next
           :iterator-endp
           :iterator-element
           :iterator-index
           ;; list
           :ensure-car
           :ensure-list
           :length-n-p
           :singlep
           :take
           :drop
           :longerp
           :longer
           :iota
           :unfold
           :unique
           :chunk
           :with-index
           :permutation
           :flatten
           ;; vector
           :dvector
           :next-permutation
           :do-permutations
           ;; string
           :split-string
           :string-prefix-p
           :string-suffix-p
           :strcat
           ;; char
           :count-alphabet
           :char-to-index
           ;; io
           :println
           ;; lazy
           :delay
           :force
           ;; symbol
           :symb
           ))
(in-package utility)

(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

;;; definition

(defmacro defun-always (name params &body body) `(eval-always (defun ,name ,params ,@body)))

(defmacro defalias (alias original)
  `(progn (setf (symbol-function ,alias) ,original)
          ,alias))

;;; reader macro

(defun-always def-dispatch-fn (char fn)
  (set-dispatch-macro-character #\# char
                                (lambda (stream char1 char2)
                                  (declare (ignorable stream char1 char2))
                                  (funcall fn (read stream t nil t)))))

(defmacro def-dispatch-macro (char params &body body)
  `(eval-always (def-dispatch-fn ,char (lambda ,params ,@body))))

(def-dispatch-macro #\? (expr)
  (let ((value (gensym)))
    `(let ((,value ,expr))
       (fresh-line *error-output*)
       (format *error-output* "DEBUG PRINT: ~S => ~S~%" ',expr ,value)
       ,value)))

(eval-always
  (let ((rpar (get-macro-character #\) )))
    (defun def-delimiter-macro-fn (left right fn)
      (set-macro-character right rpar)
      (set-dispatch-macro-character #\# left
                                    (lambda (stream char1 char2)
                                      (declare (ignorable stream char1 char2))
                                      (apply fn
                                             (read-delimited-list right stream t)))))))

(defmacro def-delimiter-macro (left right params &body body)
  `(eval-always
     (def-delimiter-macro-fn ,left ,right #'(lambda ,params ,@body))))

(def-delimiter-macro #\[ #\] (arr &rest indecies) `(aref ,arr ,@indecies))

;;; control

(defmacro nlet (name binds &body body)
  `(labels ((,name ,(mapcar #'car binds) ,@body))
     (,name ,@(mapcar #'cadr binds))))

(defmacro alambda (params &body body) `(labels ((self ,params ,@body)) #'self))

(defmacro if-let* (binds then &optional else)
  `(let* ,binds
     (if (and ,@(mapcar #'car binds)) ,then ,else)))

(defmacro when-let (binds &body body)
  `(let ,binds
     (when (and ,@(mapcar #'car binds))
       ,@body)))

(defmacro when-let* (binds &body body)
  `(let* ,binds
     (when (and ,@(mapcar #'car binds))
       ,@body)))

(defmacro aif (test then &optional else) `(let ((it ,test)) (if it ,then ,else)))

(defmacro do-array* (((&rest vars) (&rest arrays) &optional result) &body body)
  (let ((arrs (mapcar (lambda (x)
                        (declare (ignore x))
                        (gensym))
                      arrays))
        (index (gensym))
        (min-size (gensym)))
  `(let* (,@(mapcar #'list arrs arrays)
          (,min-size (apply #'min (mapcar #'array-total-size (list ,@arrs)))))
     (do ((,index 0 (1+ ,index)))
         ((= ,index ,min-size) ,result)
       (let ,(mapcar (lambda (var array-var)
                        `(,var (row-major-aref ,array-var ,index)))
                      vars arrs)
         (declare (ignorable ,@vars))
         ,@body)))))
       
(defmacro do-array ((var array &optional result) &body body)
  `(do-array* ((,var) (,array) ,result) ,@body))

(defmacro do-seq* (((&rest vars) (&rest sequences) &optional result) &body body)
  (let ((seqs (mapcar (lambda (x)
                        (declare (ignore x))
                        (gensym))
                      sequences))
        (iters (mapcar (lambda (x)
                         (declare (ignore x))
                         (gensym))
                       sequences)))
  `(let* (,@(mapcar #'list seqs sequences))
     (do ,(mapcar (lambda (seq iter)
                    `(,iter (make-iterator ,seq) (iterator-next ,iter)))
                   seqs iters)
         ((some #'iterator-endp (list ,@iters)) ,result)
       (let ,(mapcar (lambda (var iter)
                        `(,var (iterator-element ,iter)))
                      vars iters)
         (declare (ignorable ,@vars))
         ,@body)))))

(defmacro do-seq ((var sequence &optional result) &body body)
  `(do-seq* ((,var) (,sequence) ,result) ,@body))

(defmacro do-combination (((&rest vars) (&rest counts) &optional result) &body body)
  (cond ((or (null vars) (null counts)) result)
        ((or (null (cdr vars)) (null (cdr counts)))
         `(dotimes (,(car vars) ,(car counts) ,result)
            ,@body))
        (t `(dotimes (,(car vars) ,(car counts) ,result)
              (do-combination (,(cdr vars) ,(cdr counts) ,result)
                ,@body)))))

(defmacro do-popbit ((var integer &optional result) &body body)
  (let ((int (gensym)))
    `(loop with ,int = ,integer
           and ,var = 0
           until (or (zerop ,int)
                     (= ,int -1))
           when (logbitp 0 ,int)
             do (progn ,@body)
           do (setf ,int (ash ,int -1))
              (incf ,var)
           finally (return ,result))))

(defmacro do-neighbors (((var-y var-x) (point-y point-x) &optional result) &body body)
  (let ((name (gensym "DO-NEIGHBORS"))
        (dy (gensym))
        (dx (gensym)))
    `(loop named ,name
           for ,dy in '(0 1 0 -1)
           for ,dx in '(1 0 -1 0)
           do (let ((,var-y (+ ,dy ,point-y))
                    (,var-x (+ ,dx ,point-x)))
                ,@body)
           finally (return-from ,name ,result))))

(defmacro let-dyn (binds &body body)
  `(let ,binds
     (declare (dynamic-extent
               ,@(mapcar (lambda (x) (if (listp x) (car x) x)) binds)))
     ,@body))

;;; number

(defun 2* (x) (* x 2))
(defun /2 (x) (values (floor x 2)))
(defun square (x &optional (op #'*)) (funcall op x x))
(defun cube (x &optional (op #'*)) (funcall op x x x))

(defun pow (base power &key (op #'*) (identity 1))
  (let ((ret identity))
    (loop until (zerop power)
          when (oddp power)
            do (setf ret (funcall op ret base))
          do (setf base (square base op)
                   power (floor power 2))
          finally (return ret))))

(defun diff (a b) (abs (- a b)))

(defun next-pow2 (n)
  (when (zerop (logand n (1- n)))
    (return-from next-pow2 n))
  (loop with acc = 1
        while (> n 0)
        do (setf n (ash n -1)
                 acc (ash acc 1))
        finally (return acc)))

(defun repunit (n &optional (base 10))
  (let ((acc 0))
    (dotimes (i n acc)
      (setf acc (+ (* acc base) 1)))))

(defun maxp (x &rest args) (> x (apply #'max args)))
(defun minp (x &rest args) (< x (apply #'min args)))
(defmacro maxf (place &rest args) `(setf ,place (max ,place ,@args)))
(defmacro minf (place &rest args) `(setf ,place (min ,place ,@args)))
(defun logipop (n &rest indexes) (dolist (index indexes n) (setf n (logior n (ash 1 index)))))

(defun logmsb (n)
  (loop for i from 0
        when (= (ash 1 i) n)
          return i
        when (> (ash 1 i) n)
          return (1- i)))

(defun range-intersect-p (a1 a2 b1 b2 &key touchp)
  (let ((comp (if touchp #'< #'<=)))
    (not (or (funcall comp (max a1 a2) (min b1 b2))
             (funcall comp (max b1 b2) (min a1 a2))))))

;;; function

(defun do-nothing (&rest args) (declare (ignore args)))
(defun compose (&rest fns) (lambda (x) (reduce #'funcall fns :initial-value x :from-end t)))

(defmacro memoize-lambda (args &body body)
  (let ((memo (gensym))
        (original-args (gensym)))
    `(let ((,memo (make-hash-table :test #'equal)))
       (lambda (&rest ,original-args)
         (multiple-value-bind (memo-value found-p) (gethash ,original-args ,memo nil)
           (if found-p
               memo-value
               (setf (gethash ,original-args ,memo)
                     (destructuring-bind ,args ,original-args
                       ,@body))))))))

(defmacro array-memoize-lambda (arg-and-maxs &body body)
  (let ((memo (gensym))
        (args (mapcar #'car arg-and-maxs))
        (maxs (mapcar #'cadr arg-and-maxs)))
    `(let ((,memo (make-array (list ,@maxs) :initial-element nil)))
       (lambda ,args
         (or (aref ,memo ,@args)
             (setf (aref ,memo ,@args)
                   (progn ,@body)))))))

;;; sequence

(defun sum (seq) (reduce #'+ seq :initial-value 0))
(defmacro sortf (seq-place pred &rest args) `(setf ,seq-place (sort ,seq-place ,pred ,@args)))

(defstruct (%iterator-method
            (:constructor %make-iterator-method (step-fn endp-fn elm-fn
                                                 setf-elm-fn index-fn copy-fn)))
  step-fn endp-fn elm-fn setf-elm-fn index-fn copy-fn)

(defstruct (iterator (:constructor %make-iterator (&key sequence internal limit
                                                     from-end method))
                     (:copier %copy-iterator))
  sequence internal limit from-end method)

(defun make-iterator (seq &key (from-end nil))
  #-sbcl (error "Not supported implementation.")
  (multiple-value-bind (iter limit from-end step-fn endp-fn
                        elm-fn setf-elm-fn index-fn copy-fn)
      (sb-sequence:make-sequence-iterator seq :from-end from-end)
    (%make-iterator :sequence seq
                    :internal iter
                    :limit limit
                    :from-end from-end
                    :method (%make-iterator-method
                             step-fn endp-fn elm-fn
                             setf-elm-fn index-fn copy-fn))))

(defun iterator-rev-p (iter)
  (iterator-from-end iter))

(defun iterator-next (iter)
  (let ((next-iter (%copy-iterator iter)))
    (setf (iterator-internal next-iter)
          (funcall (%iterator-method-step-fn (iterator-method iter))
                   (iterator-sequence iter)
                   (iterator-internal iter)
                   (iterator-from-end iter)))
    next-iter))

(defun iterator-endp (iter)
  (funcall (%iterator-method-endp-fn (iterator-method iter))
           (iterator-sequence iter)
           (iterator-internal iter)
           (iterator-limit iter)
           (iterator-from-end iter)))

(defun iterator-element (iter)
  (funcall (%iterator-method-elm-fn (iterator-method iter))
           (iterator-sequence iter)
           (iterator-internal iter)))

(defun (setf iterator-element) (value iter)
  (funcall (%iterator-method-setf-elm-fn (iterator-method iter))
           value
           (iterator-sequence iter)
           (iterator-internal iter)))

(defun iterator-index (iter)
  (funcall (%iterator-method-index-fn (iterator-method iter))
           (iterator-sequence iter)
           (iterator-internal iter)))

(defun copy-iterator (iter)
  (let ((copied-iter (%copy-iterator iter)))
    (setf (iterator-internal copied-iter)
          (funcall (%iterator-method-copy-fn (iterator-method iter))
                   (iterator-sequence iter)
                   (iterator-internal iter)))
    copied-iter))

;;; list

(defun-always ensure-car (x) (if (consp x) (car x) x))
(defun-always ensure-list (x) (if (listp x) x (list x)))

(defun length-n-p (lst n)
  (loop when (zerop n)
          return (null lst)
        when (null lst)
          return nil
        do (setf lst (cdr lst)
                 n (1- n))))

(defun singlep (lst) (and lst (null (cdr lst))))

(defun take (lst len)
  (let ((acc nil))
    (dotimes (i len (nreverse acc))
      (push (car lst) acc)
      (setf lst (cdr lst)))))

(defun drop (lst len) (dotimes (i len lst) (setf lst (cdr lst))))

(defun longerp (lst1 lst2)
  (loop when (null lst1)
          return nil
        when (null lst2)
          return t
        do (setf lst1 (cdr lst1)
                 lst2 (cdr lst2))))
  
(defun longer (lst1 lst2) (if (longerp lst1 lst2) lst1 lst2))

(defun iota (count &key (start 0) (step 1))
  (let ((acc nil))
    (dotimes (i count (nreverse acc))
      (push start acc)
      (setf start (+ start step)))))

(defun unfold (pred fn next-gen seed)
  (loop with acc = nil
        until (funcall pred next-gen)
        do (push (funcall fn seed) acc)
           (setf seed (funcall next-gen seed))
        finally (return (nreverse acc))))

(defun unique (lst &key (test #'eql))
  (nreverse (reduce (lambda (acc x)
                      (if (funcall test x (car acc))
                          acc
                          (cons x acc)))
                    lst
                    :initial-value nil)))

(defun chunk (lst size &key (fractionp t))
  (loop with lst = lst
        and chunk = nil
        and chunk-size = 0
        and acc = nil
        until (null lst)
        if (= chunk-size size)
          do (psetq chunk nil
                    chunk-size 0
                    acc (cons (nreverse chunk) acc))
        else
          do (psetq lst (cdr lst)
                    chunk (cons (car lst) chunk)
                    chunk-size (1+ chunk-size))
        finally (return (nreverse (if fractionp
                                      (cons (nreverse chunk) acc)
                                      acc)))))

(defun with-index (lst)
  (loop with acc = nil
        for x in lst
        for i from 0
        do (push (cons i x) acc)
        finally (return (nreverse acc))))

(defun permutation (lst)
  (let ((ret nil))
    (labels ((rec (lst acc)
               (if (null lst)
                   (push acc ret)
                   (dolist (item lst)
                     (rec (remove-if (lambda (x)
                                       (eql (car x) (car item)))
                                     lst)
                          (cons (cdr item) acc))))))
      (rec (with-index lst) nil))
    ret))

(defun flatten (lst)
  (let ((acc nil))
    (labels ((rec (lst)
               (dolist (obj lst)
                 (if (listp obj)
                     (rec obj)
                     (push obj acc)))))
      (rec lst))
    (nreverse acc)))

;;; vector

(defun dvector (&rest contents)
  (make-array (length contents)
              :initial-contents contents
              :adjustable t
              :fill-pointer t))

(defun next-permutation (vector &key (compare #'<))
  (let ((i (loop for i downfrom (- (length vector) 2) downto 0
                 when (funcall compare (aref vector i) (aref vector (1+ i)))
                   do (return i)
                 when (zerop i)
                   do (return-from next-permutation nil)
                 finally (return-from next-permutation nil))))
    (let ((j (position-if (lambda (x) (funcall compare (aref vector i) x)) vector :from-end t)))
      (rotatef (aref vector i) (aref vector j))
      (setf (subseq vector (1+ i)) (nreverse (subseq vector (1+ i))))
      vector)))

(defmacro do-permutations ((var vector &optional result) &body body)
  `(do ((,var (sort (copy-seq ,vector) #'<) (next-permutation ,var)))
       ((null ,var) ,result)
     (declare (ignorable ,var))
     ,@body))

;;; char

(defun count-alphabet () #.(1+ (- (char-code #\Z) (char-code #\A))))
(defun lower-to-index (char) (- (char-code char) #.(char-code #\a)))
(defun upper-to-index (char) (- (char-code char) #.(char-code #\A)))
(defun char-to-index (char) (if (char< char #\a) (upper-to-index char) (lower-to-index char)))

;;; lazy

(defstruct promise (value nil) thunk)

(defmacro delay (expr) `(make-promise :thunk (lambda () ,expr)))

(defun force (ps)
  (when (promise-thunk ps)
    (setf (promise-value ps) (funcall (promise-thunk ps))
          (promise-thunk ps) nil))
  (promise-value ps))

;;; symbol

(defun-always symb (&rest args)
  (values (intern (with-output-to-string (s)
                    (mapcar (lambda (x) (princ x s)) args)))
           *package*))

;;;
;;; input
;;;
(defpackage :input
  (:use :cl :utility)
  (:export :input*
           :def-input-reader))
(in-package :input)

;; (input* ((n fixnum)
;;          (m fixnum1)
;;          (f double)
;;          (s string)
;;          (p (cons* fixnum fixnum nil))
;;          (l (list fixnum n))
;;          (v (vector fixnum m)))
;;   (list n m f s p l v))
;; 2 3 10.5
;; input
;; 1000000007 -1
;; 10 20
;; 30 40
;; => (2 2 10.5d0 "input" (1000000007 -1) (10 20) #(30 40))

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
  (let ((marker (input-typespec-marker typespec)))
    (if (null marker)
        nil
        (funcall (get-input-reader marker) typespec))))

(defun-always input-expand (forms)
  (if (null forms)
      nil
      (mapcar (lambda (form)
                (list (car form)
                      (input-typespec-reader (cadr form))))
              forms)))

(defmacro input* (forms &body body) `(let* ,(input-expand forms) ,@body))

(defmacro def-input-reader (marker params &body body)
  `(eval-always
     (set-input-reader ',marker
                       (macrolet ((reader (typespec)
                                    `(input-typespec-reader ,typespec)))
                         (lambda (,@params &optional arg)
                           (declare (ignore arg))
                           ,@body)))
     ',marker))

(def-input-reader fixnum () '(read))
(def-input-reader fixnum1 () '(1- (read)))
(def-input-reader double () '(let ((*read-default-float-format* 'double-float)) (read)))
(def-input-reader string () `(read-line))

(def-input-reader cons* (typespec)
  (let ((elems (cdr typespec)))
    (if (null (cdr elems))
        (reader (car elems))
        `(cons ,(reader (car elems))
               ,(reader (cons 'cons* (cdr elems)))))))

(def-input-reader nil () nil)

(def-input-reader list (typespec)
  (let ((elem (cadr typespec))
        (len (caddr typespec)))
    `(loop repeat ,len
           collect ,(reader elem))))

(def-input-reader vector (typespec)
  (let ((vec (gensym))
        (index (gensym))
        (elem (cadr typespec))
        (len (caddr typespec)))
    `(let ((,vec (make-array ,len)))
       (dotimes (,index ,len ,vec)
         (setf (aref ,vec ,index) ,(reader elem))))))

;;;
;;; deque
;;;
(defpackage deque
  (:use :cl :utility)
  (:export :make-deque
           :deque-size
           :deque-empty-p
           :deque-push-front
           :deque-push-back
           :deque-pop-front
           :deque-pop-back
           :deque-peak-front
           :deque-peak-back
           :deque-ref
           ))
(in-package deque)

(defun make-deque-buffer (size &key (element-type t))
 (make-array size :element-type element-type))

(defconstant +deque-default-buffer-size+ 64)

(defstruct (deque (:constructor make-deque
                      (&aux (buffer (make-deque-buffer +deque-default-buffer-size+)))))
  (size 0 :type fixnum)
  (capacity +deque-default-buffer-size+ :type fixnum)
  (front-index 0 :type fixnum)
  (back-index (1- +deque-default-buffer-size+) :type fixnum)
  (buffer (make-deque-buffer 64) :type simple-array))

(defun deque-empty-p (deque) (zerop (deque-size deque)))
(defun deque-full-p (deque) (= (deque-size deque) (deque-capacity deque)))
(defun deque-buffer-ref (deque index) (aref (deque-buffer deque) index))
(defun (setf deque-buffer-ref) (x deque index) (setf (aref (deque-buffer deque) index) x))
(defun round-index (capacity index) (logand (1- capacity) index))
(defun inc-index (capacity index) (round-index capacity (1+ index)))
(defun dec-index (capacity index) (round-index capacity (1- index)))

(defun deque-front (deque)
  (deque-buffer-ref deque (deque-front-index deque)))

(defun deque-back (deque)
  (deque-buffer-ref deque (deque-back-index deque)))

(defun inc-front-index (deque)
  (setf (deque-front-index deque)
        (inc-index (deque-capacity deque) (deque-front-index deque))))

(defun dec-front-index (deque)
  (setf (deque-front-index deque)
        (dec-index (deque-capacity deque) (deque-front-index deque))))

(defun inc-back-index (deque)
  (setf (deque-back-index deque)
        (inc-index (deque-capacity deque) (deque-back-index deque))))

(defun dec-back-index (deque)
  (setf (deque-back-index deque)
        (dec-index (deque-capacity deque) (deque-back-index deque))))

(defun (setf deque-front) (x deque) (setf (deque-buffer-ref deque (deque-front-index deque)) x))
(defun (setf deque-back) (x deque) (setf (deque-buffer-ref deque (deque-back-index deque)) x))

(defun deque-extends-buffer (deque)
  (let* ((prev-capacity (deque-capacity deque))
         (prev-buffer (deque-buffer deque))
         (new-capacity (* prev-capacity 2))
         (new-buffer (make-deque-buffer new-capacity
                                        :element-type (array-element-type
                                                       prev-buffer))))
    (loop repeat prev-capacity
          for prev-index = (deque-front-index deque)
            then (inc-index prev-capacity prev-index)
          for new-index from 0
          do (setf (aref new-buffer new-index)
                   (aref prev-buffer prev-index))
          finally (setf (deque-capacity deque) new-capacity
                        (deque-buffer deque) new-buffer
                        (deque-front-index deque) 0
                        (deque-back-index deque) (1- prev-capacity))
                  (return deque))))

(defun deque-push-front (deque x)
  (when (deque-full-p deque)
    (deque-extends-buffer deque))
  (dec-front-index deque)
  (incf (deque-size deque))
  (setf (deque-front deque) x)
  deque)

(defun deque-push-back (deque x)
  (when (deque-full-p deque)
    (deque-extends-buffer deque))
  (inc-back-index deque)
  (incf (deque-size deque))
  (setf (deque-back deque) x)
  deque)

(defun deque-pop-front (deque)
  (when (deque-empty-p deque)
    (error "DEQUE is empty. Cannot pop any element."))
  (setf (deque-buffer-ref deque (deque-front-index deque)) nil)
  (inc-front-index deque)
  (decf (deque-size deque))
  deque)

(defun deque-pop-back (deque)
  (when (deque-empty-p deque)
    (error "DEQUE is empty. Cannot pop any element."))
  (setf (deque-buffer-ref deque (deque-back-index deque)) nil)
  (dec-back-index deque)
  (decf (deque-size deque))
  deque)

(defun deque-peak-front (deque) (deque-front deque))
(defun deque-peak-back (deque) (deque-back deque))

(defun deque-ref (deque index)
  (deque-buffer-ref deque
                    (round-index (deque-capacity deque)
                                 (+ (deque-front-index deque)
                                    index))))

(defun (setf deque-ref) (x deque index)
  (setf (deque-buffer-ref deque
                          (round-index (deque-capacity deque)
                                       (+ (deque-front-index deque)
                                          index)))
        x))

;;;
;;; vector-bintree
;;;
(defpackage vector-bintree
  (:use :cl :utility)
  (:export :make-vector-bintree
           :make-extensible-vector-bintree
           :bintree-size
           :bintree-ref
           :bintree-push
           :bintree-pop
           :bintree-print
           :bintree-root-index-p
           :bintree-left-index
           :bintree-right-index
           :bintree-parent-index
           ))
(in-package vector-bintree)

(defun make-vector-bintree (size &rest args &key element-type initial-element initial-contents)
  (declare (ignore element-type initial-element initial-contents))
  (apply #'make-array size args))

(defun make-extensible-vector-bintree (size &rest args &key element-type initial-element initial-contents)
  (declare (ignore element-type initial-element initial-contents))
  (apply #'make-array size :adjustable t :fill-pointer 0 args))

(defun bintree-size (bt)
  (if (array-has-fill-pointer-p bt) (fill-pointer bt) (length bt)))

(defun bintree-ref (bt index) (aref bt index))
(defun (setf bintree-ref) (value bt index) (setf (aref bt index) value))

(defun bintree-push (value bintree) (vector-push-extend value bintree))
(defun bintree-pop (bintree) (vector-pop bintree))

(defun %bintree-print (bt index level)
  (let ((size (bintree-size bt))
        (left-index (bintree-left-index index))
        (right-index (bintree-right-index index)))
    (when (>= index size)
      (return-from %bintree-print))
    (%bintree-print bt right-index (1+ level))
    (dotimes (i level)
      (format t "    "))
    (format t "~A~%" (bintree-ref bt index))
    (%bintree-print bt left-index (1+ level))))

(defun bintree-print (bt) (%bintree-print bt 0 0))
(defun bintree-root-index-p (index) (zerop index))
(defun bintree-left-index (index) (1+ (* index 2)))
(defun bintree-right-index (index) (+ 2 (* index 2)))
(defun bintree-parent-index (index) (values (floor (1- index) 2)))

;;;
;;; binary-heap
;;;
(defpackage binary-heap
  (:use :cl :utility :vector-bintree)
  (:export :make-binary-heap
           :binary-heap-size
           :binary-heap-empty-p
           :binary-heap-push
           :binary-heap-top
           :binary-heap-pop
           ))
(in-package binary-heap)

(defconstant +binary-heap-default-buffer-size+ 64)

(defstruct (binary-heap (:constructor %make-binary-heap (op bintree)))
  op bintree)

(defun make-binary-heap (op &rest args &key initial-contents)
  "Call with #'<, makes ascending order."
  (let* ((len (length initial-contents))
         (bintree (apply #'make-extensible-vector-bintree
                         (if (null initial-contents) +binary-heap-default-buffer-size+ len)
                         args)))
    (unless (null initial-contents)
      (loop for index downfrom (1- len) downto 0
            do (downheap bintree index op)))
    (%make-binary-heap op bintree)))

(defun binary-heap-size (heap) (bintree-size (binary-heap-bintree heap)))
(defun binary-heap-empty-p (heap) (zerop (binary-heap-size heap)))

(defun upheap (bintree index op)
  (when (bintree-root-index-p index)
    (return-from upheap))
  (let ((parent-index (bintree-parent-index index)))
    (unless (funcall op (bintree-ref bintree parent-index) (bintree-ref bintree index))
      (rotatef (bintree-ref bintree parent-index) (bintree-ref bintree index))
      (upheap bintree parent-index op))))

(defun downheap (bintree index op)
  (let ((size (bintree-size bintree))
        (left-index (bintree-left-index index))
        (right-index (bintree-right-index index)))
    (when (>= left-index size)
      (return-from downheap))
    (when (>= right-index size)
      (downheap-swap bintree index left-index op)
      (return-from downheap))
    (if (funcall op (bintree-ref bintree left-index) (bintree-ref bintree right-index))
        (downheap-swap bintree index left-index op)
        (downheap-swap bintree index right-index op))))

(defun downheap-swap (bintree index child-index op)
  (unless (funcall op (bintree-ref bintree index) (bintree-ref bintree child-index))
    (rotatef (bintree-ref bintree index) (bintree-ref bintree child-index))
    (downheap bintree child-index op)))

(defun binary-heap-push (obj heap)
  (bintree-push obj (binary-heap-bintree heap))
  (upheap (binary-heap-bintree heap)
          (1- (binary-heap-size heap))
          (binary-heap-op heap))
  heap)

(defun binary-heap-pop (heap)
  (let ((op (binary-heap-op heap))
        (bintree (binary-heap-bintree heap))
        (size (binary-heap-size heap)))
    (rotatef (bintree-ref bintree 0) (bintree-ref bintree (1- size)))
    (bintree-pop bintree)
    (downheap bintree 0 op)
    heap))

(defun binary-heap-top (heap)
  (bintree-ref (binary-heap-bintree heap) 0))

(defun (setf binary-heap-top) (value heap)
  (let ((bintree (binary-heap-bintree heap)))
    (setf (bintree-ref bintree 0) value)
    (downheap bintree 0 (binary-heap-op heap))
    value))

(defun binary-heap-print (heap) (bintree-print (binary-heap-bintree heap)))

;;;
;;; ordered-map
;;;
(defpackage ordered-map
  (:use :cl :utility)
  (:export :make-rbtree
           :rbtree-search
           :rbtree-each
           :rbtree-to-list
           :rbtree-lower-bound
           :rbtree-upper-bound
           :rbtree-first
           :rbtree-last
           :rbtree-insert
           :rbtree-remove
           :rbtree-print
           ))
(in-package ordered-map)

;;; node

(defun make-node (&key color left value right) (list color left value right))
(defun make-empty-node () nil)
(defun node-empty-p (node) (null node))
(defun node-color (node) (if (node-empty-p node) 'black (first node)))
(defun node-left (node) (second node))
(defun node-value (node) (third node))
(defun node-right (node) (fourth node))

(defun (setf node-color) (color node)
  (if (node-empty-p node)
      (if (eq color 'black)
          'black
          (error "Leaf is must be black."))
      (setf (first node) color)))

(defun (setf node-value) (value node) (setf (third node) value))
(defun (setf node-left) (left node) (setf (second node) left))
(defun (setf node-right) (right node) (setf (fourth node) right))
(defun node-black-p (node) (eq (node-color node) 'black))
(defun node-red-p (node) (eq (node-color node) 'red))

(defun node-each (node fn)
  (unless (node-empty-p node)
    (node-each (node-left node) fn)
    (funcall fn (node-value node))
    (node-each (node-right node) fn)))

(defun node-search (node search-key key key-eq-p key-less-p &key default)
  (if (node-empty-p node)
      default
      (let ((node-key (funcall key (node-value node))))
        (cond ((funcall key-eq-p search-key node-key)
               (values (node-value node) t))
              ((funcall key-less-p search-key node-key)
               (node-search (node-left node)
                            search-key
                            key
                            key-eq-p
                            key-less-p
                            :default default))
              (t (node-search (node-right node)
                              search-key
                              key
                              key-eq-p
                              key-less-p
                              :default default))))))

(defun node-lower-bound (node search-key key key-eq-p key-less-p &key end)
  (if (node-empty-p node)
      (values end nil)
      (let ((node-key (funcall key (node-value node))))
        (if (or (funcall key-less-p search-key node-key)
                (funcall key-eq-p search-key node-key))
            (multiple-value-bind (value is-found)
                (node-lower-bound (node-left node)
                                  search-key
                                  key
                                  key-eq-p
                                  key-less-p
                                  :end end)
              (if is-found
                  (values value t)
                  (values (node-value node) t)))
            (node-lower-bound (node-right node)
                              search-key
                              key
                              key-eq-p
                              key-less-p
                              :end end)))))

(defun node-upper-bound (node search-key key key-eq-p key-less-p &key end)
  (if (node-empty-p node)
      (values end nil)
      (let ((node-key (funcall key (node-value node))))
        (if (funcall key-less-p search-key node-key)
            (multiple-value-bind (value is-found)
                (node-upper-bound (node-left node)
                                  search-key
                                  key
                                  key-eq-p
                                  key-less-p
                                  :end end)
              (if is-found
                  (values value t)
                  (values (node-value node) t)))
            (node-upper-bound (node-right node)
                              search-key
                              key
                              key-eq-p
                              key-less-p
                              :end end)))))

(macrolet ((%defun-rotate (a b)
             (let ((node-a (symb 'node- a))
                   (node-b (symb 'node- b))
                   (b-node (symb b '-node)))
               `(defun ,(symb 'rotate- a) (node)
                  (let ((,b-node (,node-b node)))
                    (setf (,node-b node) (,node-a ,b-node)
                          (,node-a ,b-node) node
                          (node-color ,b-node) (node-color node)
                          (node-color node) 'red)
                    ,b-node)))))
  (%defun-rotate right left)
  (%defun-rotate left right))
                  
(defun split-4node (node)
  (setf (node-color node) 'red
        (node-color (node-left node)) 'black
        (node-color (node-right node)) 'black)
  node)

(defun node-first (node)
  (let ((lnode (node-left node)))
    (if (node-empty-p lnode)
        node
        (node-first lnode))))

(defun node-last (node)
  (let ((rnode (node-right node)))
    (if (node-empty-p rnode)
        node
        (node-last rnode))))

(defun node-insert (node value key key-eq-p key-less-p)
  (if (node-empty-p node)
      (values (make-node :color 'red :value value) t)
      (let ((new-key (funcall key value))
            (node-key (funcall key (node-value node))))
        (cond ((funcall key-eq-p new-key node-key)
               (setf (node-value node) value)
               (values node nil))
              ((funcall key-less-p new-key node-key)
               (node-insert-left node value key key-eq-p key-less-p))
              (t (node-insert-right node value key key-eq-p key-less-p))))))

(macrolet ((%defun-node-insert (a b)
             (declare (ignorable b))
             (let ((node-a (symb 'node- a))
                   (a-node (symb a '-node)))
               `(defun ,(symb 'node-insert- a) (node value key key-eq-p key-less-p)
                  (multiple-value-bind (,a-node needs-balance)
                      (node-insert (,node-a node) value key key-eq-p key-less-p)
                    (setf (,node-a node) ,a-node)
                    (,(symb 'balance-insert- a) node needs-balance))))))
  (%defun-node-insert left right)
  (%defun-node-insert right left))

(macrolet ((%defun-balance-insert (a b aa ab)
             (let ((node-a (symb 'node- a))
                   (node-b (symb 'node- b))
                   (rotate-a (symb 'rotate- a))
                   (rotate-b (symb 'rotate- b)))
               `(defun ,(symb 'balance-insert- a) (node needs-balance)
                  (if (or (not needs-balance) (node-red-p node))
                      (values node needs-balance)
                      (cond ((and (node-red-p (,node-b node))
                                  (or (,(symb 'invalid-red- aa '-p) node)
                                      (,(symb 'invalid-red- ab '-p) node)))
                             (setf node (split-4node node))
                             (values node t))
                            ((,(symb 'invalid-red- aa '-p) node)
                             (values (,rotate-b node) nil))
                            ((,(symb 'invalid-red- ab '-p) node)
                             (setf (,node-a node) (,rotate-a (,node-a node)))
                             (values (,rotate-b node) nil))
                            (t (values node nil))))))))
  (%defun-balance-insert left right ll lr)
  (%defun-balance-insert right left rr rl))

(defun invalid-red-ll-p (node)
  (let ((lnode (node-left node)))
    (and (node-red-p lnode)
         (node-red-p (node-left lnode)))))

(defun invalid-red-lr-p (node)
  (let ((lnode (node-left node)))
    (and (node-red-p lnode)
         (node-red-p (node-right lnode)))))

(defun invalid-red-rl-p (node)
  (let ((rnode (node-right node)))
    (and (node-red-p rnode)
         (node-red-p (node-left rnode)))))

(defun invalid-red-rr-p (node)
  (let ((rnode (node-right node)))
    (and (node-red-p rnode)
         (node-red-p (node-right rnode)))))

(defun node-remove (node remove-key key key-eq-p key-less-p)
  (if (node-empty-p node)
      (values node nil)
      (let ((node-key (funcall key (node-value node))))
        (cond ((funcall key-eq-p remove-key node-key)
               (node-remove-here node remove-key key key-eq-p key-less-p))
              ((funcall key-less-p remove-key node-key)
               (node-remove-left node remove-key key key-eq-p key-less-p))
              (t (node-remove-right node remove-key key key-eq-p key-less-p))))))

(defun node-remove-here (node remove-key key key-eq-p key-less-p)
  (declare (ignore remove-key))
  (let ((lnode (node-left node))
        (rnode (node-right node)))
    (cond ((and (node-empty-p lnode)
                (node-empty-p rnode))
           (values (make-empty-node) (node-black-p node)))
          ((node-empty-p lnode)
           (setf (node-color rnode) 'black)
           (values rnode nil))
          ((node-empty-p rnode)
           (setf (node-color lnode) 'black)
           (values lnode nil))
          (t (let ((first-node (node-first rnode)))
               (setf (node-value node) (node-value first-node))
               (multiple-value-bind (new-rnode needs-balance)
                   (node-remove rnode
                                (funcall key (node-value first-node))
                                key
                                key-eq-p
                                key-less-p)
                 (setf (node-right node) new-rnode)
                 (balance-remove-right node needs-balance)))))))

(defun node-remove-left (node remove-key key key-eq-p key-less-p)
  (multiple-value-bind (new-left needs-balance)
      (node-remove (node-left node) remove-key key key-eq-p key-less-p)
    (setf (node-left node) new-left)
    (balance-remove-left node needs-balance)))

(defun balance-remove-left (node needs-balance)
  (if (not needs-balance)
      (values node nil)
      (let* ((lnode (node-left node))
             (rnode (node-right node))
             (rlnode (node-left rnode))
             (rrnode (node-right rnode)))
        (cond ((node-empty-p rnode) (values node needs-balance))
              ((and (node-black-p node)
                    (node-black-p lnode)
                    (node-black-p rnode)
                    (node-black-p rlnode)
                    (node-black-p rrnode))
               (setf (node-color rnode) 'red)
               (values node t))
              ((and (node-black-p node)
                    (node-black-p lnode)
                    (node-red-p rnode))
               (let ((new-node (rotate-left node)))
                 (multiple-value-bind (new-left needs-balance)
                     (balance-remove-left (node-left new-node) t)
                   (setf (node-left new-node) new-left)
                   (balance-remove-left new-node needs-balance))))
              ((and (node-red-p node)
                    (node-black-p rlnode)
                    (node-black-p rrnode))
               (rotatef (node-color node) (node-color rnode))
               (values node nil))
              ((and (node-black-p rnode)
                    (node-red-p rlnode)
                    (node-black-p rrnode))
               (setf (node-right node) (rotate-right rnode))
               (balance-remove-left node t))
              (t (let ((new-node (rotate-left node)))
                   (setf (node-color (node-left new-node)) 'black
                         (node-color (node-right new-node)) 'black)
                   (values new-node nil)))))))

(defun node-remove-right (node remove-key key key-eq-p key-less-p)
  (multiple-value-bind (new-right needs-balance)
      (node-remove (node-right node) remove-key key key-eq-p key-less-p)
    (setf (node-right node) new-right)
    (balance-remove-right node needs-balance)))

(defun balance-remove-right (node needs-balance)
  (if (not needs-balance)
      (values node nil)
      (let* ((rnode (node-right node))
             (lnode (node-left node))
             (lrnode (node-right lnode))
             (llnode (node-left lnode)))
        (cond ((node-empty-p lnode) (values node needs-balance))
              ((and (node-black-p node)
                    (node-black-p rnode)
                    (node-black-p lnode)
                    (node-black-p lrnode)
                    (node-black-p llnode))
               (setf (node-color lnode) 'red)
               (values node t))
              ((and (node-black-p node)
                    (node-black-p rnode)
                    (node-red-p lnode))
               (let ((new-node (rotate-right node)))
                 (multiple-value-bind (new-right needs-balance)
                     (balance-remove-right (node-right new-node) t)
                   (setf (node-right new-node) new-right)
                   (balance-remove-right new-node needs-balance))))
              ((and (node-red-p node)
                    (node-black-p lrnode)
                    (node-black-p llnode))
               (rotatef (node-color node) (node-color lnode))
               (values node nil))
              ((and (node-black-p lnode)
                    (node-red-p lrnode)
                    (node-black-p llnode))
               (setf (node-left node) (rotate-left lnode))
               (balance-remove-right node t))
              (t (let ((new-node (rotate-right node)))
                   (setf (node-color (node-left new-node)) 'black
                         (node-color (node-right new-node)) 'black)
                   (values new-node nil)))))))

(defun node-print (node depth &key (stream t) show-nil)
  (cond ((node-empty-p node)
         (when show-nil
             (loop repeat depth do (format stream "    "))
             (format stream "NIL~%")))
        (t (node-print (node-right node) (1+ depth) :stream stream :show-nil show-nil)
           (loop repeat depth do (format stream "    "))
           (format stream "(~A,~S)~%" (symbol-name (node-color node)) (node-value node))
           (node-print (node-left node) (1+ depth) :stream stream :show-nil show-nil))))

;;; rbtree

(defun make-rbtree (&key root (key #'identity) (key-eq-p #'eql) (key-less-p #'<))
  (list (or root (make-empty-node)) key key-eq-p key-less-p))

(defun rbtree-root (rbtree) (first rbtree))
(defun rbtree-key (rbtree) (second rbtree))
(defun rbtree-key-eq-p (rbtree) (third rbtree))
(defun rbtree-key-less-p (rbtree) (fourth rbtree))
(defun (setf rbtree-root) (root rbtree) (setf (first rbtree) root))
(defun rbtree-each (rbtree fn) (node-each (rbtree-root rbtree) fn))

(defun rbtree-to-list (rbtree)
  (let ((acc nil))
    (rbtree-each rbtree
                 (lambda (v)
                   (push v acc)))
    (nreverse acc)))

(defun rbtree-first (rbtree)
  (let ((node (node-first (rbtree-root rbtree))))
    (if (node-empty-p node)
        nil
        (node-value node))))

(defun rbtree-last (rbtree)
  (let ((node (node-last (rbtree-root rbtree))))
    (if (node-empty-p node)
        nil
        (node-value node))))

(defun rbtree-search (rbtree search-key &key default)
  (node-search (rbtree-root rbtree)
               search-key
               (rbtree-key rbtree)
               (rbtree-key-eq-p rbtree)
               (rbtree-key-less-p rbtree)
               :default default))

(defun rbtree-lower-bound (rbtree search-key &key end)
  (node-lower-bound (rbtree-root rbtree)
                    search-key
                    (rbtree-key rbtree)
                    (rbtree-key-eq-p rbtree)
                    (rbtree-key-less-p rbtree)
                    :end end))

(defun rbtree-upper-bound (rbtree search-key &key end)
  (node-upper-bound (rbtree-root rbtree)
                    search-key
                    (rbtree-key rbtree)
                    (rbtree-key-eq-p rbtree)
                    (rbtree-key-less-p rbtree)
                    :end end))

(defun rbtree-insert (rbtree value)
  (multiple-value-bind (root needs-balance)
      (node-insert (rbtree-root rbtree)
                   value
                   (rbtree-key rbtree)
                   (rbtree-key-eq-p rbtree)
                   (rbtree-key-less-p rbtree))
    (if needs-balance
        (setf (node-color root) 'black))
    (setf (rbtree-root rbtree) root)
    rbtree))

(defun rbtree-remove (rbtree remove-key)
  (let ((root (node-remove (rbtree-root rbtree)
                           remove-key
                           (rbtree-key rbtree)
                           (rbtree-key-eq-p rbtree)
                           (rbtree-key-less-p rbtree))))
    (setf (rbtree-root rbtree) root)
    rbtree))

(defun rbtree-print (rbtree &key (stream t) show-nil)
  (node-print (rbtree-root rbtree) 0 :stream stream :show-nil show-nil))

;;;
;;; vector
;;;
(defpackage vector
  (:use :cl :utility)
  (:export :make-vector
           :vector
           :copy-vector
           :vector-ref
           :vector-x
           :vector-y
           :vector-z
           :vector-w
           :vector-dimension
           :make-zero-vector
           :vector+
           :vector-
           :vector*
           :vector/
           :vector-dot
           :vector-cross
           :vector-squared-length
           :vector-length
           :vector-normalize
           :vector-atan
           :vector-angle
           ))
(in-package vector)

(defun make-vector (dimension &rest args &key element-type initial-element initial-contents)
  (declare (ignore element-type initial-element initial-contents))
  (apply #'make-array dimension args))

(defun copy-vector (vector) (copy-seq vector))
(defun vector-ref (vector dimension) (aref vector dimension))
(defun (setf vector-ref) (value vector dimension) (setf (aref vector dimension) value))

(macrolet ((%def-vector-ref (char index)
             `(defun ,(symb 'vector- char) (vector) (vector-ref vector ,index)))
           (%def-setf-vector-ref (char index)
             `(defun (setf ,(symb 'vector- char)) (value vector)
                (setf (vector-ref vector ,index) value))))
  (%def-vector-ref x 0)
  (%def-vector-ref y 1)
  (%def-vector-ref z 2)
  (%def-vector-ref w 3)
  (%def-setf-vector-ref x 0)
  (%def-setf-vector-ref y 1)
  (%def-setf-vector-ref z 2)
  (%def-setf-vector-ref w 3))

(defun vector-dimension (vector) (array-dimension vector 0))
(defun make-zero-vector (dimension) (make-vector dimension :initial-element 0))

(defun vector+ (vector &rest more-vectors)
  (let ((dimension (vector-dimension vector))
        (ret (copy-vector vector)))
    (dolist (vec more-vectors ret)
      (dotimes (i dimension)
        (incf (vector-ref ret i) (vector-ref vec i))))))

(defun vector- (vector &rest more-vectors)
  (if (null more-vectors)
      (vector* vector -1)
      (let ((dimension (vector-dimension vector))
            (ret (copy-vector vector)))
        (dolist (vec more-vectors ret)
          (dotimes (i dimension)
            (decf (vector-ref ret i) (vector-ref vec i)))))))

(defun vector* (vector &rest numbers)
  (let ((dimension (vector-dimension vector))
        (ret (copy-vector vector)))
    (dolist (n numbers ret)
      (dotimes (i dimension)
        (setf (vector-ref ret i)
              (* (vector-ref ret i) n))))))

(defun vector/ (vector &rest numbers)
  (let ((dimension (vector-dimension vector))
        (ret (copy-vector vector)))
    (dolist (n numbers ret)
      (dotimes (i dimension)
        (setf (vector-ref ret i)
              (/ (vector-ref ret i) n))))))

(defun vector-dot (v1 v2)
  (let ((dimension (vector-dimension v1))
        (ret 0))
    (dotimes (i dimension ret)
      (incf ret (* (vector-ref v1 i) (vector-ref v2 i))))))

(defun vector-cross (v1 v2)
  (let ((dimension (vector-dimension v1)))
    (cond ((= dimension 2)
           (- (* (vector-x v1) (vector-y v2))
              (* (vector-y v1) (vector-x v2))))
          ((= dimension 3)
           (vector (- (* (vector-y v1) (vector-z v2))
                      (* (vector-z v1) (vector-y v2)))
                   (- (* (vector-z v1) (vector-x v2))
                      (* (vector-x v1) (vector-z v2)))
                   (- (* (vector-x v1) (vector-y v2))
                      (* (vector-y v1) (vector-x v2)))))
          (t (error "VECTOR-CROSS: Not implemented on the dimension.")))))

(defun vector-squared-length (vector) (vector-dot vector vector))
(defun vector-length (vector) (sqrt (vector-squared-length vector)))
(defun vector-normalize (vector) (vector/ vector (vector-length vector)))
(defun vector-atan (vector) (atan (vector-y vector) (vector-x vector)))

(defun vector-angle (v1 v2)
  (acos (/ (vector-dot v1 v2)
           (* (vector-length v1) (vector-length v2)))))

;;;
;;; matrix
;;;
(defpackage matrix
  (:use :cl :utility :vector)
  (:export :make-matrix
           :matrix
           :matrixp
           :matrix-ref
           :matrix-row-major-ref
           :matrix-element-type
           :matrix-row
           :matrix-column
           :matrix-element-count
           :make-row-vector
           :make-zero-matrix
           :make-identity-matrix
           :matrix+
           :matrix-
           :matrix-scalar-multiply
           :matrix-product
           :linear-map
           ))
(in-package matrix)

(defun make-matrix (row column &rest args &key element-type initial-element initial-contents)
  (declare (ignore element-type initial-element initial-contents))
  (apply #'make-array (list row column) args))
              
(defun matrix (&rest rows)
  (let ((row (length rows))
        (col (length (car rows))))
    (make-matrix row col :initial-contents rows)))

(defun matrixp (matrix) (= (array-rank matrix) 2))
(defun matrix-ref (matrix y x) (aref matrix y x))
(defun matrix-row-major-ref (matrix index) (row-major-aref matrix index))
(defun (setf matrix-ref) (value matrix y x) (setf (aref matrix y x) value))

(defun (setf matrix-row-major-ref) (value matrix index)
  (setf (row-major-aref matrix index) value))

(defun matrix-element-type (matrix) (array-element-type matrix))
(defun matrix-row (matrix) (array-dimension matrix 0))
(defun matrix-column (matrix) (array-dimension matrix 1))
(defun matrix-element-count (matrix) (array-total-size matrix))

(defun make-row-vector (matrix row)
  (make-array (matrix-column matrix)
              :element-type (matrix-element-type matrix)
              :displaced-to matrix
              :displaced-index-offset (array-row-major-index matrix row 0)))

(defun make-zero-matrix (row column) (make-matrix row column :initial-element 0))

(defun make-identity-matrix (size)
  (let ((m (make-zero-matrix size size)))
    (loop for i below size
          do (setf (matrix-ref m i i) 1)
          finally (return m))))

(defun matrix-binary+ (m1 m2)
  (let* ((row (matrix-row m1))
         (col (matrix-column m1))
         (m (make-zero-matrix row col)))
    (dotimes (i (* row col) m)
      (setf (matrix-row-major-ref m i)
            (+ (matrix-row-major-ref m1 i)
               (matrix-row-major-ref m2 i))))))

(defun matrix+ (matrix &rest more-matrices)
  (reduce #'matrix-binary+
          more-matrices
          :initial-value matrix))

(defun matrix-binary- (m1 m2)
  (let* ((row (matrix-row m1))
         (col (matrix-column m1))
         (m (make-zero-matrix row col)))
    (dotimes (i (* row col) m)
      (setf (matrix-row-major-ref m i)
            (- (matrix-row-major-ref m1 i)
               (matrix-row-major-ref m2 i))))))

(defun matrix- (matrix &rest more-matrices)
  (if (null more-matrices)
      (matrix-binary- (make-zero-matrix (matrix-row matrix)
                                        (matrix-column matrix))
                      matrix)
      (reduce #'matrix-binary-
              more-matrices
              :initial-value matrix)))

(defun matrix-binary-scalar-multiply (matrix scalar)
  (let* ((row (matrix-row matrix))
         (col (matrix-column matrix))
         (m (make-zero-matrix row col)))
    (dotimes (i (* row col) m)
      (setf (matrix-row-major-ref m i)
            (* (matrix-row-major-ref matrix i) scalar)))))

(defun matrix-scalar-multiply (matrix &rest scalars)
  (reduce #'matrix-binary-scalar-multiply
          scalars
          :initial-value matrix))

(defun matrix-binary-product (m1 m2)
  (let* ((row (matrix-row m1))
         (col (matrix-column m2))
         (m (make-zero-matrix row col)))
    (dotimes (i row m)
      (dotimes (j col)
        (dotimes (k (matrix-column m1))
          (incf (matrix-ref m i j)
                (* (matrix-ref m1 i k)
                   (matrix-ref m2 k j))))))))

(defun matrix-product (matrix &rest more-matrices)
  (reduce #'matrix-binary-product
          more-matrices
          :initial-value matrix))

(defun linear-map (matrix vector)
  (let* ((row (matrix-row matrix))
         (v (make-zero-vector row)))
    (dotimes (i row v)
      (setf (vector-ref v i)
            (vector-dot vector (make-row-vector matrix i))))))

;;;
;;; geometry
;;;
(defpackage geometry
  (:use :cl :utility :vector :matrix)
  (:export :radian-to-degree
           :degree-to-radian
           :vector2-rotate90
           :vector2-rotate
           ))
(in-package geometry)

;;; angle

(defun radian-to-degree (radian) (/ (* radian 180) PI))
(defun degree-to-radian (degree) (/ (* degree PI) 180))

;;; matrix

(defun make-rotation90-matrix ()
  (matrix '(0 -1)
          '(1 0)))

(defun make-rotation-matrix (radian)
  (let* ((sin (sin radian))
         (cos (cos radian)))
    (matrix (list cos (- sin))
            (list sin cos))))

(defun vector2-rotate90 (vector)
  "Rotate vector counterclockwise in right-handed coordinates system."
  (linear-map (make-rotation90-matrix) vector))

(defun vector2-rotate (vector angle)
  "Rotate vector counterclockwise on right-handed coordinates system."
  (linear-map (make-rotation-matrix angle) vector))

;;;
;;; union-find
;;;
(defpackage union-find
  (:use :cl :utility)
  (:export :make-union-find
           :union-find-size
           :union-find-merge
           :union-find-unite-p
           :union-find-groups
           ))
(in-package union-find)

(defun make-union-find (size)
  (let ((parents (make-array size
                             :element-type 'fixnum
                             :initial-contents (iota size)))
        (ranks (make-array size
                           :element-type 'fixnum
                           :initial-element 0))
        (group-sizes (make-array size
                                 :element-type 'fixnum
                                 :initial-element 1)))
    (list parents ranks group-sizes)))

(defun union-find-size (uf) (length (first uf)))
(defun union-find-parent (uf n) (aref (first uf) n))
(defun union-find-rank (uf n) (aref (second uf) n))
(defun union-find-group-size (uf n) (aref (third uf) (union-find-root uf n)))
(defun (setf union-find-parent) (parent uf n) (setf (aref (first uf) n) parent))
(defun (setf union-find-rank) (rank uf n)(setf (aref (second uf) n) rank))
(defun (setf union-find-group-size) (group-size uf n) (setf (aref (third uf) n) group-size))

(defun union-find-root (uf n)
  (let ((parent (union-find-parent uf n)))
    (if (= parent n)
        n
        (setf (union-find-parent uf n) (union-find-root uf parent)))))

(defun union-find-merge (uf a b)
  (let ((ar (union-find-root uf a))
        (br (union-find-root uf b)))
    (when (= ar br)
      (return-from union-find-merge nil))
    (let ((ar-rank (union-find-rank uf ar))
          (br-rank (union-find-rank uf br)))
      (when (< ar-rank br-rank)
        (rotatef a b)
        (rotatef ar br)
        (rotatef ar-rank br-rank))
      (when (= ar-rank br-rank)
        (incf (union-find-rank uf ar)))
      (incf (union-find-group-size uf ar)
            (union-find-group-size uf br))
      (setf (union-find-parent uf br) ar)
      t)))

(defun union-find-unite-p (uf a b) (= (union-find-root uf a) (union-find-root uf b)))

(defun union-find-groups (uf)
  (let ((size (union-find-size uf)))
    (dotimes (i size)
      (union-find-root uf i))
    (let ((groups (make-array size :initial-element nil)))
      (dotimes (i size)
        (push i (aref groups (union-find-root uf i))))
      (coerce (remove-if #'null groups) 'list))))

;;;
;;; segment-tree
;;;
(defpackage segment-tree
  (:use :cl :utility :vector-bintree)
  (:export :make-segment-tree
           :segment-tree-get
           :segment-tree-fold
           :segment-tree-print))
(in-package segment-tree)

(defstruct (segment-tree
            (:constructor %make-st (&key size op id bintree bintree-size)))
  size op id bintree bintree-size)

(defun make-segment-tree (size op id &key initial-contents)
  "O(n)"
  (let* ((size (next-pow2 size))
         (bintree-size (1- (* size 2)))
         (bintree (make-vector-bintree bintree-size :initial-element id)))
    (unless (null initial-contents)
      (setf (subseq bintree (1- size)) initial-contents)
      (loop for index downfrom (- size 2) downto 0
            do (setf (bintree-ref bintree index)
                     (funcall op
                              (bintree-ref bintree (bintree-left-index index))
                              (bintree-ref bintree (bintree-right-index index))))))
    (%make-st :size size
              :op op
              :id id
              :bintree bintree
              :bintree-size bintree-size)))

(defun %st-index-to-bintree-index (st index) (1- (+ index (segment-tree-size st))))

(defun segment-tree-ref (st index)
  "O(1)"
  (bintree-ref (segment-tree-bintree st) (%st-index-to-bintree-index st index)))

(defun %st-fold (st fold-left fold-right index node-left node-right)
  (when (or (<= node-right fold-left)
            (<= fold-right node-left))
    (return-from %st-fold (segment-tree-id st)))
  (when (and (<= fold-left node-left)
             (<= node-right fold-right))
    (return-from %st-fold
      (bintree-ref (segment-tree-bintree st) index)))
  (let ((node-mid (+ node-left (floor (- node-right node-left) 2))))
    (funcall (segment-tree-op st)
             (%st-fold st fold-left fold-right
                       (bintree-left-index index)
                       node-left
                       node-mid)
             (%st-fold st fold-left fold-right
                       (bintree-right-index index)
                       node-mid
                       node-right))))

(defun segment-tree-fold (st left right)
  "[left, right), O(log(n))"
  (%st-fold st left right 0 0 (segment-tree-size st)))

(defun %st-set (st index value)
  (let ((bintree (segment-tree-bintree st)))
    (setf (bintree-ref bintree index) value)
    (loop with index = index
          while (> index 0)
          do (setf index
                   (bintree-parent-index index)
                   (bintree-ref bintree index)
                   (funcall (segment-tree-op st)
                            (bintree-ref bintree (bintree-left-index index))
                            (bintree-ref bintree (bintree-right-index index)))))))

(defun (setf segment-tree-ref) (value st index)
  "O(log(n))"
  (%st-set st (%st-index-to-bintree-index st index) value)
  value)

(defun segment-tree-print (st) (bintree-print (segment-tree-bintree st)))

;;;
;;; trie
;;;
(defpackage trie
  (:use :cl :utility)
  (:export :make-trie
           :trie-size
           :trie-find
           :trie-insert
           :trie-traverse
           :trie-node-char
           :trie-node-endp
           :trie-node-end-count
           :trie-node-prefix-count
           :trie-node-value
           ))
(in-package trie)

(defun make-trie-node (char) (vector char 0 0 (make-hash-table :test 'eql) nil))
(defun trie-node-char (node) (aref node 0))
(defun trie-node-end-count (node) (aref node 1))
(defun trie-node-endp (node) (not (zerop (trie-node-end-count node))))
(defun trie-node-prefix-count (node) (aref node 2))
(defun trie-node-next (node char) (values (gethash char (aref node 3) nil)))
(defun trie-node-value (node) (aref node 4))
(defun (setf trie-node-end-count) (count node) (setf (aref node 1) count))
(defun (setf trie-node-prefix-count) (count node) (setf (aref node 2) count))
(defun (setf trie-node-next) (next node char) (setf (gethash char (aref node 3)) next))
(defun (setf trie-node-value) (value node) (setf (aref node 4) value))

(defun trie-node-find (node str index &key (prefixp nil))
  (when (= index (length str))
    (return-from trie-node-find
      (if (or prefixp (trie-node-endp node))
          (values t (trie-node-value node))
          nil)))
  (let ((next (trie-node-next node (aref str index))))
    (if (null next)
        nil
        (trie-node-find next str (1+ index)))))

(defun trie-node-insert (node str index &optional value)
  (incf (trie-node-prefix-count node))
  (when (= index (length str))
    (incf (trie-node-end-count node))
    (setf (trie-node-value node) value)
    (return-from trie-node-insert (values str value)))
  (let* ((char (aref str index))
         (next (trie-node-next node char)))
    (trie-node-insert (if (null next)
                          (setf (trie-node-next node char)
                                (make-trie-node char))
                          next)
                      str (1+ index) value)))

(defun trie-node-traverse (node str index fn)
  (funcall fn node str index)
  (when (= index (length str))
    (return-from trie-node-traverse nil))
  (let ((next (trie-node-next node (aref str index))))
    (if (null next)
        nil
        (trie-node-traverse next str (1+ index) fn))))

(defun make-trie () (vector 0 (make-trie-node nil)))
(defun trie-size (trie) (aref trie 0))
(defun (setf trie-size) (size trie) (setf (aref trie 0) size))
(defun trie-root (trie) (aref trie 1))

(defun trie-find (trie str &key (prefixp nil))
  "O(|s|)"
  (trie-node-find (trie-root trie) str 0 :prefixp prefixp))

(defun trie-insert (trie str &optional value)
  "O(|s|)"
  (incf (trie-size trie))
  (trie-node-insert (trie-root trie) str 0 value))

(defun trie-traverse (trie str fn)
  "(function (trie string (function (trie-node string fixnum) t)) t)
O(|s|)"
  (trie-node-traverse (trie-root trie) str 0 fn))

;;;
;;; graph
;;;
(defpackage graph
  (:use :cl :utility :deque :binary-heap)
  (:export :<graph>
           :<edge>
           :graph-size
           :graph-node-ref
           :graph-neighbors
           :call-with-graph-neighbors
           :graph-add-edge
           :graph-delete-edge
           :do-graph-neighbors
           :edge-from
           :edge-to
           :edge-cost
           :make-edge
           :<adlist-graph>
           :make-adlist-graph
           :graph-adlist
           :<matrix-graph>
           :make-matrix-graph
           :graph-matrix
           :<grid-graph>
           :make-grid-graph
           :grid-height
           :grid-width
           :graph-grid
           :dijkstra
           :floyd-warshall
           :find-leaf))
(in-package graph)

(defconstant +graph-cost-infinity+ (ash most-positive-fixnum -1))

(defgeneric graph-size (graph))
(defgeneric graph-node-ref (graph node))
(defgeneric (setf graph-node-ref) (value graph node))
(defgeneric graph-edge-ref (graph from to))
(defgeneric (setf graph-edge-ref) (edge graph from to))
(defgeneric graph-neighbors (graph node)
  (:documentation "Returns <edge> list."))
(defgeneric call-with-graph-neighbors (graph node fn)
  (:documentation "Calls fn by <edge>."))
(defgeneric graph-add-edge (graph from to &key cost))
(defgeneric graph-delete-edge (graph from to))

(defgeneric edge-from (edge))
(defgeneric edge-to (edge))
(defgeneric edge-cost (edge))

(defmacro do-graph-neighbors ((var graph node &optional result) &body body)
  `(progn (call-with-graph-neighbors ,graph ,node (lambda (,var) ,@body))
          ,result))

(defclass <graph> () ())

(defclass <edge> ()
  ((from :initarg :from :accessor edge-from)
   (to :initarg :to :accessor edge-to)
   (cost :initarg :cost :initform 1 :accessor edge-cost)))

(defun make-edge (from to &key (cost 1)) (make-instance '<edge> :from from :to to :cost cost))

(defclass <adlist-graph> (<graph>)
  ((size :initarg :size :accessor graph-size)
   (adlist :initarg :adlist :accessor graph-adlist)
   (nodes :initarg :nodes :accessor graph-nodes)))

(defun make-adlist-graph (size)
  (make-instance '<adlist-graph>
                 :size size
                 :adlist (make-array size :initial-element nil)
                 :nodes (make-array size :initial-element nil)))

(defmethod graph-node-ref ((graph <adlist-graph>) node) (aref (graph-nodes graph) node))

(defmethod (setf graph-node-ref) (value (graph <adlist-graph>) node)
  (setf (aref (graph-nodes graph) node) value))

(defmethod graph-edge-ref ((graph <adlist-graph>) from to)
  (dolist (edge (aref (graph-adlist graph) from))
    (when (= (edge-to edge) to)
      (return-from graph-edge-ref edge))))

(defmethod (setf graph-edge-ref) (edge (graph <adlist-graph>) from to)
  (graph-delete-edge graph from to)
  (graph-add-edge graph (edge-from edge) (edge-to edge) :cost (edge-cost edge)))

(defmethod graph-neighbors ((graph <adlist-graph>) node) (aref (graph-adlist graph) node))

(defmethod call-with-graph-neighbors ((graph <adlist-graph>) node fn)
  (dolist (edge (graph-neighbors graph node))
    (funcall fn edge)))

(defmethod graph-add-edge ((graph <adlist-graph>) from to &key (cost 1))
  (push (make-edge from to :cost cost) (aref (graph-adlist graph) from)))

(defmethod graph-delete-edge ((graph <adlist-graph>) from to)
  (setf (aref (graph-adlist graph) from)
        (delete-if (lambda (edge) (and (= (edge-from edge) from)
                                       (= (edge-to edge) to)))
                   (aref (graph-adlist graph) from))))

(defclass <matrix-graph> (<graph>)
  ((size :initarg :size :accessor graph-size)
   (matrix :initarg :matrix :accessor graph-matrix)
   (nodes :initarg :nodes :accessor graph-nodes)))

(defun make-matrix-graph (size)
  (make-instance '<matrix-graph>
                 :size size
                 :matrix (let ((matrix (make-array (list size size) :initial-element nil)))
                           (dotimes (node size matrix)
                             (setf (aref matrix node node) (make-edge node node :cost 0))))
                 :nodes (make-array size :initial-element nil)))

(defmethod graph-node-ref ((graph <matrix-graph>) node) (aref (graph-nodes graph) node))

(defmethod (setf graph-node-ref) (value (graph <matrix-graph>) node)
  (setf (aref (graph-nodes graph) node) value))

(defmethod graph-edge-ref ((graph <matrix-graph>) from to) (aref (graph-matrix graph) from to))

(defmethod (setf graph-edge-ref) (edge (graph <matrix-graph>) from to)
  (setf (aref (graph-matrix graph) from to) edge))

(defmethod graph-neighbors ((graph <matrix-graph>) node)
  (let ((neighbors nil))
    (do-graph-neighbors (edge graph node (nreverse neighbors))
      (push edge neighbors))))

(defmethod call-with-graph-neighbors ((graph <matrix-graph>) node fn)
  (let ((matrix (graph-matrix graph)))
    (dotimes (to-node (graph-size graph))
      (when-let ((edge (aref matrix node to-node)))
        (funcall fn edge)))))

(defmethod graph-add-edge ((graph <matrix-graph>) from to &key (cost 1))
  (let ((edge (aref (graph-matrix graph) from to)))
    (when (or (null edge)
              (> (edge-cost edge) cost))
      (setf (aref (graph-matrix graph) from to) (make-edge from to :cost cost)))))

(defmethod graph-delete-edge ((graph <matrix-graph>) from to)
  (setf (aref (graph-matrix graph) from to) nil))

(defclass <grid-graph> (<graph>)
  ((height :initarg :height :accessor grid-height)
   (width :initarg :width :accessor grid-width)
   (grid :initarg :grid :accessor graph-grid)
   (nodes :initarg :nodes :accessor graph-nodes)))

(defun make-grid-graph (height width lines &key (wall #\#))
  (make-instance '<grid-graph>
                 :height height
                 :width width
                 :grid (make-array (list height width)
                                   :initial-contents
                                   (map 'list
                                        (lambda (row)
                                          (map 'list
                                               (lambda (e)
                                                 (if (eql e wall) +graph-cost-infinity+ 1))
                                               row))
                                        lines))))

(defun grid-row (graph node) (floor node (grid-width graph)))
(defun grid-column (graph node) (mod node (grid-width graph)))
(defun grid-point-to-index (graph row column) (+ (* (grid-height graph) row) column))
(defun grid-cost (graph row column) (aref (graph-grid graph) row column))
(defmethod graph-size ((graph <grid-graph>)) (* (grid-height graph) (grid-width graph)))
(defmethod graph-node-ref ((graph <grid-graph>) node) (aref (graph-nodes graph) node))

(defmethod (setf graph-node-ref) (value (graph <grid-graph>) node)
  (setf (aref (graph-nodes graph) node) value))

(defmethod graph-edge-ref ((graph <grid-graph>) from to)
  (let ((from-row (grid-row graph from))
        (from-col (grid-column graph from))
        (to-row (grid-row graph to))
        (to-col (grid-column graph to)))
    (cond ((and (= from-row to-row) (= from-col to-col)) (make-edge from to :cost 0))
          ((and (<= -1 (- from-row to-row) 1) (<= -1 (- from-col to-col) 1))
           (make-edge from to :cost (grid-cost graph to-row to-col)))
          (t nil))))

(defmethod (setf graph-edge-ref) (edge (graph <grid-graph>) from to) (error "Unsupported method."))

(defmethod graph-neighbors ((graph <grid-graph>) node)
  (let ((neighbors nil))
    (call-with-graph-neighbors graph node (lambda (edge) (push edge neighbors)))
    (nreverse neighbors)))

(defmethod call-with-graph-neighbors ((graph <grid-graph>) node fn)
  (do-neighbors ((y x) ((grid-row graph node) (grid-column graph node)))
    (when (and (< -1 y (grid-height graph)) (< -1 x (grid-width graph))
               (/= (aref (graph-grid graph) y x) +graph-cost-infinity+))
      (funcall fn (make-edge node (grid-point-to-index graph y x)
                             :cost (grid-cost graph y x))))))

(defmethod graph-add-edge ((graph <grid-graph>) from to &key cost)
  (declare (ignore graph from to cost))
  (error "Unsupported method."))

(defmethod graph-delete-edge ((graph <grid-graph>) from to) (error "Unsupported method."))

(defun bfs (graph &key (start 0) end)
  (loop with size = (graph-size graph)
        with distances = (make-array size :initial-element +graph-cost-infinity+)
        and usedp = (make-array size :initial-element nil)
        and deque = (make-deque)
        initially (setf (aref distances start) 0
                        (aref usedp start) t)
                  (deque-push-back deque start)
        until (deque-empty-p deque)
        do (let ((node (deque-peak-front deque)))
             (deque-pop-front deque)
             (do-graph-neighbors (edge graph node)
               (let ((next-node (edge-to edge)))
                 (unless (aref usedp next-node)
                   (setf (aref distances next-node) (1+ (aref distances node))
                         (aref usedp next-node) t)
                   (when (and end (= next-node end))
                     (return-from bfs distances))
                   (deque-push-back deque next-node)))))
        finally (return distances)))

(defun floyd-warshall (graph)
  (let* ((size (graph-size graph))
         (distances (make-array (list size size) :initial-element +graph-cost-infinity+)))
    (dotimes (i size)
      (dotimes (j size)
        (when-let ((edge (graph-edge-ref graph i j)))
          (setf (aref distances i j) (edge-cost edge)))))
    (dotimes (k size)
      (dotimes (i size)
        (dotimes (j size)
          (minf (aref distances i j) (+ (aref distances i k) (aref distances k j))))))
    distances))

(defun dijkstra-next-distance (distance edge) (+ distance (edge-cost edge)))

(defun dijkstra (graph &key (start 0) end (next-distance #'dijkstra-next-distance))
  (labels ((make-search-node (from to distance) (vector from to distance))
           (search-node-to (search-node) (elt search-node 1))
           (search-node-distance (search-node) (elt search-node 2))
           (search-node-less (search-node1 search-node2)
             (< (search-node-distance search-node1) (search-node-distance search-node2))))
    (loop with size = (graph-size graph)
          with distances = (make-array size :initial-element +graph-cost-infinity+)
          and usedp = (make-array size :initial-element nil)
          and heap = (make-binary-heap #'search-node-less)
                initially (setf (aref distances start) 0)
                          (binary-heap-push (make-search-node start start 0) heap)
          until (binary-heap-empty-p heap)
          do (let* ((top (binary-heap-top heap))
                    (node (search-node-to top))
                    (distance (search-node-distance top)))
               (binary-heap-pop heap)
               (unless (aref usedp node)
                 (when (and end (= node end))
                   (return-from dijkstra distances))
                 (setf (aref usedp node) t)
                 (do-graph-neighbors (edge graph node)
                   (let ((next-node (edge-to edge))
                         (next-distance (funcall next-distance distance edge)))
                     (when (< next-distance (aref distances next-node))
                       (setf (aref distances next-node) next-distance)
                       (binary-heap-push (make-search-node node next-node next-distance)
                                         heap))))))
          finally (return distances))))

(defun find-leaf (graph)
  (dotimes (node (graph-size graph))
    (when (singlep (graph-neighbors graph node))
      (return-from find-leaf node))))

;;;
;;; algorithm
;;;
(defpackage algorithm
  (:use :cl :utility)
  (:export :+default-sieve-max+
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

(defconstant +default-sieve-max+ 200000)

(defun sieve-of-eratosthenes (&optional (max +default-sieve-max+))
  (let ((sieve (make-array (1+ max) :initial-element t)))
    (setf (aref sieve 0) nil
          (aref sieve 1) nil)
    (loop for k from 4 to max by 2
          do (setf (aref sieve k) nil))
    (loop for i from 3 to max by 2
          do (loop for k from (* i 2) to max by i
                   do (setf (aref sieve k) nil)))
    sieve))

(defun linear-sieve (&optional (max +default-sieve-max+))
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

(defun primes (&optional (max +default-sieve-max+)) (coerce (linear-sieve max) 'list))

(defun least-prime-factors (&optional (max +default-sieve-max+))
  (multiple-value-bind (primes least-prime-factors) (linear-sieve max)
    (declare (ignore primes)) least-prime-factors))

(defun trivial-factorize (n)
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
  (loop with divisors = (list 1)
        and factors = #?(fast-factorize n least-prime-factors)
        for (factor . exp) in factors
        do (loop for d in divisors
                 for m = 1 then 1
                 do (loop repeat exp
                          do (setf m (* m factor))
                             (push (* d m) divisors)))
        finally (return divisors)))

(defun meguru-method (ok ng pred)
  (loop until (<= (abs (- ok ng)) 1)
        do (let ((mid (floor (+ ok ng) 2)))
             (if (funcall pred mid)
                 (setf ok mid)
                 (setf ng mid)))
        finally (return ok)))

(defun find-bound (vector element &key (compare #'<=) (start 0) end)
  (meguru-method (or end (length vector)) (1- start)
                 (lambda (index) (funcall compare element (aref vector index)))))

(defun lower-bound (vector element &rest args &key (compare #'<=) (start 0) end)
  (declare (ignore compare start end))
  (apply #'find-bound vector element args))

(defun upper-bound (vector element &rest args &key (compare #'<) (start 0) end)
  (declare (ignore compare start end))
  (apply #'find-bound vector element args))

(defun cumulate (seq &key (op #'+) (id 0) (element-type t))
  (loop with ret = (make-array (1+ (length seq))
                               :element-type element-type
                               :initial-element id)
        and iter = (make-iterator seq)
        and index = 0
        until (iterator-endp iter)
        do (setf (aref ret (1+ index))
                 (funcall op (iterator-element iter) (aref ret index)))
           (setf iter (iterator-next iter))
           (incf index)
        finally (return ret)))

(defmacro dp (name params &body body)
  (let ((memo (gensym))
        (key (gensym))
        (memo-value (gensym))
        (none-value (gensym)))
    `(let ((,memo (make-hash-table :test 'equal)))
       (labels ((,name ,params
                  (let* ((,key (list ,@params))
                         (,memo-value (gethash ,key ,memo ',none-value)))
                    (if (not (eq ,memo-value ',none-value))
                        ,memo-value
                        (setf (gethash ,key ,memo)
                              (progn ,@body))))))
         #',name))))

(defmacro array-dp (name param-and-maxs &body body)
  (let ((memo (gensym))
        (params (mapcar #'car param-and-maxs))
        (maxs (mapcar #'cadr param-and-maxs)))
    `(let ((,memo (make-array (list ,@maxs) :initial-element nil)))
       (labels ((,name ,params
                  (or (aref ,memo ,@params)
                      (setf (aref ,memo ,@params)
                            (progn ,@body)))))
         #',name))))

;;;
;;; atcoder
;;;
(defpackage atcoder
  (:use :cl
        :utility
        :input
        :deque
        :binary-heap
        :ordered-map
        :vector
        :matrix
        :geometry
        :union-find
        :segment-tree
        :trie
        :graph
        :algorithm
        )
  (:export :main
           :test
           ))
(in-package atcoder)

(defun test-case (input expect)
  (let ((output (make-array 0
                            :element-type 'character
                            :fill-pointer t
                            :adjustable t)))
    (with-output-to-string (*standard-output* output)
      (with-input-from-string (*standard-input* input)
        (main)))
    (if (string= (string-trim '(#\Space #\Newline) output)
                 (string-trim '(#\Space #\Newline) expect))
        (format t "Pass~%")
        (format t "Failed~%expect: ~A~%but acctual: ~A~%" expect output))))

(defun factorize (n)
  (let ((acc 1))
    (dotimes (i n acc)
      (setf acc (* (1+ i) acc)))))

(defconstant +inf+ 1000000000000000)

(defun main ()
  (input* ((n fixnum)
           (m fixnum)
           (uvt (vector (cons* fixnum1 fixnum1 fixnum nil) m))
           (q fixnum))
    (let ((graph (make-matrix-graph n)))
      (dotimes (i m)
        (let* ((edge (aref uvt i))
               (from (first edge))
               (to (second edge))
               (cost (third edge)))
          (graph-add-edge graph from to :cost cost)
          (graph-add-edge graph to from :cost cost)))
      (let ((distances (floyd-warshall graph)))
        (dotimes (i q)
          (input* ((k fixnum)
                   (b (vector fixnum1 k)))
            (let ((result +inf+))
              (do-permutations (b b)
                (dotimes (s (ash 1 k))
                  (let ((route nil))
                    (dotimes (j k)
                      (let* ((edge (aref uvt (aref b j)))
                             (from (first edge))
                             (to (second edge))
                             (cost (third edge)))
                        (if (logbitp j s)
                            (push (list from to cost) route)
                            (push (list to from cost) route))))
                    (minf result
                          (loop with ret = 0
                                and prev = 0
                                for (from to cost) in route
                                do (incf ret (+ (aref distances prev from)
                                                cost))
                                   (setf prev to)
                                finally (incf ret (aref distances prev (1- n)))
                                        (return ret))))))
              (format t "~A~%" result))))))))

(defun test ()
  (test-case "3 5
1 2 10
1 3 20
1 3 30
2 3 15
2 3 25
2
1
1
2
3 5
" "25
70
")
  (test-case "6 6
1 5 1
2 5 1
2 4 1
3 4 1
3 6 1
1 6 1
2
5
1 2 3 4 5
1
5
" "5
3
")
  (test-case "5 5
1 2 1000000000
2 3 1000000000
3 4 1000000000
4 5 1000000000
1 5 1000000000
1
1
3
" "4000000000")
  )

#-swank (main)

