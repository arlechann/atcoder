(in-package :cl-user)

#-swank
(unless (member :child-sbcl *features*)
  (quit
   :recklessly-p t
   :unix-status
   (process-exit-code
    (run-program *runtime-pathname*
                 `("--control-stack-size" "256MB"
                   "--noinform" "--disable-ldb" "--lose-on-corruption" "--end-runtime-options"
                   "--eval" "(push :child-sbcl *features*)"
                   "--script" ,(namestring *load-pathname*))
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
           :fbind
           :fbind*
           :dotimes-rev
           :dovector
           :do-combination
           :do-neighbors
           :let-dyn
           ;; number
           :2*
           :/2
           :repunit
           :next-pow2
           :maxp
           :minp
           :maxf
           :minf
           :logipop
           :logmsb
           ;; function
           :compose
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
           :length1p
           :take
           :drop
           :longerp
           :longer
           :iota
           :unfold
           :unique
           :with-index
           :permutation
           :flatten
           ;; vector
           :dvector
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

(defmacro defun-always (name params &body body)
  `(eval-always (defun ,name ,params ,@body)))

(defmacro defalias (alias original)
  `(progn (setf (symbol-function ,alias)
                ,original)
          ,alias))

;;; reader macro

(defun-always def-dispatch-fn (char fn)
  (set-dispatch-macro-character #\# char
                                (lambda (stream char1 char2)
                                  (declare (ignorable stream char1 char2))
                                  (funcall fn
                                           (read stream t nil t)))))

(defmacro def-dispatch-macro (char params &body body)
  `(eval-always
     (def-dispatch-fn ,char (lambda ,params ,@body))))

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

(def-delimiter-macro #\[ #\] (arr &rest indecies)
  `(aref ,arr ,@indecies))

;;; control

(defmacro nlet (name binds &body body)
  `(labels ((,name ,(mapcar #'car binds) ,@body))
     (,name ,@(mapcar #'cadr binds))))

(defmacro alambda (params &body body)
  `(labels ((self ,params ,@body)) #'self))

(defmacro if-let* (binds then &optional else)
  `(let* ,binds
     (if (and ,@(mapcar #'car binds))
         ,then
         ,else)))

(defmacro when-let (binds &body body)
  `(let ,binds
     (when (and ,@(mapcar #'car binds))
       ,@body)))

(defmacro when-let* (binds &body body)
  `(let* ,binds
     (when (and ,@(mapcar #'car binds))
       ,@body)))

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro fbind (binds &body body)
  (let ((args (gensym)))
    `(flet ,(mapcar (lambda (bind)
                      (let ((var (car bind))
                            (fn (cadr bind)))
                        `(,var (&rest ,args) (apply ,fn ,args))))
                    binds)
       (declare (inline ,@(mapcar #'car binds)))
       ,@body)))

(defmacro fbind* (binds &body body)
  (cond ((null binds) `(progn ,@body))
        ((null (cdr binds)) `(fbind (,(car binds)) ,@body))
        (t `(fbind (,(car binds))
              (fbind* ,(cdr binds)
                ,@body)))))

(defmacro dotimes-rev ((var count &optional result) &body body)
  (let ((index (gensym))
        (temp-count (gensym)))
    `(let ((,temp-count ,count))
       (dotimes (,index ,temp-count ,result)
         (let ((,var (- ,temp-count ,index 0)))
           ,@body)))))

(defmacro dovector ((var init-form &optional result) &body body)
  (let ((vec (gensym))
        (index (gensym)))
    `(let ((,vec ,init-form))
       (dotimes (,index (length ,vec) ,result)
         (let ((,var (aref ,vec ,index)))
           (declare (ignorable ,var))
           ,@body)))))

(defmacro do-combination (((&rest vars) (&rest counts) &optional result) &body body)
  (if (or (null vars)
          (null counts))
      `(progn ,@body)
      `(dotimes (,(car vars) ,(car counts) ,result)
         (do-combination (,(cdr vars) ,(cdr counts) ,result)
           ,@body))))

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
     (declare (dynamic-extent ,@(mapcar (lambda (x)
                                          (if (listp x) (car x) x))
                                        binds)))
     ,@body))

;;; number

(defun 2* (x) (* x 2))
(defun /2 (x) (values (floor x 2)))

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

(defun maxp (x &rest args)
  (> x (apply #'max args)))

(defun minp (x &rest args)
  (< x (apply #'min args)))

(defmacro maxf (place &rest args)
  `(setf ,place (max ,place ,@args)))

(defmacro minf (place &rest args)
  `(setf ,place (min ,place ,@args)))

(defun logipop (n &rest indexes)
  (dolist (index indexes n)
    (setf n (logior n (ash 1 index)))))

(defun logmsb (n)
  (loop for i from 0
        when (= (ash 1 i) n)
          return i
        when (> (ash 1 i) n)
          return (1- i)))

;;; function

(defun compose (&rest fns)
  (lambda (x)
    (reduce #'funcall
            fns
            :initial-value x
            :from-end t)))

;;; sequence

(defun sum (seq)
  (reduce #'+ seq :initial-value 0))

(defmacro sortf (seq-place pred &rest args)
  `(setf ,seq-place
         (sort ,seq-place ,pred ,@args)))

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

(defun-always ensure-car (x)
  (if (consp x) (car x) x))

(defun-always ensure-list (x)
  (if (listp x) x (list x)))

(defun length-n-p (lst n)
  (loop when (zerop n)
          return (null lst)
        when (null lst)
          return nil
        do (setf lst (cdr lst)
                 n (1- n))))

(defun length1p (lst)
  (and lst
       (null (cdr lst))))

(defun take (lst len)
  (let ((acc nil))
    (dotimes (i len)
      (push (car lst) acc)
      (setf lst (cdr lst)))
    (nreverse acc)))

(defun drop (lst len)
  (dotimes (i len lst)
    (setf lst (cdr lst))))

(defun longerp (lst1 lst2)
  (loop when (null lst1)
          return nil
        when (null lst2)
          return t
        do (setf lst1 (cdr lst1)
                 lst2 (cdr lst2))))
  
(defun longer (lst1 lst2)
  (if (longerp lst1 lst2) lst1 lst2))

(defun iota (count &optional (start 0) (step 1))
  (let ((acc nil))
    (dotimes (i count)
      (push start acc)
      (setf start (+ start step)))
    (nreverse acc)))

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

;;; char

(defun count-alphabet ()
  #.(1+ (- (char-code #\Z) (char-code #\A))))

(defun lower-to-index (char)
  (- (char-code char) #.(char-code #\a)))

(defun upper-to-index (char)
  (- (char-code char) #.(char-code #\A)))

(defun char-to-index (char)
  (if (char< char #\a)
      (upper-to-index char)
      (lower-to-index char)))

;;; lazy

(defstruct promise (value nil) thunk)

(defmacro delay (expr)
  `(make-promise :thunk (lambda () ,expr)))

(defun force (ps)
  (when (promise-thunk ps)
    (setf (promise-value ps) (funcall (promise-thunk ps))
          (promise-thunk ps) nil))
  (promise-value ps))

;;; symbol

(defun-always symb (&rest args)
  (values (intern
           (concatenate 'string
                        (with-output-to-string (s)
                          (mapcar (lambda (x) (princ x s))
                                  args)))
           *package*)))

;;;
;;; input
;;;
(defpackage :input
  (:use :cl
        :utility)
  (:export :input*
           :def-input-reader))
(in-package :input)

;; (input* ((n fixnum)
;;          (v (vector (cons* fixnum1 fixnum1 nil) n)))
;;   (list n v))
;; 3
;; 1 2
;; 3 4
;; 5 6
;; ; => (3 #((0 1) (2 3) (4 5)))

(eval-always
  (defvar *input-reader-table* (make-hash-table :test #'eq)))

(defun-always set-input-reader (marker reader)
  (setf (gethash marker *input-reader-table*) reader))

(defun-always get-input-reader (marker)
  (gethash marker *input-reader-table*))

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

(defmacro input* (forms &body body)
  `(let* ,(input-expand forms) ,@body))

(defmacro def-input-reader (marker params &body body)
  `(eval-always
     (set-input-reader ',marker
                       (macrolet ((reader (typespec)
                                    `(input-typespec-reader ,typespec)))
                         (lambda (,@params &optional arg)
                           (declare (ignore arg))
                           ,@body)))
     ',marker))

(def-input-reader fixnum ()
  '(read))

(def-input-reader fixnum1 ()
  '(1- (read)))

(def-input-reader double ()
  '(let ((*read-default-float-format* 'double-float))
    (read)))

(def-input-reader string ()
  `(read-line))

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
  (:use :cl
        :utility)
  (:export :make-deque
           :deque-size
           :deque-empty-p
           :deque-push-front
           :deque-push-back
           :deque-pop-front
           :deque-pop-back
           :deque-peak-front
           :deque-peak-back
           ))
(in-package deque)

(defun make-deque-buffer (size &key (element-type t))
  (make-array size :element-type element-type))

(defstruct (deque (:constructor make-deque
                      (&key (element-type t)
                       &aux (buffer (make-deque-buffer 64 :element-type element-type)))))
  (size 0 :type fixnum)
  (capacity 64 :type fixnum)
  (front-index 0 :type fixnum)
  (back-index 0 :type fixnum)
  (buffer (make-deque-buffer 64) :type simple-array))

(defun deque-empty-p (deque)
  (zerop (deque-size deque)))

(defun deque-full-p (deque)
  (= (deque-size deque)
     (deque-capacity deque)))

(defun deque-buffer-at (deque index)
  (aref (deque-buffer deque)
        index))

(defun (setf deque-buffer-at) (x deque index)
  (setf (aref (deque-buffer deque)
              index)
        x))

(defun dec-index (capacity index)
  (logand (1- capacity)
          (1- index)))

(defun inc-index (capacity index)
  (logand (1- capacity)
          (1+ index)))

(defun deque-front (deque)
  (deque-buffer-at deque
                   (deque-front-index deque)))

(defun deque-back (deque)
  (deque-buffer-at deque
                   (dec-index (deque-capacity deque)
                              (deque-back-index deque))))

(defun (setf deque-front) (x deque)
  (setf (deque-buffer-at deque
                         (deque-front-index deque))
        x))

(defun (setf deque-back) (x deque)
  (setf (deque-buffer-at deque
                         (dec-index (deque-capacity deque)
                                    (deque-back-index deque)))
        x))

(defun inc-front-index (deque)
  (setf (deque-front-index deque)
        (inc-index (deque-capacity deque)
                   (deque-front-index deque)))
  deque)

(defun dec-front-index (deque)
  (setf (deque-front-index deque)
        (dec-index (deque-capacity deque)
                   (deque-front-index deque)))
  deque)

(defun inc-back-index (deque)
  (setf (deque-back-index deque)
        (inc-index (deque-capacity deque)
                   (deque-back-index deque)))
  deque)

(defun dec-back-index (deque)
  (setf (deque-back-index deque)
        (dec-index (deque-capacity deque)
                   (deque-back-index deque)))
  deque)

(defun deque-extends-buffer (deque)
  (let* ((prev-capacity (deque-capacity deque))
         (prev-buffer (deque-buffer deque))
         (new-capacity (2* prev-capacity))
         (new-buffer
           (make-deque-buffer new-capacity
                              :element-type (array-element-type
                                             prev-buffer))))
    (loop repeat prev-capacity
          for prev-index = (deque-front-index deque)
            then (inc-index prev-capacity prev-index)
          for new-index = 0
            then (inc-index new-capacity new-index)
          do (setf (aref new-buffer new-index)
                   (aref prev-buffer prev-index))
          finally (setf (deque-capacity deque) new-capacity
                        (deque-buffer deque) new-buffer
                        (deque-front-index deque) 0
                        (deque-back-index deque) prev-capacity)
                  (return deque))))

(defun deque-push-front (deque x)
  (when (deque-full-p deque)
    (deque-extends-buffer deque))
  (dec-front-index deque)
  (setf (deque-front deque) x)
  (incf (deque-size deque))
  deque)

(defun deque-push-back (deque x)
  (when (deque-full-p deque)
    (deque-extends-buffer deque))
  (inc-back-index deque)
  (setf (deque-back deque) x)
  (incf (deque-size deque))
  deque)

(defun need-free-p (typespec)
  (eq typespec 't))

(defun deque-pop-front (deque)
  (when (deque-empty-p deque)
    (error "DEQUE is empty. Cannot pop any element."))
  (when (need-free-p (array-element-type (deque-buffer deque)))
    (setf (deque-back deque) nil))
  (inc-front-index deque)
  (decf (deque-size deque))
  deque)

(defun deque-pop-back (deque)
  (when (deque-empty-p deque)
    (error "DEQUE is empty. Cannot pop any element."))
  (when (need-free-p (array-element-type (deque-buffer deque)))
    (setf (deque-back deque) nil))
  (dec-back-index deque)
  (decf (deque-size deque))
  deque)

(defun deque-peak-front (deque)
  (deque-front deque))

(defun deque-peak-back (deque)
  (deque-back deque))

;;;
;;; ordered-map
;;;
(defpackage ordered-map
  (:nicknames :omap)
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

(defun make-node (&key color left value right)
  (list color left value right))

(defun make-empty-node () nil)

(defun node-empty-p (node) (null node))

(defun node-color (node)
  (if (node-empty-p node)
      'black
      (first node)))

(defun node-left (node) (second node))

(defun node-value (node) (third node))

(defun node-right (node) (fourth node))

(defun (setf node-color) (color node)
  (if (node-empty-p node)
      (if (eq color 'black)
          'black
          (error "Leaf is must be black."))
      (setf (first node) color)))

(defun (setf node-value) (value node)
  (setf (third node) value))

(defun (setf node-left) (left node)
  (setf (second node) left))

(defun (setf node-right) (right node)
  (setf (fourth node) right))

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

(defun (setf rbtree-root) (root rbtree)
  (setf (first rbtree) root))

(defun rbtree-each (rbtree fn)
  (node-each (rbtree-root rbtree) fn))

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

(defun union-find-size (uf)
  (length (first uf)))

(defun union-find-parent (uf n)
  (aref (first uf) n))

(defun union-find-rank (uf n)
  (aref (second uf) n))

(defun union-find-group-size (uf n)
  (aref (third uf) (union-find-root uf n)))

(defun (setf union-find-parent) (parent uf n)
  (setf (aref (first uf) n) parent))

(defun (setf union-find-rank) (rank uf n)
  (setf (aref (second uf) n) rank))

(defun (setf union-find-group-size) (group-size uf n)
  (setf (aref (third uf) n) group-size))

(defun union-find-root (uf n)
  (let ((parent (union-find-parent uf n)))
    (if (= parent n)
        n
        (let ((root (union-find-root uf parent)))
          (setf (union-find-parent uf n) root)
          root))))

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

(defun union-find-unite-p (uf a b)
  (= (union-find-root uf a)
     (union-find-root uf b)))

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
  (:use :cl :utility)
  (:export :make-segment-tree
           :make-segment-tree-from-sequence
           :segment-tree-get
           :segment-tree-fold
           :segment-tree-print))
(in-package segment-tree)

(defun make-bintree (size &key (element-type t) initial-element)
  (make-array size
              :element-type element-type
              :initial-element initial-element))

(defun bintree-size (bt) (length bt))
(defun bintree-get (bt index) (aref bt index))
(defun (setf bintree-get) (value bt index)
  (setf (aref bt index) value))

(defun %bintree-print (bt index level)
  (let ((size (bintree-size bt))
        (left-index (bintree-index-left index))
        (right-index (bintree-index-right index)))
    (when (>= index size)
      (return-from %bintree-print))
    (%bintree-print bt right-index (1+ level))
    (dotimes (i level)
      (format t "    "))
    (format t "~A~%" (bintree-get bt index))
    (%bintree-print bt left-index (1+ level))))

(defun bintree-print (bt)
  (%bintree-print bt 0 0))

(defun bintree-index-left (index)
  (1+ (* index 2)))
(defun bintree-index-right (index)
  (+ 2 (* index 2)))
(defun bintree-index-parent (index)
  (values (floor (1- index) 2)))

(defstruct (segment-tree
            (:constructor %make-st (&key size op id bintree bintree-size)))
  size op id bintree bintree-size)

(defun make-segment-tree (size op id &key (element-type t))
  "O(n)"
  (let* ((size (next-pow2 size))
         (bintree-size (1- (* size 2)))
         (bintree (make-bintree bintree-size
                                :element-type element-type
                                :initial-element id)))
    (%make-st :size size
              :op op
              :id id
              :bintree bintree
              :bintree-size bintree-size)))

(defun make-segment-tree-from-sequence (seq op id &key (element-type t element-type-p))
  "O(n)"
  (let* ((size (next-pow2 (length seq)))
         (bintree-size (1- (* size 2)))
         (bintree (cond (element-type-p
                         (make-bintree bintree-size
                                       :element-type element-type
                                       :initial-element id))
                        ((typep seq 'array)
                         (make-bintree bintree-size
                                       :element-type (array-element-type seq)
                                       :initial-element id))
                        (t (make-bintree bintree-size
                                         :initial-element id)))))
    (loop with iter = (make-iterator seq)
          with index = 0
          until (iterator-endp iter)
          do (setf (bintree-get bintree (1- (+ index size)))
                   (iterator-element iter))
             (setf iter (iterator-next iter))
             (incf index))
    (loop for index downfrom (- size 2) downto 0
          do (setf (bintree-get bintree index)
                   (funcall op
                            (bintree-get bintree (bintree-index-left index))
                            (bintree-get bintree (bintree-index-right index)))))
    (%make-st :size size
              :op op
              :id id
              :bintree bintree
              :bintree-size bintree-size)))

(defun %st-index-to-bintree-index (st index)
  (1- (+ index (segment-tree-size st))))

(defun segment-tree-get (st index)
  "O(1)"
  (bintree-get (segment-tree-bintree st)
               (%st-index-to-bintree-index st index)))

(defun %st-fold (st fold-left fold-right index node-left node-right)
  (when (or (<= node-right fold-left)
            (<= fold-right node-left))
    (return-from %st-fold (segment-tree-id st)))
  (when (and (<= fold-left node-left)
             (<= node-right fold-right))
    (return-from %st-fold
      (bintree-get (segment-tree-bintree st) index)))
  (let ((node-mid (+ node-left (floor (- node-right node-left) 2))))
    (funcall (segment-tree-op st)
             (%st-fold st fold-left fold-right
                       (bintree-index-left index)
                       node-left
                       node-mid)
             (%st-fold st fold-left fold-right
                       (bintree-index-right index)
                       node-mid
                       node-right))))

(defun segment-tree-fold (st left right)
  "[left, right), O(log(n))"
  (%st-fold st left right 0 0 (segment-tree-size st)))

(defun %st-set (st index value)
  (let ((bintree (segment-tree-bintree st)))
    (setf (bintree-get bintree index) value)
    (loop with index = index
          while (> index 0)
          do (setf index
                   (bintree-index-parent index)
                   (bintree-get bintree index)
                   (funcall (segment-tree-op st)
                            (bintree-get bintree (bintree-index-left index))
                            (bintree-get bintree (bintree-index-right index)))))))

(defun (setf segment-tree-get) (value st index)
  "O(log(n))"
  (%st-set st (%st-index-to-bintree-index st index) value)
  value)

(defun segment-tree-print (st)
  (bintree-print (segment-tree-bintree st)))

;;;
;;; trie
;;;
(defpackage trie
  (:use :cl)
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

(defun make-trie-node (char)
  (vector char 0 0 (make-hash-table :test 'eql) nil))
(defun trie-node-char (node) (aref node 0))
(defun trie-node-end-count (node) (aref node 1))
(defun trie-node-endp (node) (not (zerop (trie-node-end-count node))))
(defun trie-node-prefix-count (node) (aref node 2))
(defun trie-node-next (node char)
  (values (gethash char (aref node 3) nil)))
(defun trie-node-value (node) (aref node 4))
(defun (setf trie-node-end-count) (count node) (setf (aref node 1) count))
(defun (setf trie-node-prefix-count) (count node) (setf (aref node 2) count))
(defun (setf trie-node-next) (next node char)
  (setf (gethash char (aref node 3)) next))
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

(defun make-trie ()
  (vector 0 (make-trie-node nil)))
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
  (:use :cl :utility)
  (:export :make-graph-from-edges
           :graph-neighbors
           :graph-size
           :find-leaf))
(in-package graph)

(defun make-graph-from-edges (size edges &key (bidirectional t))
  (let ((graph (make-array size :initial-element nil)))
    (dovector (edge edges graph)
      (push (second edge) (aref graph (first edge)))
      (when bidirectional
        (push (first edge) (aref graph (second edge)))))))

(defun graph-neighbors (graph node)
  (aref graph node))

(defun graph-size (graph)
  (length graph))

(defun find-leaf (graph)
  (dotimes (node (graph-size graph))
    (when (length1p (graph-neighbors graph node))
      (return-from find-leaf node))))

;;;
;;; algorithm
;;;
(defpackage algorithm
  (:nicknames :algo)
  (:use :cl :utility)
  (:export :meguru-method
           :cumulate
           :cumsum
           :dp
           :defdp
           ))
(in-package algorithm)

(defun meguru-method (ok ng pred)
  (loop until (<= (abs (- ok ng)) 1)
        do (let ((mid (floor (+ ok ng) 2)))
             (if (funcall pred mid)
                 (setf ok mid)
                 (setf ng mid)))
        finally (return ok)))

(defun cumulate (seq op id &key (element-type t))
  (loop with ret = (make-array (1+ (length seq))
                               :element-type element-type
                               :initial-element id)
        with iter = (make-iterator seq)
        with index = 0
        until (iterator-endp iter)
        do (setf (aref ret (1+ index))
                 (funcall op (iterator-element iter) (aref ret index)))
           (setf iter (iterator-next iter))
           (incf index)
        finally (return ret)))

(defun cumsum (seq &key (element-type 'fixnum))
  (cumulate seq #'+ 0 :element-type element-type))

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

(defmacro defdp (name params &body body)
  `(defalias ,name (dp ,name ,params ,@body)))

;;;
;;; atcoder
;;;
(defpackage atcoder
  (:nicknames :ac)
  (:use :cl
        :utility
        :input
        :ordered-map
        :union-find
        :segment-tree
        :trie
        :graph
        :algorithm)
  (:export :main
           :test))
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

(defun test-case* (&rest input-expects)
  (cond ((null input-expects) nil)
        ((null (cdr input-expects)) (error "unmatched"))
        (t (test-case (car input-expects)
                      (cadr input-expects))
           (apply #'test-case* (cddr input-expects)))))

(defun main ()
  (input* ((a fixnum)
           (b fixnum))
    (format t "~A~%" (+ a b))))

(defun test ()
  (test-case* "1 2" "3"))

#-swank (main)

