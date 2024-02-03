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
           ;; control
           :nlet
           :alet
           :nlambda
           :alambda
           :if-let
           :if-let*
           :when-let
           :when-let*
           :aif
           :dovector
           :let-dyn
           ;; number
           :2*
           :/2
           :repunit
           ;; function
           :compose
           ;; sequence
           :emptyp
           :make-iterator
           :iterator-next
           :iterator-end-p
           :iterator-with-index
           ;; list
           :length-n-p
           :length1p
           :take
           :drop
           :longerp
           :longer
           :iota
           :unfold
           :unique
           ;; vector
           :dvector
           ;; string
           :split-string
           :string-prefix-p
           :string-suffix-p
           :strcat
           ;; io
           :println
           ;; lazy
           :delay
           :force
           ;; symbol
           :sym
           ))
(in-package utility)

;;; reader macro

(eval-when (:compile-toplevel :execute)
  (defun def-dispatch-fn (char fn)
    (set-dispatch-macro-character #\# char
                                  (lambda (stream char1 char2)
                                    (declare (ignorable stream char1 char2))
                                    (funcall fn
                                             (read stream t nil t))))))

(defmacro def-dispatch-macro (char params &body body)
  `(def-dispatch-fn ,char (lambda ,params ,@body)))

(eval-when (:compile-toplevel :execute)
  (def-dispatch-macro #\? (expr)
    (let ((value (gensym)))
      `(let ((,value ,expr))
         (fresh-line *error-output*)
         (format *error-output* "DEBUG PRINT: ~S => ~S~%" ',expr ,value)
         ,value))))

(eval-when (:compile-toplevel :execute)
  (let ((rpar (get-macro-character #\) )))
    (defun def-delimiter-macro-fn (left right fn)
      (set-macro-character right rpar)
      (set-dispatch-macro-character #\# left
                                    (lambda (stream char1 char2)
                                      (declare (ignorable stream char1 char2))
                                      (apply fn
                                             (read-delimited-list right stream t)))))))

(defmacro def-delimiter-macro (left right params &body body)
  `(def-delimiter-macro-fn ,left ,right #'(lambda ,params ,@body)))

(eval-when (:compile-toplevel :execute)
  (def-delimiter-macro #\[ #\] (arr &rest indecies)
    `(aref ,arr ,@indecies)))

;;; control

(defmacro nlet (name binds &body body)
  `(labels ((,name ,(mapcar #'car binds) ,@body))
     (,name ,@(mapcar #'cadr binds))))

(defmacro alet (binds &body body)
  `(nlet self ,binds ,@body))

(defmacro nlambda (name params &body body)
  `(labels ((,name ,params ,@body))
     #',name))

(defmacro alambda (params &body body)
  `(nlambda self ,params ,@body))

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

(defmacro dovector ((var init-form &optional result) &body body)
  (let ((vec (gensym))
        (index (gensym)))
    `(let ((,vec ,init-form))
       (dotimes (,index (length ,vec) ,result)
         (let ((,var (aref ,vec ,index)))
           (declare (ignorable ,var))
           ,@body)))))

(defmacro let-dyn (binds &body body)
  `(let ,binds
     (declare (dynamic-extent ,@(mapcar (lambda (x)
                                          (if (listp x) (car x) x))
                                        binds)))
     ,@body))

;;; number

(defun 2* (x) (* x 2))
(defun /2 (x) (values (floor x 2)))

(defun repunit (n &optional (base 10))
  (labels ((rec (n acc)
             (if (zerop n)
                 acc
                 (rec (1- n) (+ (* acc base) 1)))))
    (rec n 0)))
  
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

(defvar *iterator-end* (gensym "*ITERATOR-END*"))

(defun make-iterator (seq)
  (if (listp seq)
      (make-list-iterator seq)
      (make-array-iterator seq)))

(defun make-list-iterator (lst)
  (lambda (op)
    (cond ((eq op :next)
           (if (null lst)
               *iterator-end*
               (pop lst)))
          ((eq op :end)
           (null lst))
          (t (error (format nil "Unsupported method: ~S" op))))))

(defun make-array-iterator (arr)
  (let ((index 0)
        (len (length arr)))
    (lambda (op)
      (cond ((eq op :next)
             (if (= index len)
                 *iterator-end*
                 (let ((prev-index index))
                   (declare (dynamic-extent prev-index))
                   (incf index)
                   (aref arr prev-index))))
            ((eq op :end)
             (= index len))
            (t (error (format nil "Unsupported method: ~S" op)))))))

(defun iterator-with-index (iter)
  (let ((index 0))
    (lambda (op)
      (if (eq op :next)
          (let ((prev-index index))
            (declare (dynamic-extent prev-index))
            (incf index)
            (values (funcall iter :next) prev-index))
          (funcall iter op)))))

(defun iterator-next (iter)
  (funcall iter :next))

(defun iterator-end-p (iter)
  (funcall iter :end))

;;; list

(defun length-n-p (lst n)
  (cond ((zerop n) (null lst))
        ((null lst) nil)
        (t (length-n-p (cdr lst) (1- n)))))

(defun length1p (lst)
  (and lst
       (null (cdr lst))))

(defun take (lst len)
  (labels ((rec (lst len acc)
             (if (<= len 0)
                 (nreverse acc)
                 (rec (cdr lst) (1- len) (cons (car lst) acc)))))
    (rec lst len nil)))

(defun drop (lst len)
  (if (or (zerop len) (null lst))
      lst
      (drop (cdr lst) (1- len))))

(defun longerp (lst1 lst2)
  (nlet rec ((lst1 lst1) (lst2 lst2))
    (cond ((null lst1) nil)
          ((null lst2) t)
          (t (rec (cdr lst1) (cdr lst2))))))
  
(defun longer (lst1 lst2)
  (if (longerp lst1 lst2)
      lst1
      lst2))

(defun iota (count &optional (start 0) (step 1))
  (labels ((rec (count start step acc)
             (if (<= count 0)
                 (nreverse acc)
                 (rec (1- count) (+ start step) step (cons start acc)))))
    (rec count start step nil)))

(defun unfold (pred fn next-gen seed)
  (labels ((rec (p f g n acc)
             (if (funcall p n)
                 (nreverse acc)
                 (rec p f g (funcall g n) (cons (funcall f n) acc)))))
    (rec pred fn next-gen seed nil)))

(defun unique (lst &key (test #'eql))
  (nreverse (reduce (lambda (acc x)
                      (if (funcall test x (car acc))
                          acc
                          (cons x acc)))
                    lst
                    :initial-value nil)))

;;; vector

(defun dvector (&rest contents)
  (make-array (length contents)
              :initial-contents contents
              :adjustable t
              :fill-pointer t))

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun sym (&rest args)
    (values (intern
             (concatenate 'string
                          (with-output-to-string (s)
                            (mapcar (lambda (x) (princ x s))
                                    args)))
             *package*))))

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
;;          (v (vector (list fixnum1 fixnum1) n)))
;;   (list n v))
;; 3
;; 1 2
;; 3 4
;; 5 6
;; ; => (3 #((0 1) (2 3) (4 5)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *input-reader-table* (make-hash-table :test #'eq))

  (defun set-input-reader (marker reader)
    (setf (gethash marker *input-reader-table*) reader))

  (defun get-input-reader (marker)
    (gethash marker *input-reader-table*))

  (defun input-typespec-marker (typespec)
    (let ((*package* #.*package*))
      (sym (if (listp typespec)
               (car typespec)
               typespec))))

  (defun input-typespec-reader (typespec)
    (let ((marker (input-typespec-marker typespec)))
      (if (null marker)
          nil
          (funcall (get-input-reader marker) typespec))))

  (defun input-expand (forms)
    (if (null forms)
        nil
        (mapcar (lambda (form)
                  (list (car form)
                        (input-typespec-reader (cadr form))))
                forms)))
  )


(defmacro input* (forms &body body)
  `(let* ,(input-expand forms) ,@body))

(defmacro def-input-reader (marker params &body body)
  `(progn (set-input-reader
           ',marker
           (macrolet ((reader (typespec)
                        `(input-typespec-reader ,typespec)))
             (lambda (,@params &optional arg)
               (declare (ignore arg))
               ,@body)))
          ',marker))

(eval-when (:compile-toplevel :execute)
  (def-input-reader fixnum ()
    '(read))

  (def-input-reader fixnum1 ()
    '(1- (read)))

  (def-input-reader double ()
    '(let ((*read-default-float-format* 'double-float))
      (read)))

  (def-input-reader string ()
    `(read-line))

  (def-input-reader list (typespec)
    (let ((elems (cdr typespec)))
      (if (null elems)
          nil
          `(cons ,(reader (car elems))
                 ,(reader (cons 'list (cdr elems)))))))

  (def-input-reader vector (typespec)
    (let ((vec (gensym))
          (index (gensym))
          (elem (cadr typespec))
          (len (caddr typespec)))
      `(let ((,vec (make-array ,len)))
         (dotimes (,index ,len ,vec)
           (setf (aref ,vec ,index) ,(reader elem))))))
  )

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
             (let ((node-a (sym 'node- a))
                   (node-b (sym 'node- b))
                   (b-node (sym b '-node)))
               `(defun ,(sym 'rotate- a) (node)
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
             (let ((node-a (sym 'node- a))
                   (a-node (sym a '-node)))
               `(defun ,(sym 'node-insert- a) (node value key key-eq-p key-less-p)
                  (multiple-value-bind (,a-node needs-balance)
                      (node-insert (,node-a node) value key key-eq-p key-less-p)
                    (setf (,node-a node) ,a-node)
                    (,(sym 'balance-insert- a) node needs-balance))))))
  (%defun-node-insert left right)
  (%defun-node-insert right left))

(macrolet ((%defun-balance-insert (a b aa ab)
             (let ((node-a (sym 'node- a))
                   (node-b (sym 'node- b))
                   (rotate-a (sym 'rotate- a))
                   (rotate-b (sym 'rotate- b)))
               `(defun ,(sym 'balance-insert- a) (node needs-balance)
                  (if (or (not needs-balance) (node-red-p node))
                      (values node needs-balance)
                      (cond ((and (node-red-p (,node-b node))
                                  (or (,(sym 'invalid-red- aa '-p) node)
                                      (,(sym 'invalid-red- ab '-p) node)))
                             (setf node (split-4node node))
                             (values node t))
                            ((,(sym 'invalid-red- aa '-p) node)
                             (values (,rotate-b node) nil))
                            ((,(sym 'invalid-red- ab '-p) node)
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
    (cond ((= ar br) nil)
          ((< (union-find-rank uf ar)
              (union-find-rank uf br))
           (union-find-merge uf br ar))
          ((= (union-find-rank uf ar)
              (union-find-rank uf br))
           (incf (union-find-rank uf ar))
           (union-find-merge uf ar br))
          (t (setf (union-find-parent uf br) ar)
             (incf (union-find-group-size uf ar)
                   (union-find-group-size uf br))
             t))))

(defun union-find-unite-p (uf a b)
  (let ((ar (union-find-root uf a))
        (br (union-find-root uf b)))
    (= ar br)))

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
           ))
(in-package algorithm)

(defun meguru-method (ok ng pred)
  (if (<= (abs (- ok ng)) 1)
      ok
      (let ((mid (floor (+ ok ng) 2)))
        (if (funcall pred mid)
            (meguru-method mid ng pred)
            (meguru-method ok mid pred)))))

(defun cumulate (seq op id &key element-type)
  (let ((arr (make-array (1+ (length seq))
                         :element-type element-type
                         :initial-element id)))
    (loop with iter = (iterator-with-index (make-iterator seq))
          until (iterator-end-p iter)
          do (multiple-value-bind (element index) (iterator-next iter)
               (setf (aref arr (1+ index))
                     (funcall op element (aref arr index))))
          finally (return arr))))

(defun cumsum (seq &key (element-type 'fixnum))
  (cumulate seq #'+ 0 :element-type element-type))

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

(defun mod-add (a b m)
  (mod (+ a b m) m))

(defun main ()
  (input* ((h fixnum)
           (w fixnum)
           (n fixnum))
    (let ((grid (make-array h
                            :initial-contents
                            (loop for i from 0 below h
                                  collect (make-array w
                                                      :initial-element #\.
                                                      :element-type 'character)))))
      (let ((i 0)
            (j 0)
            (di 0)
            (dydxs #((-1 . 0) (0 . 1) (1 . 0) (0 . -1))))
        (dotimes (k n)
          (if (char= #\. (aref (aref grid i) j))
                (setf (aref (aref grid i) j) #\#
                      di (mod-add di 1 4))
                (setf (aref (aref grid i) j) #\.
                      di (mod-add di -1 4)))
          (let ((dydx (aref dydxs di)))
            (setf i (mod-add i (car dydx) h)
                  j (mod-add j (cdr dydx) w)))))
      (dotimes (i h)
        (format t "~A~%" (aref grid i))))))

(defun test ()
  (test-case "3 4 5" ".#..
##..
....
")
  (test-case "2 2 1000" "..
..
")
  (test-case "10 10 10" "##........
##........
..........
..........
..........
..........
..........
..........
..........
#........#
")
  )

#-swank (main)

