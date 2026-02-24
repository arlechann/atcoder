(in-package :cl-user)

(require :asdf)

#-swank
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :atcoder *features*))

#|
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
|#

;;;
;;; utility.syntax
;;;
(defpackage utility.syntax
  (:use :cl)
  (:import-from :uiop :if-let)
  (:export :it
           :self
           :eval-always
           :defun-always
           :defalias
           :defabbrev
           :symb
           :def-dispatch-macro
           :def-delimiter-macro
           :named-let
           :nlet
           :aif
           :alambda
           :aand
           :aprog1
           :if-let*
           :when-let
           :when-let*
           :do-array
           :do-array*
           :do-seq
           :do-seq*
           :do-bit
           :do-4neighbors
           :do-8neighbors
           :let-dyn
           :flet-accessor
           :if-let
           :dbind
           :mvcall
           :mvbind
           :mvlist
           :mvprog1
           :mvsetq
           ))
(in-package :utility.syntax)

(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(defmacro defun-always (name params &body body) `(eval-always (defun ,name ,params ,@body)))

(defmacro defalias (alias original)
  #-atcoder
  (unless (symbolp alias)
    (error "DEFALIAS: ALIAS must be a symbol literal, got ~S" alias))
  `(progn
     (setf (symbol-function ',alias) ,original)
     ',alias))

(defmacro defabbrev (short long) `(defmacro ,short (&rest args) `(,',long ,@args)))

(defun-always symb (&rest args)
  (values (intern (with-output-to-string (s)
                    (mapc (lambda (x) (princ x s)) args)))
          *package*))

(defun-always def-dispatch-fn (char fn)
  (set-dispatch-macro-character #\# char
                                (lambda (stream char1 char2)
                                  (declare (ignorable stream char1 char2))
                                  (funcall fn (read stream t nil t)))))

(defmacro def-dispatch-macro (char params &body body)
  `(eval-always
     (def-dispatch-fn ,char (lambda ,params ,@body))))

(eval-always
  (let ((rpar (get-macro-character #\))))
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

(def-dispatch-macro #\? (expr)
  (let ((value (gensym)))
    `(let ((,value ,expr))
       (fresh-line *error-output*)
       (format *error-output* "DEBUG PRINT: ~S => ~S~%" ',expr ,value)
       ,value)))

(defmacro named-let (name binds &body body)
  "クロージャに展開されるnamed-let"
  `(labels ((,name ,(mapcar #'car binds) ,@body))
     (,name ,@(mapcar #'cadr binds))))

(defmacro nlet (name binds &body body)
  "ループに展開されるnamed-let"
  (let ((tag (gensym))
        (vars (mapcar #'car binds))
        (vals (mapcar #'cadr binds))
        (tmp-vars (mapcar #'(lambda (bind)
                              (declare (ignore bind))
                              (gensym))
                          binds))
        (rec-args (mapcar #'(lambda (bind)
                          (declare (ignore bind))
                          (gensym))
                      binds)))
    `(block ,name
       (let ,(mapcar #'list tmp-vars vals)
         (tagbody
            ,tag
            (let ,(mapcar #'list vars tmp-vars)
              (return-from ,name
                (macrolet ((,name ,rec-args
                             `(progn (psetq ,@(mapcan #'list ',tmp-vars (list ,@rec-args)))
                                     (go ,',tag))))
                  ,@body))))))))

(defmacro until (test &body body)
  `(do () (,test) ,@body))

(defmacro while (test &body body)
  `(until (not ,test) ,@body))

(defmacro aif (test then &optional else)
  `(let ((it ,test)) (if it ,then ,else)))

(defmacro alambda (params &body body)
  `(labels ((self ,params ,@body)) #'self))

(defmacro aand (&body body)
  (cond ((null body) 't)
        ((null (cdr body)) (car body))
        (t `(let ((it ,(car body)))
              (and it (aand ,@(cdr body)))))))

(defmacro aprog1 (result &body body) `(let ((it ,result)) (prog1 it ,@body)))

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
  `(block nil
     (map nil
          (lambda ,vars
            (declare (ignorable ,@vars))
            ,@body)
          ,@sequences)
     ,result))

(defmacro do-seq ((var sequence &optional result) &body body)
  `(do-seq* ((,var) (,sequence) ,result) ,@body))

(defmacro do-bit ((index bitpopp integer &optional result) &body body)
  (let ((int (gensym)))
    `(do* ((,int ,integer (ash ,int -1))
           (,index 0 (1+ ,index))
           (,bitpopp (logbitp 0 ,int) (logbitp 0 ,int)))
          ((<= ,int 0) ,result)
       ,@body)))

(macrolet ((def-do-neighbors (name neighbors-y neighbors-x)
             `(defmacro ,name (((var-y var-x) (point-y point-x) &optional result) &body body)
                (let ((dy (gensym))
                      (dx (gensym)))
                  `(loop for ,dy in ,',neighbors-y
                         and ,dx in ,',neighbors-x
                         for ,var-y = (+ ,dy ,point-y)
                         and ,var-x = (+ ,dx ,point-x)
                         do (progn ,@body)
                         finally (return ,result))))))
  (def-do-neighbors do-4neighbors '(0 1 0 -1) '(1 0 -1 0))
  (def-do-neighbors do-8neighbors '(0 1 1 1 0 -1 -1 -1) '(1 1 0 -1 -1 -1 0 1)))
      
(defmacro let-dyn (binds &body body)
  `(let ,binds
     (declare (dynamic-extent
               ,@(mapcar (lambda (x) (if (listp x) (car x) x)) binds)))
     ,@body))

(defmacro flet-accessor (definitions &body body)
  (let ((value (gensym)))
    `(flet ,(mapcan #'(lambda (definition)
                        (let ((name (car definition))
                              (params (cadr definition))
                              (accessor (caddr definition)))
                          (list (list name params accessor)
                                (list (list 'setf name) (cons value params)
                                (list 'setf accessor value)))))
                    definitions)
       ,@body)))

(defabbrev dbind destructuring-bind)
(defabbrev mvcall multiple-value-call)
(defabbrev mvbind multiple-value-bind)
(defabbrev mvlist multiple-value-list)
(defabbrev mvprog1 multiple-value-prog1)
(defabbrev mvsetq multiple-value-setq)

;;;
;;; utility.number
;;;
(defpackage utility.number
  (:use :cl :utility.syntax)
  (:export ;; number
           :2*
           :/2
           :square
           :cube
           :cuber
           :pow
           :to-integer
           :exgcd
           :combinatorics-table
           :make-combinatorics-table
           :combinatorics-fact
           :combinatorics-ifact
           :combinatorics-nck
           :combinatorics-npk
           :diff
           :triangular-number
           :next-pow2
           :repunit
           :maxp
           :minp
           :maxf
           :minf
           :logipop
           :logmsb
           :approx=
           :approx-zero-p
           :approx<=
           :approx>=
           :range-intersect-p
           ))
(in-package :utility.number)

(declaim (inline onep 2* /2 square cube cuber
                 diff triangular-number repunit maxp minp logmsb
                 approx= approx-zero-p approx<= approx>=))

(declaim (ftype (function (number) t) onep))
(defun onep (x) (= x 1))
(declaim (ftype (function (number) number) 2*))
(defun 2* (x) (* x 2))
(declaim (ftype (function (real) integer) /2))
(defun /2 (x) (values (floor x 2)))

(declaim (ftype (function (t &optional (function (t t) t)) t) square))
(defun square (x &optional (op #'*)) (funcall op x x))
(declaim (ftype (function (t &optional (function (t t) t)) t) cube))
(defun cube (x &optional (op #'*)) (funcall op (funcall op x x) x))
(declaim (ftype (function (t (function (t t) t)) t) cuber))
(defun cuber (x op) (funcall op x (funcall op x x)))

(defgeneric to-integer (x)
  (:documentation "Convert X to an integer value."))

(defmethod to-integer ((x integer))
  x)

(declaim (ftype (function (t unsigned-byte &key (:op (function (t t) t)) (:identity t)) t) pow))
(defun pow (base power &key (op #'*) (identity 1))
  (nlet rec ((base base) (power power) (ret identity))
    (cond ((zerop power) ret)
          ((oddp power)
           (rec (square base op) (floor power 2) (funcall op ret base)))
          (t (rec (square base op) (floor power 2) ret)))))

(declaim (ftype (function (number number) (real 0 *)) diff))
(defun diff (a b) (abs (- a b)))

(declaim (ftype (function (unsigned-byte) unsigned-byte) triangular-number))
(defun triangular-number (n)
  (values (floor (* n (1+ n)) 2)))

(declaim (ftype (function (unsigned-byte) unsigned-byte) next-pow2))
(defun next-pow2 (n)
  (declare (type unsigned-byte n))
  (if (zerop (logand n (1- n))) n
      (nlet rec ((n n) (acc 1))
        (if (zerop n)
            acc
            (rec (ash n -1) (ash acc 1))))))

(declaim (ftype (function (unsigned-byte &optional unsigned-byte) unsigned-byte) repunit))
(defun repunit (n &optional (base 10))
  (declare (type unsigned-byte n base))
  (nlet rec ((n n) (acc 0))
    (if (zerop n) acc
        (rec (1- n) (+ (* base acc) 1)))))

(declaim (ftype (function (real &rest real) boolean) maxp))
(defun maxp (x &rest args)
  (or (null args)
      (> x (apply #'max args))))
(declaim (ftype (function (real &rest real) boolean) minp))
(defun minp (x &rest args)
  (or (null args)
      (< x (apply #'min args))))
(defmacro maxf (place &rest args) `(setf ,place (max ,place ,@args)))
(defmacro minf (place &rest args) `(setf ,place (min ,place ,@args)))

(declaim (ftype (function (unsigned-byte &rest unsigned-byte) unsigned-byte) logipop))
(defun logipop (n &rest indexes)
  (declare (type unsigned-byte n))
  (dolist (index indexes n)
    (declare (type unsigned-byte index))
    (setf n (logior n (ash 1 index)))))

(declaim (ftype (function (unsigned-byte) (integer -1 *)) logmsb))
(defun logmsb (n)
  (nlet rec ((i 0) (k 1))
    (cond ((= k n) i)
          ((> k n) (1- i))
          (t (rec (1+ i) (ash k 1))))))

(declaim (ftype (function (real real &key (:eps real)) boolean) approx=))
(defun approx= (x y &key (eps 1d-12))
  (<= (abs (- x y)) eps))

(declaim (ftype (function (real &key (:eps real)) boolean) approx-zero-p))
(defun approx-zero-p (x &key (eps 1d-12))
  (<= (abs x) eps))

(declaim (ftype (function (real real &key (:eps real)) boolean) approx<=))
(defun approx<= (x y &key (eps 1d-12))
  (or (< x y)
      (approx= x y :eps eps)))

(declaim (ftype (function (real real &key (:eps real)) boolean) approx>=))
(defun approx>= (x y &key (eps 1d-12))
  (or (> x y)
      (approx= x y :eps eps)))

(declaim (ftype (function (real real real real &key (:touchp t)) boolean)))
(defun range-intersect-p (a1 a2 b1 b2 &key touchp)
  (let ((comp (if touchp #'< #'<=)))
    (not (or (funcall comp (max a1 a2) (min b1 b2))
             (funcall comp (max b1 b2) (min a1 a2))))))

(let* ((dp-combination-max 0)
       (memo (make-array (list dp-combination-max dp-combination-max)
                         :element-type 'unsigned-byte
                         :initial-element 0)))
  (defun set-dp-combination-max (max)
    (setf dp-combination-max max)
    (setf memo (make-array (list dp-combination-max dp-combination-max)
                           :element-type 'unsigned-byte
                           :initial-element 0))
    (proclaim `(ftype (function ((mod ,dp-combination-max) (mod ,dp-combination-max))
                                unsigned-byte)
                      dp-combination)))
  (set-dp-combination-max 2501)
  (defun dp-combination (n k)
    (cond ((< n k) 0)
          ((= n k) 1)
          ((zerop k) 1)
          (t (let ((memo-value (aref memo n k)))
               (if (> memo-value 0)
                   memo-value
                   (setf (aref memo n k)
                         (+ (dp-combination (1- n) (1- k))
                            (dp-combination (1- n) k)))))))))

;;;
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
;;; list-queue
;;;
(defpackage list-queue
  (:use :cl :utility.syntax :utility.number :utility.base)
  (:export :make-list-queue
           :list-queue-empty-p
           :list-queue-peek
           :list-queue-raw
           :list-queue-enqueue
           :list-queue-dequeue
           ))
(in-package list-queue)

(defun make-list-queue ()
  "空の連結リストキューを生成して返す。"
  (cons nil nil))
(defun list-queue-empty-p (queue)
  "QUEUE が空なら真を返す。"
  (null (car queue)))

(defun list-queue-peek (queue)
  "先頭要素を返す（削除しない）。"
  #-atcoder
  (when (list-queue-empty-p queue)
    (error "LIST-QUEUE is empty. Cannot peek any element."))
  (caar queue))

(defun list-queue-raw (queue)
  "内部リスト（先頭から末尾まで）を返す。"
  (car queue))

(defun list-queue-enqueue (queue value)
  "末尾に VALUE を追加し、QUEUE を返す。"
  (let ((new-cell (cons value nil)))
    (if (list-queue-empty-p queue)
        (setf (car queue) new-cell
              (cdr queue) new-cell)
        (setf (cddr queue) new-cell
              (cdr queue) new-cell))
    queue))

(defun list-queue-dequeue (queue)
  "先頭要素を取り出して返す。"
  #-atcoder
  (when (list-queue-empty-p queue)
    (error "LIST-QUEUE is empty. Cannot dequeue any element."))
  (prog1 (caar queue)
    (or (setf (car queue) (cdar queue))
        (setf (cdr queue) nil))))

;;;
;;; utility.window
;;;
(defpackage utility.window
  (:use :cl :utility.base)
  (:export ;; sequence
           :window-map
           :window-nmap))
(in-package :utility.window)

;;; sequence

(declaim (ftype (function ((or cons symbol class)
                           (integer 1 *)
                           (or (function (t &rest t) t) symbol)
                           sequence)
                          sequence)
                window-map))
(let ((t-fn (constantly t)))
  (defun window-map (result-type window-size fn sequence)
    (let ((len (length sequence)))
      (declare (ignorable len))
      #-atcoder
      (when (> window-size len)
        (error "WINDOW-MAP: window-size ~D exceeds sequence length ~D." window-size len))
      (let ((result (let ((index 0)
                          (queue (list-queue:make-list-queue)))
                      (map result-type
                           (lambda (e)
                             (list-queue:list-queue-enqueue queue e)
                             (incf index)
                             (when (>= index window-size)
                               (prog1 (apply fn (list-queue:list-queue-raw queue))
                                 (list-queue:list-queue-dequeue queue))))
                           sequence))))
        (and result
             (delete-if t-fn
                        result
                        :end (1- window-size)))))))

(declaim (ftype (function ((integer 1 *)
                           (or (function (t &rest t) t) symbol)
                           sequence)
                          sequence)
                window-nmap))
(let ((t-fn (constantly t)))
  (defun window-nmap (window-size fn sequence)
    (let ((len (length sequence)))
      (declare (ignorable len))
      #-atcoder
      (when (> window-size len)
        (error "WINDOW-NMAP: window-size ~D exceeds sequence length ~D." window-size len))
      (delete-if t-fn
                 (let ((index 0)
                       (queue (list-queue:make-list-queue)))
                   (utility.base::nmap
                    (lambda (e)
                      (list-queue:list-queue-enqueue queue e)
                      (incf index)
                      (when (>= index window-size)
                        (prog1 (apply fn (list-queue:list-queue-raw queue))
                          (list-queue:list-queue-dequeue queue))))
                    sequence))
                 :end (1- window-size)))))

;;;
;;; utility
;;;
(defpackage utility
  (:use :cl
        :utility.syntax
        :utility.number
        :utility.base
        :utility.window))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (dolist (src '(:utility.syntax
                 :utility.number
                 :utility.base
                 :utility.window))
    (do-external-symbols (sym src)
      (export sym :utility))))

;;;
;;; utility.number math
;;;
(in-package :utility.number)

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

;;;
(defpackage match
  (:use :cl :utility)
  (:export :if-match
           :match))
(in-package :match)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun match-wildcard-p (pat)
    (and (symbolp pat)
         (string= (symbol-name pat) "_")))

  (defun match-variable-p (pat)
    (and (symbolp pat)
         (not (keywordp pat))
         (not (member pat '(t nil)))
         (not (match-wildcard-p pat))))

  (defun match-list-pattern-p (pat)
    (and (consp pat) (eq (car pat) 'list)))

  (defun match-list*-pattern-p (pat)
    (and (consp pat) (eq (car pat) 'list*)))

  (defun match-cons-pattern-p (pat)
    (and (consp pat) (eq (car pat) 'cons)))

  (defun match-quote-pattern-p (pat)
    (and (consp pat) (eq (car pat) 'quote) (consp (cdr pat)) (null (cddr pat))))

  (defun match-guard-pattern-p (pat)
    (and (consp pat) (eq (car pat) 'guard) (consp (cdr pat)) (consp (cddr pat))))

  (defun match-or-pattern-p (pat)
    (and (consp pat) (eq (car pat) 'or) (consp (cdr pat))))

  (defun list-pattern->cons-pattern (items)
    (if (null items)
        nil
        `(cons ,(car items) ,(list-pattern->cons-pattern (cdr items)))))

  (defun list*-pattern->cons-pattern (items)
    (cond ((null items) nil)
          ((null (cdr items)) (car items))
          (t `(cons ,(car items) ,(list*-pattern->cons-pattern (cdr items))))))

  (defun binding-vars (binds)
    (remove-duplicates (mapcar #'car binds)))

  (defun same-binding-vars-p (var-list-1 var-list-2)
    (and (null (set-difference var-list-1 var-list-2))
         (null (set-difference var-list-2 var-list-1))))

  (defun compile-pattern (pat target env)
    (cond
      ((match-wildcard-p pat)
       (values t nil env))

      ((match-variable-p pat)
       (let ((bound (assoc pat env)))
         (if bound
             (values `(equal ,(cdr bound) ,target) nil env)
             (values t `((,pat ,target)) (acons pat target env)))))

      ((match-quote-pattern-p pat)
       (values `(equal ',(cadr pat) ,target) nil env))

      ((match-cons-pattern-p pat)
       (multiple-value-bind (test-car binds-car env1)
           (compile-pattern (cadr pat) `(car ,target) env)
         (multiple-value-bind (test-cdr binds-cdr env2)
             (compile-pattern (caddr pat) `(cdr ,target) env1)
           (values
            `(and (consp ,target) ,test-car ,test-cdr)
            (append binds-car binds-cdr)
            env2))))

      ((match-list-pattern-p pat)
       (compile-pattern (list-pattern->cons-pattern (cdr pat)) target env))

      ((match-list*-pattern-p pat)
       (compile-pattern (list*-pattern->cons-pattern (cdr pat)) target env))

      ((match-guard-pattern-p pat)
       (multiple-value-bind (test binds env1)
           (compile-pattern (cadr pat) target env)
         (values `(and ,test
                       (let* ,binds
                         (declare (ignorable ,@(mapcar #'car binds)))
                         ,@(cddr pat)))
                 binds
                 env1)))

      ((match-or-pattern-p pat)
       (let ((compiled nil))
         (dolist (subpat (cdr pat))
           (multiple-value-bind (test binds subenv)
               (compile-pattern subpat target env)
             (declare (ignore subenv))
             (push (list test binds) compiled)))
         (setf compiled (nreverse compiled))
         (let* ((tests (mapcar #'first compiled))
                (binds-list (mapcar #'second compiled))
                (base-vars (binding-vars (first binds-list))))
           (dolist (binds (rest binds-list))
             (unless (same-binding-vars-p base-vars (binding-vars binds))
               (error "OR pattern branches must bind same variables: ~S" pat)))
           (let ((merged-binds
                   (mapcar (lambda (var)
                             (list var
                                   `(cond
                                      ,@(mapcar (lambda (entry)
                                                  (let ((test (first entry))
                                                        (binds (second entry)))
                                                    (list test (second (assoc var binds)))))
                                                compiled)
                                      (t nil))))
                           base-vars)))
             (values `(or ,@tests) merged-binds env)))))

      ((consp pat)
       (compile-pattern (list-pattern->cons-pattern pat) target env))

      (t
       (values `(equal ',pat ,target) nil env)))))

  (defun compile-match-clause (target clause)
    (let ((pattern (car clause))
          (body (cdr clause)))
      (if (eq pattern 'otherwise)
          `(t ,@body)
          (multiple-value-bind (test bindings env)
              (compile-pattern pattern target nil)
            (declare (ignore env))
            `(,test
              (let* ,bindings
                (declare (ignorable ,@(mapcar #'car bindings)))
                ,@body))))))

(defmacro if-match (pattern expr then &optional else)
  "Match EXPR against PATTERN. If matched, evaluate THEN, else evaluate ELSE.

Patterns:
  _                      wildcard
  symbol                 variable binding
  atom / 'literal        literal equality
  (or p1 ... pn)         OR-pattern (all branches must bind same vars)
  (cons p1 p2)           cons pattern
  (list p1 ... pn)       proper list pattern
  (list* p1 ... tail)    dotted list pattern
  (guard pat pred...)    match PAT first, then evaluate predicates"
  (let ((value (gensym "VALUE-")))
    (multiple-value-bind (test bindings env)
        (compile-pattern pattern value nil)
      (declare (ignore env))
      `(let ((,value ,expr))
         (if ,test
             (let* ,bindings
               (declare (ignorable ,@(mapcar #'car bindings)))
               ,then)
             ,else)))))

(defmacro match (expr &rest clauses)
  "Pattern matching on EXPR.

Usage:
  (match expr
    ((list x y) (+ x y))
    ((or nil (list _)) :singleton-or-nil)
    ((guard (cons x _) (> x 0)) x)
    (otherwise nil))"
  (let ((value (gensym "VALUE-")))
    `(let ((,value ,expr))
       (cond
         ,@(mapcar (lambda (clause)
                     (compile-match-clause value clause))
                   clauses)))))
;;; mint
;;;
(defpackage :mint
  (:use :cl :utility.number)
  (:export :*mint-modulus*
           :with-mint-modulus
           :mint
           :mint-p
           :make-mint
           :to-mint
           :mint-value
           :mint+
           :mint-
           :mint*
           :mint/
           :mint-inv
           :mint-pow
           :make-mint-combinatorics
           :mint-fact
           :mint-ifact
           :mint-nck
           :mint-npk))
(in-package :mint)

(defparameter *mint-modulus* 998244353)

(defstruct (mint
            (:constructor %make-mint (value)))
  (value 0 :type fixnum))

(declaim (inline normalize-mint-value))
(defun normalize-mod (value modulus)
  #-atcoder
  (unless (and (integerp modulus) (> modulus 0))
    (error "NORMALIZE-MOD: modulus must be a positive integer, got ~S." modulus))
  (mod value modulus))

(defun normalize-mint-value (x)
  (the fixnum (normalize-mod x *mint-modulus*)))

(defun mod-inv (value modulus)
  (multiple-value-bind (g x _y)
      (exgcd (normalize-mod value modulus) modulus)
    (declare (ignore _y))
    (when (= (abs g) 1)
      (normalize-mod x modulus))))

(defmethod to-integer ((x mint))
  (mint-value x))

(defgeneric to-mint (x)
  (:documentation "Convert X to mint."))

(defmethod to-mint ((x mint))
  x)

(defmethod to-mint ((x integer))
  (%make-mint (normalize-mint-value x)))

(defmethod to-mint ((x rational))
  (let* ((num (numerator x))
         (den (denominator x))
         (inv (mod-inv den *mint-modulus*)))
    #-atcoder
    (unless inv
      (error "TO-MINT: denominator ~S has no inverse under modulus ~S." den *mint-modulus*))
    (make-mint (* num (or inv 0)))))

(defun make-mint (x)
  (%make-mint (normalize-mint-value (to-integer x))))

(defun mint+ (&rest xs)
  (make-mint (reduce #'+ xs :initial-value 0 :key #'to-integer)))

(defun mint- (x &rest more)
  (if (null more)
      (make-mint (- (to-integer x)))
      (make-mint (reduce #'- more :initial-value (to-integer x) :key #'to-integer))))

(defun mint* (&rest xs)
  (make-mint (reduce #'* xs :initial-value 1 :key #'to-integer)))

(defun mint-inv (x)
  (let* ((a (normalize-mint-value (to-integer x)))
         (m *mint-modulus*)
         (inv (mod-inv a m)))
      #-atcoder
      (unless inv
        (error "MINT-INV: inverse does not exist for ~S under modulus ~S." a m))
      (make-mint (or inv 0))))

(defun mint/ (x &rest ys)
  (reduce (lambda (acc y) (mint* acc (mint-inv y)))
          ys
          :initial-value (to-mint x)))

(defun mint-pow (x n)
  #-atcoder
  (unless (and (integerp n) (<= 0 n))
    (error "MINT-POW: exponent must be a non-negative integer, got ~S." n))
  (pow (to-mint x) n :op #'mint* :identity (make-mint 1)))

(defun make-mint-combinatorics (max-n)
  (make-combinatorics-table
   max-n
   :one (make-mint 1)
   :zero (make-mint 0)
   :mul #'mint*
   :inv #'mint-inv
   :from-integer #'make-mint))

(defun mint-fact (table n)
  (combinatorics-fact table n))

(defun mint-ifact (table n)
  (combinatorics-ifact table n))

(defun mint-nck (table n k)
  (combinatorics-nck table n k))

(defun mint-npk (table n k)
  (combinatorics-npk table n k))

(defmacro with-mint-modulus ((modulus) &body body)
  `(let ((*mint-modulus* ,modulus))
     #-atcoder
     (unless (and (typep *mint-modulus* 'fixnum) (> *mint-modulus* 0))
       (error "WITH-MINT-MODULUS: modulus must be a positive fixnum, got ~S." *mint-modulus*))
     ,@body))

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
           :deque-peek-front
           :deque-peek-back
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

(setf (documentation 'make-deque 'function)
      "空の deque を生成して返す。")
(setf (documentation 'deque-size 'function)
      "deque の要素数を返す。")

(defun deque-empty-p (deque)
  "DEQUE が空なら真を返す。"
  (zerop (deque-size deque)))
(defun deque-full-p (deque) (= (deque-size deque) (deque-capacity deque)))

(defun deque-index-in-range-p (deque index)
  (and (integerp index)
       (<= 0 index)
       (< index (deque-size deque))))

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
  "先頭に X を追加し、DEQUE を返す。"
  (when (deque-full-p deque)
    (deque-extends-buffer deque))
  (dec-front-index deque)
  (incf (deque-size deque))
  (setf (deque-front deque) x)
  deque)

(defun deque-push-back (deque x)
  "末尾に X を追加し、DEQUE を返す。"
  (when (deque-full-p deque)
    (deque-extends-buffer deque))
  (inc-back-index deque)
  (incf (deque-size deque))
  (setf (deque-back deque) x)
  deque)

(defun deque-pop-front (deque)
  "先頭要素を削除し、DEQUE を返す。"
  #-atcoder
  (when (deque-empty-p deque)
    (error "DEQUE is empty. Cannot pop any element."))
  (setf (deque-buffer-ref deque (deque-front-index deque)) nil)
  (inc-front-index deque)
  (decf (deque-size deque))
  deque)

(defun deque-pop-back (deque)
  "末尾要素を削除し、DEQUE を返す。"
  #-atcoder
  (when (deque-empty-p deque)
    (error "DEQUE is empty. Cannot pop any element."))
  (setf (deque-buffer-ref deque (deque-back-index deque)) nil)
  (dec-back-index deque)
  (decf (deque-size deque))
  deque)

(defun deque-peek-front (deque)
  "先頭要素を返す（削除しない）。"
  #-atcoder
  (when (deque-empty-p deque)
    (error "DEQUE is empty. Cannot peek front."))
  (deque-front deque))

(defun deque-peek-back (deque)
  "末尾要素を返す（削除しない）。"
  #-atcoder
  (when (deque-empty-p deque)
    (error "DEQUE is empty. Cannot peek back."))
  (deque-back deque))


(defun deque-ref (deque index)
  "先頭から INDEX 番目（0始まり）の要素を返す。"
  #-atcoder
  (unless (deque-index-in-range-p deque index)
    (error "DEQUE-REF index out of range: ~S (size=~S)." index (deque-size deque)))
  (deque-buffer-ref deque
                    (round-index (deque-capacity deque)
                                 (+ (deque-front-index deque)
                                    index))))

(defun (setf deque-ref) (x deque index)
  #-atcoder
  (unless (deque-index-in-range-p deque index)
    (error "(SETF DEQUE-REF) index out of range: ~S (size=~S)." index (deque-size deque)))
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
           :bintree-capacity
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
  (declare (ignore element-type initial-element))
  (let ((default-fill-pointer (if initial-contents
                                  (length initial-contents)
                                  0)))
    (apply #'make-array
           size
           :adjustable t
           :fill-pointer default-fill-pointer
           args)))

(defun bintree-size (bt)
  (if (array-has-fill-pointer-p bt) (fill-pointer bt) (length bt)))

(defun bintree-capacity (bt)
  (array-total-size bt))

(defun bintree-ref (bt index) (aref bt index))
(defun (setf bintree-ref) (value bt index) (setf (aref bt index) value))

(defun bintree-push (value bintree)
  #-atcoder
  (unless (array-has-fill-pointer-p bintree)
    (error "BINTREE-PUSH requires an array with fill-pointer."))
  (vector-push-extend value bintree))

(defun bintree-pop (bintree)
  #-atcoder
  (unless (array-has-fill-pointer-p bintree)
    (error "BINTREE-POP requires an array with fill-pointer."))
  (vector-pop bintree))

(defun bintree-root-index-p (index) (zerop index))
(defun bintree-left-index (index) (1+ (* index 2)))
(defun bintree-right-index (index) (+ 2 (* index 2)))
(defun bintree-parent-index (index) (values (floor (1- index) 2)))

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

(defun downheap (bintree index op)
  (labels ((downheap-swap (index child-index)
             (unless (funcall op (bintree-ref bintree index) (bintree-ref bintree child-index))
               (rotatef (bintree-ref bintree index) (bintree-ref bintree child-index))
               (downheap-rec child-index)))
           (downheap-rec (index)
             (let ((size (bintree-size bintree))
                   (left-index (bintree-left-index index))
                   (right-index (bintree-right-index index)))
               (when (>= left-index size)
                 (return-from downheap-rec))
               (when (>= right-index size)
                 (downheap-swap index left-index)
                 (return-from downheap-rec))
               (if (funcall op (bintree-ref bintree left-index) (bintree-ref bintree right-index))
                   (downheap-swap index left-index)
                   (downheap-swap index right-index)))))
    (downheap-rec index)))

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

(declaim (inline node-empty-p node-color node-left node-value node-right node-black-p node-red-p))

(defun make-node (&key color left value right) (list color left value right))
(defun make-empty-node () nil)
(defun node-empty-p (node) (null node))
(defun node-color (node) (if (node-empty-p node) 'black (first node)))
(defun node-left (node) (second node))
(defun node-value (node) (third node))
(defun node-right (node) (fourth node))

(defun (setf node-color) (color node)
  (if (node-empty-p node)
      (progn
        #-atcoder
        (unless (eq color 'black)
          (error "Leaf is must be black."))
        'black)
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
      (values default nil)
      (let ((node-key (funcall key (node-value node))))
        (match:match (list (funcall key-eq-p search-key node-key)
                           (funcall key-less-p search-key node-key))
          ((list t _) (values (node-value node) t))
          ((list nil t)
           (node-search (node-left node)
                        search-key
                        key
                        key-eq-p
                        key-less-p
                        :default default))
          (otherwise
           (node-search (node-right node)
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

(declaim (ftype (function (t t t t t) (values t boolean))
                node-insert-left
                node-insert-right
                node-remove-here
                node-remove-left
                node-remove-right))
(declaim (ftype (function (t boolean) (values t boolean))
                balance-insert-left
                balance-insert-right
                balance-remove-left
                balance-remove-right))

(defun node-insert (node value key key-eq-p key-less-p)
  (if (node-empty-p node)
      (values (make-node :color 'red :value value) t)
      (let ((new-key (funcall key value))
            (node-key (funcall key (node-value node))))
        (match:match (list (funcall key-eq-p new-key node-key)
                           (funcall key-less-p new-key node-key))
          ((list t _)
           (setf (node-value node) value)
           (values node nil))
          ((list nil t)
           (node-insert-left node value key key-eq-p key-less-p))
          (otherwise
           (node-insert-right node value key key-eq-p key-less-p))))))

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

(defun balance-insert-left (node needs-balance)
  (if (or (not needs-balance) (eq (node-color node) 'red))
      (values node needs-balance)
      (match:match node
        ((list _ (list 'red (list 'red _ _ _) _ _) _ (list 'red _ _ _))
         (setf node (split-4node node))
         (values node t))
        ((list _ (list 'red _ _ (list 'red _ _ _)) _ (list 'red _ _ _))
         (setf node (split-4node node))
         (values node t))
        ((list _ (list 'red (list 'red _ _ _) _ _) _ _)
         (values (rotate-right node) nil))
        ((list _ (list 'red _ _ (list 'red _ _ _)) _ _)
         (setf (node-left node) (rotate-left (node-left node)))
         (values (rotate-right node) nil))
        (otherwise
         (values node nil)))))

(defun balance-insert-right (node needs-balance)
  (if (or (not needs-balance) (eq (node-color node) 'red))
      (values node needs-balance)
      (match:match node
        ((list _ (list 'red _ _ _) _ (list 'red _ _ (list 'red _ _ _)))
         (setf node (split-4node node))
         (values node t))
        ((list _ (list 'red _ _ _) _ (list 'red (list 'red _ _ _) _ _))
         (setf node (split-4node node))
         (values node t))
        ((list _ _ _ (list 'red _ _ (list 'red _ _ _)))
         (values (rotate-left node) nil))
        ((list _ _ _ (list 'red (list 'red _ _ _) _ _))
         (setf (node-right node) (rotate-right (node-right node)))
         (values (rotate-left node) nil))
        (otherwise
         (values node nil)))))

(defun node-remove (node remove-key key key-eq-p key-less-p)
  (if (node-empty-p node)
      (values node nil)
      (let ((node-key (funcall key (node-value node))))
        (match:match (list (funcall key-eq-p remove-key node-key)
                           (funcall key-less-p remove-key node-key))
          ((list t _)
           (node-remove-here node remove-key key key-eq-p key-less-p))
          ((list nil t)
           (node-remove-left node remove-key key key-eq-p key-less-p))
          (otherwise
           (node-remove-right node remove-key key key-eq-p key-less-p))))))

(defun node-remove-here (node remove-key key key-eq-p key-less-p)
  (declare (ignore remove-key))
  (let ((lnode (node-left node))
        (rnode (node-right node)))
    (match:match (list (node-empty-p lnode) (node-empty-p rnode))
      ((list t t) (values (make-empty-node) (node-black-p node)))
      ((list t nil)
       (setf (node-color rnode) 'black)
       (values rnode nil))
      ((list nil t)
       (setf (node-color lnode) 'black)
       (values lnode nil))
      (otherwise
       (let ((first-node (node-first rnode)))
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
      (match:match node
        ((list _ _ _ nil)
         (values node needs-balance))
        ((list 'black left _ (list 'black rleft _ rright))
         (if (and (match:if-match (or nil (list 'black _ _ _)) left t nil)
                  (match:if-match (or nil (list 'black _ _ _)) rleft t nil)
                  (match:if-match (or nil (list 'black _ _ _)) rright t nil))
             (progn
               (setf (node-color (node-right node)) 'red)
               (values node t))
             (if (and (match:if-match (list 'red _ _ _) rleft t nil)
                      (match:if-match (or nil (list 'black _ _ _)) rright t nil))
                 (progn
                   (setf (node-right node) (rotate-right (node-right node)))
                   (balance-remove-left node t))
                 (let ((new-node (rotate-left node)))
                   (setf (node-color (node-left new-node)) 'black
                         (node-color (node-right new-node)) 'black)
                   (values new-node nil)))))
        ((list 'black left _ (list 'red _ _ _))
         (if (match:if-match (or nil (list 'black _ _ _)) left t nil)
             (let ((new-node (rotate-left node)))
               (multiple-value-bind (new-left needs-balance)
                   (balance-remove-left (node-left new-node) t)
                 (setf (node-left new-node) new-left)
                 (balance-remove-left new-node needs-balance)))
             (let ((new-node (rotate-left node)))
               (setf (node-color (node-left new-node)) 'black
                     (node-color (node-right new-node)) 'black)
               (values new-node nil))))
        ((list 'red _ _ (list _ rleft _ rright))
         (if (and (match:if-match (or nil (list 'black _ _ _)) rleft t nil)
                  (match:if-match (or nil (list 'black _ _ _)) rright t nil))
             (progn
               (rotatef (node-color node) (node-color (node-right node)))
               (values node nil))
             (let ((new-node (rotate-left node)))
               (setf (node-color (node-left new-node)) 'black
                     (node-color (node-right new-node)) 'black)
               (values new-node nil))))
        (otherwise
         (let ((new-node (rotate-left node)))
           (setf (node-color (node-left new-node)) 'black
                 (node-color (node-right new-node)) 'black)
           (values new-node nil))))))

(defun node-remove-right (node remove-key key key-eq-p key-less-p)
  (multiple-value-bind (new-right needs-balance)
      (node-remove (node-right node) remove-key key key-eq-p key-less-p)
    (setf (node-right node) new-right)
    (balance-remove-right node needs-balance)))

(defun balance-remove-right (node needs-balance)
  (if (not needs-balance)
      (values node nil)
      (match:match node
        ((list _ nil _ _)
         (values node needs-balance))
        ((list 'black (list 'black lleft _ lright) _ right)
         (if (and (match:if-match (or nil (list 'black _ _ _)) right t nil)
                  (match:if-match (or nil (list 'black _ _ _)) lleft t nil)
                  (match:if-match (or nil (list 'black _ _ _)) lright t nil))
             (progn
               (setf (node-color (node-left node)) 'red)
               (values node t))
             (if (and (match:if-match (list 'red _ _ _) lright t nil)
                      (match:if-match (or nil (list 'black _ _ _)) lleft t nil))
                 (progn
                   (setf (node-left node) (rotate-left (node-left node)))
                   (balance-remove-right node t))
                 (let ((new-node (rotate-right node)))
                   (setf (node-color (node-left new-node)) 'black
                         (node-color (node-right new-node)) 'black)
                   (values new-node nil)))))
        ((list 'black (list 'red _ _ _) _ right)
         (if (match:if-match (or nil (list 'black _ _ _)) right t nil)
             (let ((new-node (rotate-right node)))
               (multiple-value-bind (new-right needs-balance)
                   (balance-remove-right (node-right new-node) t)
                 (setf (node-right new-node) new-right)
                 (balance-remove-right new-node needs-balance)))
             (let ((new-node (rotate-right node)))
               (setf (node-color (node-left new-node)) 'black
                     (node-color (node-right new-node)) 'black)
               (values new-node nil))))
        ((list 'red (list _ lleft _ lright) _ _)
         (if (and (match:if-match (or nil (list 'black _ _ _)) lleft t nil)
                  (match:if-match (or nil (list 'black _ _ _)) lright t nil))
             (progn
               (rotatef (node-color node) (node-color (node-left node)))
               (values node nil))
             (let ((new-node (rotate-right node)))
               (setf (node-color (node-left new-node)) 'black
                     (node-color (node-right new-node)) 'black)
               (values new-node nil))))
        (otherwise
         (let ((new-node (rotate-right node)))
           (setf (node-color (node-left new-node)) 'black
                 (node-color (node-right new-node)) 'black)
           (values new-node nil))))))

(defun node-print (node depth &key (stream t) show-nil)
  (if (node-empty-p node)
      (when show-nil
        (loop repeat depth do (format stream "    "))
        (format stream "NIL~%"))
      (progn
        (node-print (node-right node) (1+ depth) :stream stream :show-nil show-nil)
        (loop repeat depth do (format stream "    "))
        (format stream "(~A,~S)~%" (symbol-name (node-color node)) (node-value node))
        (node-print (node-left node) (1+ depth) :stream stream :show-nil show-nil))))

;;; rbtree

(defun make-rbtree (&key root (key #'identity) (key-eq-p #'eql) (key-less-p #'<))
  "空または ROOT 指定の赤黒木マップを作成して返す。"
  (list (or root (make-empty-node)) key key-eq-p key-less-p))

(defun rbtree-root (rbtree) (first rbtree))
(defun rbtree-key (rbtree) (second rbtree))
(defun rbtree-key-eq-p (rbtree) (third rbtree))
(defun rbtree-key-less-p (rbtree) (fourth rbtree))
(defun (setf rbtree-root) (root rbtree) (setf (first rbtree) root))
(defun rbtree-each (rbtree fn)
  "キー順（中順）に各要素へ FN を適用する。"
  (node-each (rbtree-root rbtree) fn))

(defun rbtree-to-list (rbtree)
  "木の要素をキー昇順のリストとして返す。"
  (let ((acc nil))
    (rbtree-each rbtree
                 (lambda (v)
                   (push v acc)))
    (nreverse acc)))

(defun rbtree-first (rbtree)
  "最小キーの要素を返す。空なら NIL。"
  (let ((node (node-first (rbtree-root rbtree))))
    (if (node-empty-p node)
        nil
        (node-value node))))

(defun rbtree-last (rbtree)
  "最大キーの要素を返す。空なら NIL。"
  (let ((node (node-last (rbtree-root rbtree))))
    (if (node-empty-p node)
        nil
        (node-value node))))

(defun rbtree-search (rbtree search-key &key default)
  "SEARCH-KEY に一致する要素を返す。未発見時は DEFAULT。"
  (node-search (rbtree-root rbtree)
               search-key
               (rbtree-key rbtree)
               (rbtree-key-eq-p rbtree)
               (rbtree-key-less-p rbtree)
               :default default))

(defun rbtree-lower-bound (rbtree search-key &key end)
  "SEARCH-KEY 以上の最小要素を返す。存在しなければ END。"
  (node-lower-bound (rbtree-root rbtree)
                    search-key
                    (rbtree-key rbtree)
                    (rbtree-key-eq-p rbtree)
                    (rbtree-key-less-p rbtree)
                    :end end))

(defun rbtree-upper-bound (rbtree search-key &key end)
  "SEARCH-KEY より大きい最小要素を返す。存在しなければ END。"
  (node-upper-bound (rbtree-root rbtree)
                    search-key
                    (rbtree-key rbtree)
                    (rbtree-key-eq-p rbtree)
                    (rbtree-key-less-p rbtree)
                    :end end))

(defun rbtree-insert (rbtree value)
  "VALUE を挿入（同一キーは上書き）し、RBTree を返す。"
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
  "REMOVE-KEY の要素を削除し、RBTree を返す。"
  (let ((root (node-remove (rbtree-root rbtree)
                           remove-key
                           (rbtree-key rbtree)
                           (rbtree-key-eq-p rbtree)
                           (rbtree-key-less-p rbtree))))
    (unless (node-empty-p root)
      (setf (node-color root) 'black))
    (setf (rbtree-root rbtree) root)
    rbtree))

(defun rbtree-print (rbtree &key (stream t) show-nil)
  "木構造を整形して STREAM に出力する。"
  (node-print (rbtree-root rbtree) 0 :stream stream :show-nil show-nil))

;;;
;;; linalg.vector
;;;
(defpackage linalg.vector
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
           :vector-dimension=
           :make-zero-vector
           :vector+
           :vector-
           :vector*
           :vector/
           :vector-add!
           :vector-sub!
           :vector-scale!
           :vector-dot
           :vector-cross
           :vector-squared-length
           :vector-length
           :vector-normalize
           :vector-atan
           :vector-angle
           :zero-vector-p
           ))
(in-package linalg.vector)

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
(defun vector-dimension= (&rest vectors)
  (or (null vectors)
      (null (cdr vectors))
      (let ((dimension (vector-dimension (car vectors))))
        (every (lambda (vector)
                 (= dimension (vector-dimension vector)))
               (cdr vectors)))))
(defun make-zero-vector (dimension) (make-vector dimension :initial-element 0))

(defun assert-same-dimension (v1 v2 op-name)
  (let ((d1 (vector-dimension v1))
        (d2 (vector-dimension v2)))
    #-atcoder
    (unless (= d1 d2)
      (error "~A: dimension mismatch (~S vs ~S)." op-name d1 d2))
    d1))

(defun zero-vector-p (vector &key (eps 1d-12))
  (dotimes (i (vector-dimension vector) t)
    (unless (approx= (vector-ref vector i) 0 :eps eps)
      (return-from zero-vector-p nil))))

(defun vector+ (vector &rest more-vectors)
  (let ((dimension (vector-dimension vector))
        (ret (copy-vector vector)))
    (dolist (vec more-vectors ret)
      (assert-same-dimension vector vec "VECTOR+")
      (dotimes (i dimension)
        (incf (vector-ref ret i) (vector-ref vec i))))))

(defun vector* (vector &rest numbers)
  (let ((dimension (vector-dimension vector))
        (ret (copy-vector vector)))
    (dolist (n numbers ret)
      (dotimes (i dimension)
        (setf (vector-ref ret i)
              (* (vector-ref ret i) n))))))

(defun vector- (vector &rest more-vectors)
  (if (null more-vectors)
      (vector* vector -1)
      (let ((dimension (vector-dimension vector))
            (ret (copy-vector vector)))
        (dolist (vec more-vectors ret)
          (assert-same-dimension vector vec "VECTOR-")
          (dotimes (i dimension)
            (decf (vector-ref ret i) (vector-ref vec i)))))))

(defun vector/ (vector &rest numbers)
  (let ((dimension (vector-dimension vector))
        (ret (copy-vector vector)))
    (dolist (n numbers ret)
      (dotimes (i dimension)
        (setf (vector-ref ret i)
              (/ (vector-ref ret i) n))))))

(defun vector-add! (vector &rest more-vectors)
  (let ((dimension (vector-dimension vector)))
    (dolist (vec more-vectors vector)
      (assert-same-dimension vector vec "VECTOR-ADD!")
      (dotimes (i dimension)
        (incf (vector-ref vector i) (vector-ref vec i))))))

(defun vector-sub! (vector &rest more-vectors)
  (let ((dimension (vector-dimension vector)))
    (dolist (vec more-vectors vector)
      (assert-same-dimension vector vec "VECTOR-SUB!")
      (dotimes (i dimension)
        (decf (vector-ref vector i) (vector-ref vec i))))))

(defun vector-scale! (vector &rest numbers)
  (let ((dimension (vector-dimension vector)))
    (dolist (n numbers vector)
      (dotimes (i dimension)
        (setf (vector-ref vector i)
              (* (vector-ref vector i) n))))))

(defun vector-dot (v1 v2)
  (let ((dimension (assert-same-dimension v1 v2 "VECTOR-DOT"))
        (ret 0))
    (dotimes (i dimension ret)
      (incf ret (* (vector-ref v1 i) (vector-ref v2 i))))))

(defun vector-cross (v1 v2)
  (let ((dimension (assert-same-dimension v1 v2 "VECTOR-CROSS")))
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
          #-atcoder
          (t (error "VECTOR-CROSS: Not implemented on the dimension.")))))

(defun vector-squared-length (vector) (vector-dot vector vector))
(defun vector-length (vector) (sqrt (vector-squared-length vector)))
(defun vector-normalize (vector)
  (let ((length (vector-length vector)))
    #-atcoder
    (when (zerop length)
      (error "VECTOR-NORMALIZE: zero vector cannot be normalized."))
    (vector/ vector length)))
(defun vector-atan (vector) (atan (vector-y vector) (vector-x vector)))

(defun vector-angle (v1 v2)
  (let ((denominator (* (vector-length v1) (vector-length v2))))
    #-atcoder
    (when (zerop denominator)
      (error "VECTOR-ANGLE: zero vector is not allowed."))
    (let ((cosine (/ (vector-dot v1 v2) denominator)))
      (acos (max -1 (min 1 cosine))))))

;;;
;;; linalg.matrix
;;;
(defpackage linalg.matrix
  (:use :cl :utility :linalg.vector)
  (:export :make-matrix
           :matrix
           :matrixp
           :matrix-shape
           :matrix-shape=
           :matrix-ref
           :matrix-row-major-ref
           :matrix-element-type
           :matrix-row
           :matrix-column
           :matrix-element-count
           :matrix-row-vector
           :matrix-column-vector
           :make-row-vector
           :vector->row-matrix
           :vector->column-matrix
           :make-zero-matrix
           :make-identity-matrix
           :matrix+
           :matrix-
           :matrix-add!
           :matrix-sub!
           :matrix-scalar-multiply
           :matrix-scale!
           :matrix-transpose
           :matrix-product
           :matrix-vector-product
           :vector-matrix-product
           :outer-product
           :linear-map
           :matrix-trace
           :matrix-diagonal
           :matrix-frobenius-norm
           :identity-matrix-p
           :matrix-power
           :zero-matrix-p
           ))
(in-package linalg.matrix)

(defun make-matrix (row column &rest args &key element-type initial-element initial-contents)
  (declare (ignore element-type initial-element initial-contents))
  (apply #'make-array (list row column) args))
              
(defun matrix (&rest rows)
  (let ((row (length rows))
        (col (length (car rows))))
    (make-matrix row col :initial-contents rows)))

(defun matrixp (matrix) (= (array-rank matrix) 2))
(defun matrix-element-type (matrix) (array-element-type matrix))
(defun matrix-row (matrix) (array-dimension matrix 0))
(defun matrix-column (matrix) (array-dimension matrix 1))
(defun matrix-element-count (matrix) (array-total-size matrix))

(defun matrix-shape (matrix) (list (matrix-row matrix) (matrix-column matrix)))
(defun matrix-shape= (&rest matrices)
  (or (null matrices)
      (null (cdr matrices))
      (let ((row (matrix-row (car matrices)))
            (column (matrix-column (car matrices))))
        (every (lambda (matrix)
                 (and (= row (matrix-row matrix))
                      (= column (matrix-column matrix))))
               (cdr matrices)))))

(defun assert-same-shape (m1 m2 op-name)
  #-atcoder
  (unless (and (= (matrix-row m1) (matrix-row m2))
               (= (matrix-column m1) (matrix-column m2)))
    (error "~A: shape mismatch (~Sx~S) vs (~Sx~S)."
           op-name
           (matrix-row m1) (matrix-column m1)
           (matrix-row m2) (matrix-column m2))))

(defun matrix-ref (matrix y x) (aref matrix y x))
(defun matrix-row-major-ref (matrix index) (row-major-aref matrix index))
(defun (setf matrix-ref) (value matrix y x) (setf (aref matrix y x) value))

(defun (setf matrix-row-major-ref) (value matrix index)
  (setf (row-major-aref matrix index) value))

(defun make-row-vector (matrix row)
  (make-array (matrix-column matrix)
              :element-type (matrix-element-type matrix)
              :displaced-to matrix
              :displaced-index-offset (array-row-major-index matrix row 0)))

(defun matrix-row-vector (matrix row)
  (make-row-vector matrix row))

(defun matrix-column-vector (matrix column)
  (let ((row (matrix-row matrix))
        (ret (make-vector (matrix-row matrix))))
    (dotimes (i row ret)
      (setf (vector-ref ret i) (matrix-ref matrix i column)))))

(defun vector->row-matrix (vector)
  (make-matrix 1
               (vector-dimension vector)
               :initial-contents (list (coerce vector 'list))))

(defun vector->column-matrix (vector)
  (make-matrix (vector-dimension vector)
               1
               :initial-contents
               (loop for i below (vector-dimension vector)
                     collect (list (vector-ref vector i)))))

(defun make-zero-matrix (row column) (make-matrix row column :initial-element 0))

(defun make-identity-matrix (size)
  (let ((m (make-zero-matrix size size)))
    (loop for i below size
          do (setf (matrix-ref m i i) 1)
          finally (return m))))

(defun matrix-binary+ (m1 m2)
  (assert-same-shape m1 m2 "MATRIX+")
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
  (assert-same-shape m1 m2 "MATRIX-")
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

(defun matrix-add! (matrix &rest more-matrices)
  (let* ((row (matrix-row matrix))
         (col (matrix-column matrix))
         (size (* row col)))
    (dolist (m more-matrices matrix)
      (assert-same-shape matrix m "MATRIX-ADD!")
      (dotimes (i size)
        (incf (matrix-row-major-ref matrix i)
              (matrix-row-major-ref m i))))))

(defun matrix-sub! (matrix &rest more-matrices)
  (let* ((row (matrix-row matrix))
         (col (matrix-column matrix))
         (size (* row col)))
    (dolist (m more-matrices matrix)
      (assert-same-shape matrix m "MATRIX-SUB!")
      (dotimes (i size)
        (decf (matrix-row-major-ref matrix i)
              (matrix-row-major-ref m i))))))

(defun matrix-scale! (matrix &rest scalars)
  (let* ((row (matrix-row matrix))
         (col (matrix-column matrix))
         (size (* row col)))
    (dolist (scalar scalars matrix)
      (dotimes (i size)
        (setf (matrix-row-major-ref matrix i)
              (* (matrix-row-major-ref matrix i) scalar))))))

(defun matrix-transpose (matrix)
  (let* ((row (matrix-row matrix))
         (col (matrix-column matrix))
         (ret (make-matrix col row)))
    (dotimes (i row ret)
      (dotimes (j col)
        (setf (matrix-ref ret j i) (matrix-ref matrix i j))))))

(defun matrix-binary-product (m1 m2)
  #-atcoder
  (unless (= (matrix-column m1) (matrix-row m2))
    (error "MATRIX-PRODUCT: dimension mismatch (~Sx~S) * (~Sx~S)."
           (matrix-row m1) (matrix-column m1) (matrix-row m2) (matrix-column m2)))
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
  #-atcoder
  (unless (= (matrix-column matrix) (vector-dimension vector))
    (error "LINEAR-MAP: dimension mismatch matrix-column=~S vector-dimension=~S."
           (matrix-column matrix) (vector-dimension vector)))
  (let* ((row (matrix-row matrix))
         (v (make-zero-vector row)))
    (dotimes (i row v)
      (setf (vector-ref v i)
            (vector-dot vector (make-row-vector matrix i))))))

(defun matrix-vector-product (matrix vector)
  (linear-map matrix vector))

(defun vector-matrix-product (vector matrix)
  #-atcoder
  (unless (= (vector-dimension vector) (matrix-row matrix))
    (error "VECTOR-MATRIX-PRODUCT: dimension mismatch vector=~S matrix-row=~S."
           (vector-dimension vector) (matrix-row matrix)))
  (let* ((col (matrix-column matrix))
         (ret (make-zero-vector col)))
    (dotimes (j col ret)
      (let ((acc 0))
        (dotimes (i (matrix-row matrix))
          (incf acc (* (vector-ref vector i)
                       (matrix-ref matrix i j))))
        (setf (vector-ref ret j) acc)))))

(defun outer-product (v1 v2)
  (let* ((row (vector-dimension v1))
         (col (vector-dimension v2))
         (ret (make-zero-matrix row col)))
    (dotimes (i row ret)
      (dotimes (j col)
        (setf (matrix-ref ret i j)
              (* (vector-ref v1 i) (vector-ref v2 j)))))))

(defun matrix-trace (matrix)
  #-atcoder
  (unless (= (matrix-row matrix) (matrix-column matrix))
    (error "MATRIX-TRACE: matrix must be square, got (~Sx~S)."
           (matrix-row matrix) (matrix-column matrix)))
  (let ((acc 0))
    (dotimes (i (matrix-row matrix) acc)
      (incf acc (matrix-ref matrix i i)))))

(defun matrix-diagonal (matrix)
  (let* ((n (min (matrix-row matrix) (matrix-column matrix)))
         (ret (make-vector n)))
    (dotimes (i n ret)
      (setf (vector-ref ret i) (matrix-ref matrix i i)))))

(defun (setf matrix-diagonal) (value matrix)
  (let ((n (min (matrix-row matrix) (matrix-column matrix))))
    #-atcoder
    (unless (= (vector-dimension value) n)
      (error "(SETF MATRIX-DIAGONAL): dimension mismatch vector=~S diagonal-size=~S."
             (vector-dimension value) n))
    (dotimes (i n value)
      (setf (matrix-ref matrix i i) (vector-ref value i)))))

(defun matrix-frobenius-norm (matrix)
  (sqrt
   (let ((acc 0))
     (dotimes (i (matrix-element-count matrix) acc)
       (incf acc (square (matrix-row-major-ref matrix i)))))))

(defun identity-matrix-p (matrix &key (eps 1d-12))
  (let ((row (matrix-row matrix))
        (col (matrix-column matrix)))
    (and (= row col)
         (dotimes (i row t)
           (dotimes (j col)
             (unless (approx= (matrix-ref matrix i j)
                              (if (= i j) 1 0)
                              :eps eps)
               (return-from identity-matrix-p nil)))))))

(defun zero-matrix-p (matrix &key (eps 1d-12))
  (dotimes (i (matrix-element-count matrix) t)
    (unless (approx= (matrix-row-major-ref matrix i) 0 :eps eps)
      (return-from zero-matrix-p nil))))

(defun matrix-power (matrix exponent)
  #-atcoder
  (unless (and (integerp exponent) (<= 0 exponent))
    (error "MATRIX-POWER: exponent must be a non-negative integer, got ~S." exponent))
  #-atcoder
  (unless (= (matrix-row matrix) (matrix-column matrix))
    (error "MATRIX-POWER: matrix must be square, got (~Sx~S)."
           (matrix-row matrix) (matrix-column matrix)))
  (pow matrix
       exponent
       :op #'matrix-binary-product
       :identity (make-identity-matrix (matrix-row matrix))))

;;;
;;; linalg.geometry
;;;
(defpackage linalg.geometry
  (:use :cl :utility :linalg.vector :linalg.matrix)
  (:export :radian-to-degree
           :degree-to-radian
           :vector2-squared-distance
           :vector2-distance
           :manhattan-distance
           :vector2-upper-half-p
           :vector2-argument<
           :vector2-cross
           :collinear-p
           :vector2-rotate90
           :vector2-rotate
           ))
(in-package linalg.geometry)

;;; angle

(defun radian-to-degree (radian) (/ (* radian 180) PI))
(defun degree-to-radian (degree) (/ (* degree PI) 180))

(defun vector2-squared-distance (v1 v2)
  (let ((dx (- (vector-x v1) (vector-x v2)))
        (dy (- (vector-y v1) (vector-y v2))))
    (+ (square dx) (square dy))))

(defun vector2-distance (v1 v2)
  (sqrt (vector2-squared-distance v1 v2)))

(defun manhattan-distance (v1 v2)
  (+ (diff (vector-x v1) (vector-x v2))
     (diff (vector-y v1) (vector-y v2))))

(defun vector2-upper-half-p (vector)
  (let ((x (vector-x vector))
        (y (vector-y vector)))
    (or (> y 0)
        (and (zerop y)
             (>= x 0)))))

(defun vector2-cross (v1 v2)
  (vector-cross v1 v2))

(defun vector2-argument< (v1 v2)
  (let ((u1 (vector2-upper-half-p v1))
        (u2 (vector2-upper-half-p v2)))
    (cond ((and u1 (not u2)) t)
          ((and (not u1) u2) nil)
          (t (> (vector-cross v1 v2) 0)))))

(defun collinear-p (v1 v2)
  (zerop (vector2-cross v1 v2)))

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
(defun (setf union-find-parent) (parent uf n) (setf (aref (first uf) n) parent))
(defun (setf union-find-rank) (rank uf n)(setf (aref (second uf) n) rank))
(defun (setf union-find-group-size) (group-size uf n) (setf (aref (third uf) n) group-size))

(defun union-find-root (uf n)
  (let ((parent (union-find-parent uf n)))
    (if (= parent n)
        n
        (setf (union-find-parent uf n) (union-find-root uf parent)))))

(defun union-find-group-size (uf n) (aref (third uf) (union-find-root uf n)))

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
           :segment-tree-ref
           :segment-tree-fold
           :segment-tree-print))
(in-package segment-tree)

(defstruct (segment-tree
            (:constructor %make-st (&key size capacity op id bintree bintree-size)))
  size capacity op id bintree bintree-size)

(defun segment-tree-index-in-range-p (st index)
  (and (integerp index)
       (<= 0 index)
       (< index (segment-tree-size st))))

(defun segment-tree-fold-range-valid-p (st left right)
  (and (integerp left)
       (integerp right)
       (<= 0 left)
       (<= left right)
       (<= right (segment-tree-size st))))

(defun make-segment-tree (size op id &key initial-contents)
  "セグメント木を構築して返す。OP は二項演算、ID は単位元。構築は O(n)。"
  #-atcoder
  (unless (and (integerp size) (> size 0))
    (error "MAKE-SEGMENT-TREE: size must be a positive integer, got ~S." size))
  (let* ((logical-size size)
         (capacity (next-pow2 size))
         (bintree-size (1- (* capacity 2)))
         (bintree (make-vector-bintree bintree-size :initial-element id)))
    (unless (null initial-contents)
      #-atcoder
      (when (> (length initial-contents) logical-size)
        (error "MAKE-SEGMENT-TREE: initial-contents length (~S) must be <= size (~S)."
               (length initial-contents) logical-size))
      (setf (subseq bintree (1- capacity) (+ (1- capacity) (length initial-contents)))
            initial-contents)
      (loop for index downfrom (- capacity 2) downto 0
            do (setf (bintree-ref bintree index)
                     (funcall op
                              (bintree-ref bintree (bintree-left-index index))
                              (bintree-ref bintree (bintree-right-index index))))))
    (%make-st :size logical-size
              :capacity capacity
              :op op
              :id id
              :bintree bintree
              :bintree-size bintree-size)))

(defun %st-index-to-bintree-index (st index) (1- (+ index (segment-tree-capacity st))))

(defun segment-tree-ref (st index)
  "葉 INDEX の値を返す。取得は O(1)。"
  #-atcoder
  (unless (segment-tree-index-in-range-p st index)
    (error "SEGMENT-TREE-REF: index out of range: ~S (size=~S)." index (segment-tree-size st)))
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
  "半開区間 [LEFT, RIGHT) の畳み込み結果を返す。計算量 O(log n)。"
  #-atcoder
  (unless (segment-tree-fold-range-valid-p st left right)
    (error "SEGMENT-TREE-FOLD: invalid range [~S, ~S) for size ~S."
           left right (segment-tree-size st)))
  (%st-fold st left right 0 0 (segment-tree-capacity st)))

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
  #-atcoder
  (unless (segment-tree-index-in-range-p st index)
    (error "(SETF SEGMENT-TREE-REF): index out of range: ~S (size=~S)."
           index (segment-tree-size st)))
  (%st-set st (%st-index-to-bintree-index st index) value)
  value)

(defun segment-tree-print (st)
  "セグメント木の内部構造を標準出力向けに表示する。"
  (bintree-print (segment-tree-bintree st)))

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
(defun trie-node-char (node)
  "NODE が表す文字を返す。根ノードでは通常 NIL。"
  (aref node 0))
(defun trie-node-end-count (node)
  "NODE を終端とする登録キー数を返す。"
  (aref node 1))
(defun trie-node-endp (node)
  "NODE が少なくとも1つのキー終端かどうかを返す。"
  (not (zerop (trie-node-end-count node))))
(defun trie-node-prefix-count (node)
  "NODE を接頭辞として通過するキー数を返す。"
  (aref node 2))
(defun trie-node-next (node char) (values (gethash char (aref node 3) nil)))
(defun trie-node-value (node)
  "NODE に紐づく値を返す（終端ノードで主に利用）。"
  (aref node 4))
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
  (when-let ((next (trie-node-next node (aref str index))))
    (trie-node-find next str (1+ index) :prefixp prefixp)))

(defun trie-node-insert (node str index &optional value)
  (incf (trie-node-prefix-count node))
  (when (= index (length str))
    (let ((new-key-p (zerop (trie-node-end-count node))))
      (incf (trie-node-end-count node))
      (setf (trie-node-value node) value)
      (return-from trie-node-insert (values str value new-key-p))))
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
  "空の trie を生成して返す。"
  (vector 0 (make-trie-node nil)))
(defun trie-size (trie)
  "trie に登録されたユニークキー数を返す。"
  (aref trie 0))
(defun (setf trie-size) (size trie) (setf (aref trie 0) size))
(defun trie-root (trie) (aref trie 1))

(defun trie-find (trie str &key (prefixp nil))
  "STR を検索する。見つかれば (values t value) を返す。計算量 O(|STR|)。"
  (trie-node-find (trie-root trie) str 0 :prefixp prefixp))

(defun trie-insert (trie str &optional value)
  "STR を trie に登録し、VALUE を終端ノードへ設定する。計算量 O(|STR|)。"
  (multiple-value-bind (inserted-key inserted-value new-key-p)
      (trie-node-insert (trie-root trie) str 0 value)
    (declare (ignore inserted-key inserted-value))
    (when new-key-p
      (incf (trie-size trie)))
    value))

(defun trie-traverse (trie str fn)
  "STR を辿る各ノードで FN を呼ぶ。FN は (node str index) を受ける。O(|STR|)。"
  (trie-node-traverse (trie-root trie) str 0 fn))

;;;
;;; graph
;;;
(defpackage graph
  (:use :cl :utility :deque :binary-heap)
  (:export :<graph>
           :<single-edge-graph>
           :<multigraph>
           :<edge>
           :graph-size
           :graph-node-ref
           :graph-edge-ref
           :graph-multi-edges-ref
           :graph-neighbors
           :call-with-graph-neighbors
           :graph-add-edge
           :graph-delete-edges
           :do-graph-neighbors
           :edge-from
           :edge-to
           :edge-cost
           :make-edge
           :<adlist-graph>
           :<adlist-multigraph>
           :<adlist-single-edge-graph>
           :make-adlist-single-edge-graph
           :make-adlist-multigraph
           :graph-adlist
           :<matrix-graph>
           :make-matrix-graph
           :graph-matrix
           :<grid-graph>
           :make-grid-graph
           :grid-height
           :grid-width
           :graph-grid
           :*graph-cost-infinity*
           :bfs
           :dijkstra
           :floyd-warshall
           :find-leaf))
(in-package graph)

(defparameter *graph-cost-infinity* (ash most-positive-fixnum -1))

(defgeneric graph-size (graph))
(defgeneric graph-node-ref (graph node))
(defgeneric (setf graph-node-ref) (value graph node))
(defgeneric graph-edge-ref (graph from to)
  (:documentation
   "Returns one edge from FROM to TO.
For multigraph, an arbitrary edge is returned (current implementations usually return the latest one)."))
(defgeneric (setf graph-edge-ref) (edge graph from to))
(defgeneric graph-multi-edges-ref (graph from to)
  (:documentation
   "Returns all edges from FROM to TO as a list.
For single-edge graphs, the list length is at most 1."))
(defgeneric (setf graph-multi-edges-ref) (edges graph from to)
  (:documentation
   "Replaces the whole edge collection from FROM to TO with EDGES.
For single-edge graphs, EDGES must be empty or singleton."))
(defgeneric graph-neighbors (graph node)
  (:documentation "Returns <edge> list."))
(defgeneric call-with-graph-neighbors (graph node fn)
  (:documentation "Calls fn by <edge>."))
(defgeneric graph-add-edge (graph from to &key cost))
(defgeneric graph-delete-edges (graph from to))
(defgeneric graph-low-level-edge-ref (graph from to))
(defgeneric (setf graph-low-level-edge-ref) (edge graph from to))
(defgeneric graph-low-level-multi-edges-ref (graph from to))
(defgeneric (setf graph-low-level-multi-edges-ref) (edges graph from to))

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

(defun graph-node-index-valid-p (graph node)
  (and (integerp node)
       (<= 0 node)
       (< node (graph-size graph))))

(defun assert-graph-node-index (graph node who)
  #-atcoder
  (unless (graph-node-index-valid-p graph node)
    (error "~A: node index out of range: ~S (size=~S)." who node (graph-size graph))))

(defun assert-graph-edge-index (graph from to who)
  (assert-graph-node-index graph from who)
  (assert-graph-node-index graph to who))

(defclass <single-edge-graph> (<graph>) ())

(defmethod graph-edge-ref ((graph <single-edge-graph>) from to)
  (assert-graph-edge-index graph from to "GRAPH-EDGE-REF")
  (ensure-car (graph-low-level-edge-ref graph from to)))

(defmethod (setf graph-edge-ref) (edge (graph <single-edge-graph>) from to)
  (assert-graph-edge-index graph from to "(SETF GRAPH-EDGE-REF)")
  (setf (graph-low-level-edge-ref graph from to) edge)
  edge)

(defmethod graph-multi-edges-ref ((graph <single-edge-graph>) from to)
  (assert-graph-edge-index graph from to "GRAPH-MULTI-EDGES-REF")
  (let ((edge (graph-low-level-edge-ref graph from to)))
    (ensure-list edge)))

(defmethod (setf graph-multi-edges-ref) (edges (graph <single-edge-graph>) from to)
  (assert-graph-edge-index graph from to "(SETF GRAPH-MULTI-EDGES-REF)")
  #-atcoder
  (when (> (length edges) 1)
    (error "(SETF GRAPH-MULTI-EDGES-REF): single-edge-graph accepts at most one edge."))
  (setf (graph-low-level-edge-ref graph from to) (car edges))
  edges)

(defmethod graph-add-edge ((graph <single-edge-graph>) from to &key (cost 1))
  (assert-graph-edge-index graph from to "GRAPH-ADD-EDGE")
  (let ((current-edge (graph-edge-ref graph from to)))
    (when (or (null current-edge)
              (< cost (edge-cost current-edge)))
      (setf (graph-edge-ref graph from to) (make-edge from to :cost cost)))))

(defclass <multigraph> (<graph>) ())

(defmethod graph-edge-ref ((graph <multigraph>) from to)
  (assert-graph-edge-index graph from to "GRAPH-EDGE-REF")
  (car (graph-low-level-multi-edges-ref graph from to)))

(defmethod (setf graph-edge-ref) (edge (graph <multigraph>) from to)
  (assert-graph-edge-index graph from to "(SETF GRAPH-EDGE-REF)")
  (setf (graph-low-level-multi-edges-ref graph from to)
        (ensure-list edge))
  edge)

(defmethod graph-multi-edges-ref ((graph <multigraph>) from to)
  (assert-graph-edge-index graph from to "GRAPH-MULTI-EDGES-REF")
  (graph-low-level-multi-edges-ref graph from to))

(defmethod (setf graph-multi-edges-ref) (edges (graph <multigraph>) from to)
  (assert-graph-edge-index graph from to "(SETF GRAPH-MULTI-EDGES-REF)")
  (setf (graph-low-level-multi-edges-ref graph from to) edges)
  edges)

(defmethod graph-add-edge ((graph <multigraph>) from to &key (cost 1))
  (assert-graph-edge-index graph from to "GRAPH-ADD-EDGE")
  (push (make-edge from to :cost cost)
        (graph-multi-edges-ref graph from to)))

(defclass <adlist-graph> (<graph>)
  ((size :initarg :size :accessor graph-size)
   (adlist :initarg :adlist :accessor graph-adlist)
   (nodes :initarg :nodes :accessor graph-nodes)))

(defclass <adlist-multigraph> (<adlist-graph> <multigraph>) ())

(defclass <adlist-single-edge-graph> (<adlist-graph> <single-edge-graph>) ())

(defun make-adlist-single-edge-graph (size)
  "Adjacency-list graph. Multiple edges between the same (from,to) are prohibited."
  (make-instance '<adlist-single-edge-graph>
                 :size size
                 :adlist (make-array size :initial-element nil)
                 :nodes (make-array size :initial-element nil)))

(defun make-adlist-multigraph (size)
  "Adjacency-list graph. Multiple edges between the same (from,to) are allowed."
  (make-instance '<adlist-multigraph>
                 :size size
                 :adlist (make-array size :initial-element nil)
                 :nodes (make-array size :initial-element nil)))

(defmethod graph-node-ref ((graph <adlist-graph>) node) (aref (graph-nodes graph) node))

(defmethod (setf graph-node-ref) (value (graph <adlist-graph>) node)
  (assert-graph-node-index graph node "GRAPH-NODE-REF")
  (setf (aref (graph-nodes graph) node) value))

(defmethod graph-low-level-edge-ref ((graph <adlist-graph>) from to)
  (assert-graph-edge-index graph from to "GRAPH-LOW-LEVEL-EDGE-REF")
  (find-if #'(lambda (edge) (= (edge-to edge) to))
           (aref (graph-adlist graph) from)))

(defmethod (setf graph-low-level-edge-ref) (edge (graph <adlist-graph>) from to)
  (assert-graph-edge-index graph from to "(SETF GRAPH-LOW-LEVEL-EDGE-REF)")
  (setf (graph-low-level-multi-edges-ref graph from to)
        (ensure-list edge))
  edge)

(defmethod graph-low-level-multi-edges-ref ((graph <adlist-graph>) from to)
  (assert-graph-edge-index graph from to "GRAPH-LOW-LEVEL-MULTI-EDGES-REF")
  (remove-if #'(lambda (edge) (/= (edge-to edge) to))
             (aref (graph-adlist graph) from)))

(defmethod (setf graph-low-level-multi-edges-ref) (edges (graph <adlist-graph>) from to)
  (assert-graph-edge-index graph from to "(SETF GRAPH-LOW-LEVEL-MULTI-EDGES-REF)")
  #-atcoder
  (unless (every (lambda (edge)
                   (and (= from (edge-from edge))
                        (= to (edge-to edge))))
                 edges)
    (error "(SETF GRAPH-LOW-LEVEL-MULTI-EDGES-REF): all edges must satisfy FROM=~S, TO=~S."
           from to))
  (graph-delete-edges graph from to)
  (dolist (edge (nreverse edges))
    (push edge (aref (graph-adlist graph) from)))
  edges)

(defmethod graph-neighbors ((graph <adlist-graph>) node)
  (assert-graph-node-index graph node "GRAPH-NEIGHBORS")
  (aref (graph-adlist graph) node))

(defmethod call-with-graph-neighbors ((graph <adlist-graph>) node fn)
  (assert-graph-node-index graph node "CALL-WITH-GRAPH-NEIGHBORS")
  (dolist (edge (graph-neighbors graph node))
    (funcall fn edge)))

(defmethod graph-delete-edges ((graph <adlist-graph>) from to)
  (assert-graph-edge-index graph from to "GRAPH-DELETE-EDGES")
  (setf (aref (graph-adlist graph) from)
        (delete-if (lambda (edge) (and (= (edge-from edge) from)
                                       (= (edge-to edge) to)))
                   (aref (graph-adlist graph) from))))

(defclass <matrix-graph> (<single-edge-graph>)
  ((size :initarg :size :accessor graph-size)
   (matrix :initarg :matrix :accessor graph-matrix)
   (nodes :initarg :nodes :accessor graph-nodes)))

(defun make-matrix-graph (size)
  "Adjacency-matrix graph. Only one edge is stored per (from,to)."
  (make-instance '<matrix-graph>
                 :size size
                 :matrix (let ((matrix (make-array (list size size) :initial-element nil)))
                           (dotimes (node size matrix)
                             (setf (aref matrix node node) (make-edge node node :cost 0))))
                 :nodes (make-array size :initial-element nil)))

(defmethod graph-node-ref ((graph <matrix-graph>) node) (aref (graph-nodes graph) node))

(defmethod (setf graph-node-ref) (value (graph <matrix-graph>) node)
  (assert-graph-node-index graph node "GRAPH-NODE-REF")
  (setf (aref (graph-nodes graph) node) value))

(defmethod graph-low-level-edge-ref ((graph <matrix-graph>) from to)
  (assert-graph-edge-index graph from to "GRAPH-LOW-LEVEL-EDGE-REF")
  (aref (graph-matrix graph) from to))

(defmethod (setf graph-low-level-edge-ref) (edge (graph <matrix-graph>) from to)
  (assert-graph-edge-index graph from to "(SETF GRAPH-LOW-LEVEL-EDGE-REF)")
  (setf (aref (graph-matrix graph) from to) edge))

(defmethod graph-low-level-multi-edges-ref ((graph <matrix-graph>) from to)
  (assert-graph-edge-index graph from to "GRAPH-LOW-LEVEL-MULTI-EDGES-REF")
  (ensure-list (graph-low-level-edge-ref graph from to)))

(defmethod (setf graph-low-level-multi-edges-ref) (edges (graph <matrix-graph>) from to)
  (assert-graph-edge-index graph from to "(SETF GRAPH-LOW-LEVEL-MULTI-EDGES-REF)")
  #-atcoder
  (unless (length<= edges 1)
    (error "(SETF GRAPH-LOW-LEVEL-MULTI-EDGES-REF): matrix-single-edge-graph accepts at most one edge."))
  #-atcoder
  (unless (every (lambda (edge)
                   (and (= from (edge-from edge))
                        (= to (edge-to edge))))
                 edges)
    (error "(SETF GRAPH-LOW-LEVEL-MULTI-EDGES-REF): all edges must satisfy FROM=~S, TO=~S."
           from to))
  (setf (aref (graph-matrix graph) from to) (car edges))
  edges)

(defmethod graph-neighbors ((graph <matrix-graph>) node)
  (assert-graph-node-index graph node "GRAPH-NEIGHBORS")
  (let ((neighbors nil))
    (do-graph-neighbors (edge graph node (nreverse neighbors))
      (push edge neighbors))))

(defmethod call-with-graph-neighbors ((graph <matrix-graph>) node fn)
  (assert-graph-node-index graph node "CALL-WITH-GRAPH-NEIGHBORS")
  (let ((matrix (graph-matrix graph)))
    (dotimes (to-node (graph-size graph))
      (when-let ((edge (aref matrix node to-node)))
        (funcall fn edge)))
    (values)))

(defmethod graph-delete-edges ((graph <matrix-graph>) from to)
  (assert-graph-edge-index graph from to "GRAPH-DELETE-EDGES")
  (setf (aref (graph-matrix graph) from to) nil))

(defclass <grid-graph> (<single-edge-graph>)
  ((height :initarg :height :accessor grid-height)
   (width :initarg :width :accessor grid-width)
   (grid :initarg :grid :accessor graph-grid)
   (nodes :initarg :nodes :accessor graph-nodes)))

(defun make-grid-graph (height width lines &key (wall #\#) (nodes nil))
  (make-instance '<grid-graph>
                 :height height
                 :width width
                 :grid (make-array (list height width)
                                   :initial-contents
                                   (map 'list
                                        (lambda (row)
                                          (map 'list
                                               (lambda (e)
                                                 (if (eql e wall) *graph-cost-infinity* 1))
                                               row))
                                        lines))
                 :nodes nodes))

(defun grid-row (graph node) (floor node (grid-width graph)))
(defun grid-column (graph node) (mod node (grid-width graph)))
(defun grid-point-to-index (graph row column) (+ (* (grid-width graph) row) column))
(defun grid-cost (graph row column) (aref (graph-grid graph) row column))
(defun grid-node-wall-p (graph node)
  (= (grid-cost graph (grid-row graph node) (grid-column graph node))
     *graph-cost-infinity*))
(defmethod graph-size ((graph <grid-graph>)) (* (grid-height graph) (grid-width graph)))
(defmethod graph-node-ref ((graph <grid-graph>) node)
  (assert-graph-node-index graph node "GRAPH-NODE-REF")
  (let ((nodes (graph-nodes graph)))
    #-atcoder
    (when (null nodes)
      (error "GRID-GRAPH nodes is NIL. Pass :NODES to MAKE-GRID-GRAPH before using GRAPH-NODE-REF."))
    (aref nodes node)))

(defmethod (setf graph-node-ref) (value (graph <grid-graph>) node)
  (assert-graph-node-index graph node "(SETF GRAPH-NODE-REF)")
  (let ((nodes (graph-nodes graph)))
    #-atcoder
    (when (null nodes)
      (error "GRID-GRAPH nodes is NIL. Pass :NODES to MAKE-GRID-GRAPH before using (SETF GRAPH-NODE-REF)."))
    (setf (aref nodes node) value)))

(defmethod graph-low-level-edge-ref ((graph <grid-graph>) from to)
  (assert-graph-edge-index graph from to "GRAPH-EDGE-REF")
  (let ((from-row (grid-row graph from))
        (from-col (grid-column graph from))
        (to-row (grid-row graph to))
        (to-col (grid-column graph to)))
    (cond ((or (grid-node-wall-p graph from)
               (grid-node-wall-p graph to))
           nil)
          ((and (= from-row to-row) (= from-col to-col)) (make-edge from to :cost 0))
          ((and (= 1 (+ (abs (- from-row to-row))
                        (abs (- from-col to-col))))
                (/= (grid-cost graph to-row to-col) *graph-cost-infinity*))
           (make-edge from to :cost (grid-cost graph to-row to-col)))
          (t nil))))

(defmethod (setf graph-edge-ref) (edge (graph <grid-graph>) from to) (error "Unsupported method."))

(defmethod graph-neighbors ((graph <grid-graph>) node)
  (assert-graph-node-index graph node "GRAPH-NEIGHBORS")
  (let ((neighbors nil))
    (call-with-graph-neighbors graph node (lambda (edge) (push edge neighbors)))
    (nreverse neighbors)))

(defmethod call-with-graph-neighbors ((graph <grid-graph>) node fn)
  (assert-graph-node-index graph node "CALL-WITH-GRAPH-NEIGHBORS")
  (when (grid-node-wall-p graph node)
    (return-from call-with-graph-neighbors nil))
  (do-4neighbors ((y x) ((grid-row graph node) (grid-column graph node)))
    (when (and (< -1 y (grid-height graph)) (< -1 x (grid-width graph))
               (/= (aref (graph-grid graph) y x) *graph-cost-infinity*))
      (funcall fn (make-edge node (grid-point-to-index graph y x)
                             :cost (grid-cost graph y x))))))

(defmethod graph-add-edge ((graph <grid-graph>) from to &key cost)
  (declare (ignore graph from to cost))
  (error "Unsupported method."))

(defmethod graph-delete-edges ((graph <grid-graph>) from to) (error "Unsupported method."))

(defun bfs (graph &key (start 0) end)
  (assert-graph-node-index graph start "BFS")
  (when end
    (assert-graph-node-index graph end "BFS"))
  (loop with size = (graph-size graph)
        with distances = (make-array size :initial-element *graph-cost-infinity*)
        and usedp = (make-array size :initial-element nil)
        and deque = (make-deque)
        initially (setf (aref distances start) 0
                        (aref usedp start) t)
                  (deque-push-back deque start)
        until (deque-empty-p deque)
        do (let ((node (deque-peek-front deque)))
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
         (distances (make-array (list size size) :initial-element *graph-cost-infinity*)))
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
  (assert-graph-node-index graph start "DIJKSTRA")
  (when end
    (assert-graph-node-index graph end "DIJKSTRA"))
  (labels ((make-search-node (from to distance) (vector from to distance))
           (search-node-to (search-node) (elt search-node 1))
           (search-node-distance (search-node) (elt search-node 2))
           (search-node-less (search-node1 search-node2)
             (< (search-node-distance search-node1) (search-node-distance search-node2))))
    (loop with size = (graph-size graph)
          with distances = (make-array size :initial-element *graph-cost-infinity*)
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
;;; amb
;;;
(defpackage amb
  (:use :cl)
  (:export :*failed*
           :amb-reset
           :amb
           :amb-bind
           ))
(in-package :amb)

(defvar *stack* nil)
(defvar *failed* nil)

(defun amb-reset () (setf *stack* nil))

(defun fail ()
  (if (null *stack*)
      *failed*
      (funcall (pop *stack*))))

(defmacro amb (&rest options)
  (if (null options)
      `(fail)
      `(progn ,@(mapcar #'(lambda (option)
                            `(push #'(lambda () ,option) *stack*))
                        (reverse (cdr options)))
              ,(car options))))

(defmacro amb-bind (var options &body body)
  `(amb-bind-fn (lambda (,var) ,@body) ,options))

(defun amb-bind-fn (cont options)
  #-sbcl (error "Unsupported implementation.")
  (when (sb-sequence:emptyp options)
    (return-from amb-bind-fn (fail)))
  (sb-sequence:with-sequence-iterator (iterator limit from-end-p step endp element set-element index copy)
      (options)
    (declare (ignore set-element index copy))
    (labels ((rec (iterator)
               (if (not (funcall endp options iterator limit from-end-p))
                   (progn (push #'(lambda ()
                                    (rec (funcall step options iterator from-end-p)))
                                *stack*)
                          (funcall cont (funcall element options iterator)))
                   (fail))))
      (rec iterator))))

;;;
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
;;; test
;;;
(defpackage :atcoder.test
  (:use :cl :utility)
  (:export :test-case
           :*atcoder-session-cookie*
           :fetch-atcoder-samples
           :test-url
           :test))
(in-package :atcoder.test)

(defun normalize-whitespaces (string)
  (with-output-to-string (out)
    (loop with pending-space = nil
          and wrote-char-p = nil
          for ch across string
          do (if (find ch '(#\Space #\Tab #\Newline #\Return))
                 (setf pending-space t)
                 (progn
                   (when (and pending-space wrote-char-p)
                     (write-char #\Space out))
                   (write-char ch out)
                   (setf wrote-char-p t)
                   (setf pending-space nil))))))

(defun test-case (fn input expect)
  (let ((output (with-output-to-string (*standard-output*)
                  (with-input-from-string (*standard-input* input)
                    (funcall fn)))))
    (if (string= (normalize-whitespaces output)
                 (normalize-whitespaces expect))
        (format t "Pass~%")
        (format t "Failed~%expect: ~A~%but actual: ~A~%" expect output))))

(defun atcoder-html-unescape (string)
  (labels ((replace-all (s from to)
             (with-output-to-string (out)
               (loop with start = 0
                     and from-len = (length from)
                     for pos = (search from s :start2 start)
                     if pos
                       do (write-string s out :start start :end pos)
                          (write-string to out)
                          (setf start (+ pos from-len))
                     else
                       do (write-string s out :start start)
                          (return)))))
    (let ((out string))
      (setf out (replace-all out "&lt;" "<"))
      (setf out (replace-all out "&gt;" ">"))
      (setf out (replace-all out "&amp;" "&"))
      (setf out (replace-all out "&quot;" "\""))
      (setf out (replace-all out "&#39;" "'"))
      out)))

(defun atcoder-strip-tags (string)
  (with-output-to-string (out)
    (loop with in-tag = nil
          for ch across string
          do (cond ((char= ch #\<) (setf in-tag t))
                   ((char= ch #\>) (setf in-tag nil))
                   ((not in-tag) (write-char ch out))))))

(defun atcoder-clean-pre-text (text)
  (let* ((lines (split-string text :separator '(#\Newline)))
         (filtered (loop for line in lines
                         unless (string-prefix-p "```" (trim-spaces line))
                           collect line)))
    (trim-spaces (atcoder-html-unescape (strjoin filtered)))))

(defun detect-sample-kind (heading)
  (let ((h (string-downcase (trim-spaces heading))))
    (cond ((or (search "入力例" h)
               (search "sample input" h))
           :input)
          ((or (search "出力例" h)
               (search "sample output" h))
           :output)
          (t nil))))

(defun parse-atcoder-samples-from-html (html)
  (let ((pos 0)
        (pending-input nil)
        (cases nil))
    (loop
      for h3-open = (search "<h3" html :start2 pos)
      while h3-open
      do (let* ((h3-tag-end (search ">" html :start2 h3-open))
                (h3-close (and h3-tag-end (search "</h3>" html :start2 (1+ h3-tag-end)))))
           (if (and h3-tag-end h3-close)
               (let* ((heading-raw (subseq html (1+ h3-tag-end) h3-close))
                      (kind (detect-sample-kind (atcoder-strip-tags heading-raw)))
                      (pre-open (search "<pre" html :start2 h3-close))
                      (pre-tag-end (and pre-open (search ">" html :start2 pre-open)))
                      (pre-close (and pre-tag-end (search "</pre>" html :start2 (1+ pre-tag-end)))))
                 (if (and kind pre-open pre-tag-end pre-close)
                     (let ((text (atcoder-clean-pre-text (subseq html (1+ pre-tag-end) pre-close))))
                       (cond ((eq kind :input)
                              (setf pending-input text))
                             ((and (eq kind :output) pending-input)
                              (push (list pending-input text) cases)
                              (setf pending-input nil)))
                       (setf pos (+ pre-close (length "</pre>"))))
                     (setf pos (+ h3-close (length "</h3>")))))
               (setf pos (1+ h3-open)))))
    (nreverse cases)))

(defparameter *atcoder-session-cookie* nil
  "AtCoder session cookie string for authenticated fetch.
Either full cookie pair (e.g. \"REVEL_SESSION=...\") or just cookie value.")

(defun normalize-atcoder-cookie (cookie)
  (cond ((null cookie) nil)
        ((search "=" cookie) cookie)
        (t (format nil "REVEL_SESSION=~A" cookie))))

(defun ensure-load-dexador ()
  (unless (find-package :dexador)
    (when (find-package :ql)
      (funcall (intern (string :quickload) :ql) :dexador :silent t)))
  (unless (find-package :dexador)
    (error "Dexador package is not loaded. Load dexador before calling fetch-atcoder-samples.")))

(defun resolve-dexador-get-function ()
  "Resolve and return the function object of DEXADOR:GET lazily.

This indirection intentionally avoids writing DEXADOR:GET directly at read/load
time, so this file can still be loaded in environments where the DEXADOR package
is not present yet. We first ensure dexador is loaded, then resolve GET from the
DEXADOR package at runtime and return its function object."
  (ensure-load-dexador)
  (let ((symbol (find-symbol (string :get) :dexador)))
    (unless (and symbol (fboundp symbol))
      (error "DEXADOR:GET function is not available."))
    (symbol-function symbol)))

(defun fetch-url (url &key cookie)
  (let ((cookie-header (normalize-atcoder-cookie cookie))
        (dexador-get (resolve-dexador-get-function)))
    (if cookie-header
        (funcall dexador-get
                 url
                 :headers `(("Cookie" . ,cookie-header)))
        (funcall dexador-get url))))

(defun fetch-atcoder-samples (url &key cookie)
  "Fetch sample input/output pairs from an AtCoder task URL.
Returns a list of (input output)."
  (let* ((html (fetch-url url :cookie (or cookie *atcoder-session-cookie*)))
         (cases (parse-atcoder-samples-from-html html)))
    #-atcoder (when (null cases)
      (error "No samples found in URL: ~A" url))
    cases))

(defun test-url (fn url &key cookie)
  "Run test-case for all samples fetched from AtCoder task URL."
  (let ((cases (fetch-atcoder-samples url :cookie cookie)))
    (loop for (input expect) in cases
          for index from 1
          do (format t "[Sample ~D]~%" index)
             (test-case fn input expect))
    (length cases)))

(defun test (fn url &key cookie)
  (test-url fn url :cookie cookie))

;;;
;;; atcoder
;;;
(defpackage atcoder
  (:use :cl
        :utility
        :input
        :list-queue
        :deque
        :binary-heap
        :ordered-map
        :linalg.vector
        :linalg.matrix
        :linalg.geometry
        :union-find
        :segment-tree
        :trie
        :graph
        :algorithm
        :amb
        :atcoder.test
        )
  (:export :main))
(in-package atcoder)

(defun main ()
  (input* ((a fixnum)
           (b fixnum))
    (format t "~A~%" (+ a b))))

#+atcoder (main)
