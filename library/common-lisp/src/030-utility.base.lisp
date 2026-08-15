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
           :flip
           :compose
           :conjoin
           :disjoin
           :pa
           :pa*
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
           :reduce-with-index
           :find-with-index
           :argopt
           :argmax
           :argmin
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
           :length>
           :length>=
           :singlep
           :last1
           :mklist
           :take
           :drop
           :tconc
           :filter-map
           :longerp
           :longer
           :iota
           :reverse-nconc
           :unfold
           :unique
           :with-collector
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
(defun flip (function)
  (lambda (&rest args)
    (apply function (reverse args))))
(defun compose (&rest fns) (lambda (x) (reduce #'funcall fns :initial-value x :from-end t)))

(eval-always
  (defun %predicate-chain-expansion (predicates terminal-value short-circuit-op)
    (cond ((null predicates) `(constantly ,terminal-value))
          ((null (cdr predicates)) (car predicates))
          (t (let ((args (gensym)))
               `#'(lambda (&rest ,args)
                    (,short-circuit-op
                     ,@(mapcar (lambda (predicate)
                                 `(apply ,predicate ,args))
                               predicates))))))))

(defun conjoin (&rest predicates)
  (cond ((null predicates) (constantly t))
        ((null (cdr predicates)) (car predicates))
        (t (lambda (&rest args)
             (dolist (predicate predicates t)
               (unless (apply predicate args)
                 (return nil)))))))

(defun disjoin (&rest predicates)
  (cond ((null predicates) (constantly nil))
        ((null (cdr predicates)) (car predicates))
        (t (lambda (&rest args)
             (dolist (predicate predicates nil)
               (when (apply predicate args)
                 (return t)))))))

(define-compiler-macro conjoin (&whole form &rest predicates)
  (if (every #'constantp predicates)
      (%predicate-chain-expansion predicates t 'and)
      form))

(define-compiler-macro disjoin (&whole form &rest predicates)
  (if (every #'constantp predicates)
      (%predicate-chain-expansion predicates nil 'or)
      form))

(eval-always
  (defun %pa-placeholder-index (form)
    (when (keywordp form)
      (let ((name (symbol-name form)))
        (when (and (> (length name) 1)
                   (char= (char name 0) #\$))
          (multiple-value-bind (value position)
              (parse-integer name :start 1 :junk-allowed t)
            (when (= position (length name))
              value)))))))

(eval-always
  (defun %pa-rest-placeholder-p (form)
    (and (keywordp form)
         (string= (symbol-name form) "$@"))))

(eval-always
  (defun %pa-max-index (forms)
    (loop for form in forms
          for index = (%pa-placeholder-index form)
          when index maximize index into max-index
          finally (return (or max-index -1)))))

(eval-always
  (defun %pa-lambda-vars (max-index)
    (loop repeat (1+ max-index) collect (gensym "ARG"))))

(eval-always
  (defun %pa-call-expression (function-form forms expanded-forms restp)
    (if restp
        `(apply ,function-form
                (append
                 ,@(mapcar (lambda (form expanded-form)
                             (if (%pa-rest-placeholder-p form)
                                 expanded-form
                                 `(list ,expanded-form)))
                           forms
                           expanded-forms)))
        `(funcall ,function-form ,@expanded-forms))))

(eval-always
  (defun %pa-lambda-form (vars rest-var call-form)
    `#'(lambda ,(append vars (if rest-var `(&rest ,rest-var) nil))
         ,call-form)))

(eval-always
  (defun %pa-form (function forms)
    (let* ((max-index (%pa-max-index forms))
           (vars (%pa-lambda-vars max-index))
           (restp (find-if #'%pa-rest-placeholder-p forms))
           (rest-var (and restp (gensym "REST")))
           (expanded-forms
             (mapcar (lambda (form)
                       (let ((index (%pa-placeholder-index form)))
                         (cond (index (nth index vars))
                               ((%pa-rest-placeholder-p form) rest-var)
                               (t form))))
                     forms))
           (call-form (%pa-call-expression function forms expanded-forms restp)))
      (%pa-lambda-form vars rest-var call-form))))

(eval-always
  (defun %pa*-form (function forms)
    (let* ((max-index (%pa-max-index forms))
           (vars (%pa-lambda-vars max-index))
           (restp (find-if #'%pa-rest-placeholder-p forms))
           (rest-var (and restp (gensym "REST")))
           (bindings nil)
           (function-var (gensym "FUNCTION"))
           (expanded-forms
             (mapcar (lambda (form)
                       (let ((index (%pa-placeholder-index form)))
                         (cond (index (nth index vars))
                               ((%pa-rest-placeholder-p form) rest-var)
                               (t
                                (let ((temp (gensym "FIXED")))
                                  (push (list temp form) bindings)
                                  temp)))))
                     forms))
           (call-form (%pa-call-expression function-var forms expanded-forms restp)))
      (push (list function-var function) bindings)
      `(let ,(nreverse bindings)
         ,(%pa-lambda-form vars rest-var call-form)))))

(defmacro pa (function &rest forms)
  "FUNCTION と :$0, :$1, ... , :$@ プレースホルダから関数を構築する。
プレースホルダでない式は、返された関数の呼び出し時に評価される。"
  (%pa-form function forms))

(defmacro pa* (function &rest forms)
  "PA と同様に関数を構築するが、プレースホルダでない式は生成時に評価して束縛する。"
  (%pa*-form function forms))

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

(declaim (ftype (function ((or (function (unsigned-byte t t) t) symbol)
                           sequence
                           &rest t
                           &key (:key t) (:from-end t) (:start fixnum) (:end fixnum) (:initial-value t))
                          t)
                reduce-with-index))
(defun reduce-with-index (function sequence
                          &rest args
                          &key key from-end (start 0) end (initial-value nil ivp))
  (declare (ignore args))
  (let* ((end (or end (length sequence)))
         (transform (or key #'identity)))
    (cond
      ((= start end)
       (if ivp
           initial-value
           (error "REDUCE-WITH-INDEX requires an initial value for an empty sequence.")))
      (from-end
       (let ((acc (if ivp
                      initial-value
                      (funcall transform (elt sequence (1- end))))))
         (loop for index downfrom (if ivp
                                      (1- end)
                                      (- end 2))
               to start
               do (setf acc (funcall function
                                     index
                                     acc
                                     (funcall transform (elt sequence index)))))
         acc))
      (t
       (let ((acc (if ivp
                      initial-value
                      (funcall transform (elt sequence start)))))
         (loop for index from (if ivp start (1+ start))
               below end
               do (setf acc (funcall function
                                     index
                                     acc
                                     (funcall transform (elt sequence index)))))
         acc)))))

(declaim (ftype (function ((or (function (unsigned-byte t) t) symbol)
                           sequence
                           &rest t
                           &key (:from-end t) (:start fixnum) (:end fixnum) (:key t))
                          (or null (cons fixnum t)))
                find-with-index))
(defun find-with-index (predicate sequence &rest args &key from-end start end key)
  (declare (ignore args))
  (let* ((start (or start 0))
         (end (or end (length sequence)))
         (transform (or key #'identity)))
    (if from-end
        (loop for index downfrom (1- end) to start
              for item = (elt sequence index)
              when (funcall predicate index (funcall transform item))
                do (return (cons index item)))
        (loop for index from start below end
              for item = (elt sequence index)
              when (funcall predicate index (funcall transform item))
                do (return (cons index item))))))

(declaim (ftype (function ((or (function (t t) boolean) symbol)
                           sequence
                           &rest t
                           &key (:key t) (:from-end t) (:start fixnum) (:end fixnum)) t)
                argopt))
(defun argopt (predicate sequence &rest args &key key from-end start end)
  (declare (ignore args))
  (let* ((start (or start 0))
         (end (or end (length sequence)))
         (transform (or key #'identity)))
    (when (< start end)
      (if from-end
          (let* ((best-index (1- end))
                 (best-value (funcall transform (elt sequence best-index))))
            (loop for index downfrom (- end 2) to start
                  for value = (funcall transform (elt sequence index))
                  when (funcall predicate value best-value)
                    do (setf best-index index
                             best-value value))
            best-index)
          (let* ((best-index start)
                 (best-value (funcall transform (elt sequence best-index))))
            (loop for index from (1+ start) below end
                  for value = (funcall transform (elt sequence index))
                  when (funcall predicate value best-value)
                    do (setf best-index index
                             best-value value))
            best-index)))))

(declaim (ftype (function (sequence &rest t &key (:key t) (:from-end t) (:start fixnum) (:end fixnum)) t)
                argmax))
(defun argmax (sequence &rest args &key key from-end start end)
  (declare (ignore key from-end start end))
  (apply #'argopt #'> sequence args))

(declaim (ftype (function (sequence &rest t &key (:key t) (:from-end t) (:start fixnum) (:end fixnum)) t)
                argmin))
(defun argmin (sequence &rest args &key key from-end start end)
  (declare (ignore key from-end start end))
  (apply #'argopt #'< sequence args))

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

(declaim (ftype (function (list unsigned-byte) boolean) length>))
(defun length> (lst n)
  (not (length<= lst n)))

(declaim (ftype (function (list unsigned-byte) boolean) length>=))
(defun length>= (lst n)
  (not (length< lst n)))

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

(declaim (ftype (function (list t) cons) tconc))
(defun tconc (pointer obj)
  (when (null pointer)
    (setf pointer (cons nil nil)))
  (let ((cell (cons obj nil)))
    (if (null (car pointer))
        (setf (car pointer) cell
              (cdr pointer) cell)
        (setf (cdr (cdr pointer)) cell
              (cdr pointer) cell))
    pointer))

(declaim (ftype (function ((function (t &rest t) t) list &rest list) list) filter-map))
(defun filter-map (fn lst &rest more-lst)
  (do ((lists (cons lst more-lst) (mapcar #'cdr lists))
       (acc nil))
      ((some #'null lists) (nreverse acc))
    (when-let ((item (apply fn (mapcar #'car lists))))
      (push item acc))))

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

(defmacro with-collector ((&rest collectors) &body body)
  (let ((lists (mapcar (lambda (collector)
                         (declare (ignore collector))
                         (gensym))
                       collectors)))
    `(let ,(mapcar (lambda (list)
                     `(,list (cons nil nil)))
                   lists)
       (flet ,(mapcar (lambda (collector list)
                        `(,collector (&optional (value nil supplied-p))
                           (when supplied-p
                             (tconc ,list value))
                           (car ,list)))
                      collectors
                      lists)
         ,@body))))

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
