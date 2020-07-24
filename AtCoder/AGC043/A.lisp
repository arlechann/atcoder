(unless (member :child-sbcl *features*)
  #-swank (quit
            :unix-status
            (process-exit-code
              (run-program *runtime-pathname*
                           `("--control-stack-size" "128MB"
                             "--noinform"
                             "--disable-ldb"
                             "--lose-on-corruption"
                             "--end-runtime-options"
                             "--eval" "(push :child-sbcl *features*)"
                             "--script" ,(namestring *load-pathname*))
                           :output t :error t :input t))))

(eval-when (:compile-toplevel)
  #-swank (proclaim '(optimize (speed 3) (debug 0) (safety 0))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (labels ((array-reference-reader (stream c n)
             "Reader macro #&(array index) => (aref array index)"
             (declare (ignore c n))
             (let ((list (read stream)))
               `(aref ,(car list) ,@(cdr list)))))
    (setf *readtable* (copy-readtable))
    (set-dispatch-macro-character #\# #\& #'array-reference-reader)))

(defconstant mod-number 1000000007)

(defmacro let-if (var cond tbody &optional fbody)
  `(let ((,var ,cond))
     (if ,var
         ,tbody
         ,fbody)))

(defmacro named-let (name binds &body body)
  (let ((params (mapcar #'car binds))
        (args (mapcar #'cadr binds)))
    `(labels ((,name (,@params)
                ,@body))
       (,name ,@args))))
                     
(defun unfold (p f g seed &optional (tail-gen (lambda () '())))
  (if (p seed)
      (tail-gen seed)
      (cons (f seed)
            (unfold p f g (g seed) tail-gen))))

(defun iota (c &optional (s 0) (step 1) (acc nil))
  (if (zerop c)
      (nreverse acc)
      (iota (1- c) (+ s step) step (cons s acc))))

(defun sum (list &optional (init 0))
  (reduce #'+ list :initial-value init))

(defun chomp (str)
  (string-right-trim '(#\Return #\Linefeed) str))

(defun read-integer (&optional (in *standard-input*))
  (declare (inline read-byte))
  (labels ((number-char-p (b)
             (<= #.(char-code #\0) b #.(char-code #\9)))
           (minus-char-p (b)
             (= b #.(char-code #\-)))
           (to-number (b)
             (- b #.(char-code #\0))))
    (declare (inline number-char-p minus-char-p to-number))
    (macrolet ((%read-byte ()
                 '(the (unsigned-byte 8)
                       #+swank (char-code (read-char in nil #\Nul))
                       #-swank (read-byte in nil 0))))
      (let* ((minus-p nil)
             (x (loop for b = (%read-byte)
                      if (number-char-p b)
                        return (to-number b)
                        end
                      if (minus-char-p b)
                        do (setf minus-p t))))
        (declare (boolean minus-p) (fixnum x))
        (the fixnum (loop for b = (%read-byte)
                          and y = x then (+ (* y 10) (to-number b))
                          unless (number-char-p b)
                            return (funcall (if minus-p #'- #'+) y)))))))

(defun split (x str &optional (acc nil))
  (let ((pos (search x str))
        (size (length x)))
    (if pos
        (split x (subseq str (+ pos size)) (cons (subseq str 0 pos) acc))
        (nreverse (cons str acc)))))

(defmacro minf (field &rest x)
  `(setf ,field (min ,field ,@x)))

(defmacro maxf (field &rest x)
  `(setf ,field (max ,field ,@x)))

(defmacro debug-print (x)
  `(let ((y ,x))
    (format t "~A: ~A~%" ',x y)
    y))

(defmacro defdp (name args &body body)
  (let ((argc (length args))
        (key (gensym)))
    `(let ((dp (make-hash-table :test #'equal)))
       (defun ,name ,args
         (let ((,key ,@(if (= argc 1) args `((list ,@args)))))
           (let-if memo (gethash ,key dp)
                   memo
                   (setf (gethash ,key dp)
                         (progn ,@body))))))))

(defparameter h (read))
(defparameter w (read))
(defparameter s (make-array (list h w)))

(dotimes (i h)
  (dotimes (j w)
    (setf (aref s i j) (read-char)))
  (read-char))

(defdp solve (i j)
  (cond ((and (zerop i) (zerop j)) (if (eq (aref s 0 0) #\.) 0 1))
        ((zerop i) (+ (solve i (1- j))
                      (if (and (eq (aref s i j) #\#)
                               (eq (aref s i (1- j)) #\.))
                          1
                          0)))
        ((zerop j) (+ (solve (1- i) j)
                      (if (and (eq (aref s i j) #\#)
                               (eq (aref s (1- i) j) #\.))
                          1
                          0)))
        (t (min (+ (solve (1- i) j)
                   (if (and (eq (aref s i j) #\#)
                            (eq (aref s (1- i) j) #\.))
                       1
                       0))
                (+ (solve i (1- j))
                   (if (and (eq (aref s i j) #\#)
                            (eq (aref s i (1- j)) #\.))
                       1
                       0))))))

(format t "~A~%" (solve (1- h) (1- w)))
