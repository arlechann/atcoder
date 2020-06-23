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

(defun unfold (p f g seed &optional (tail-gen (lambda () '())))
  (if (p seed)
      (tail-gen seed)
      (cons (f seed)
            (unfold p f g (g seed) tail-gen))))

(defun iota (c &optional (s 0) (step 1) (acc nil))
  (if (zerop c)
      (reverse acc)
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
  (let ((argc (length args)))
    `(let ((dp (make-hash-table :test #'equal)))
       (defun ,name ,args
         (let-if memo (gethash ,@(if (= argc 1)
                                     args
                                     `((list ,@args)))
                               dp)
                 memo
                 (setf (gethash ,@(if (= argc 1)
                                      args
                                      `((list ,@args)))
                                dp)
                       ,@body))))))

(defparameter n (read))
(defparameter abc (make-array `(,n 3)))
(dotimes (i n)
  (setf #&(abc i 0) (read-integer))
  (setf #&(abc i 1) (read-integer))
  (setf #&(abc i 2) (read-integer)))

(defdp solve (x act)
  (if (< x 0)
      0
      (+ (loop for i to 2
               unless (= i act)
                 maximize (solve (1- x) i))
         #&(abc x act))))

(format t "~A~%" (max (solve (1- n) 0) (solve (1- n) 1) (solve (1- n) 2)))
