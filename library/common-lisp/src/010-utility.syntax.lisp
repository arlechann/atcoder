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
