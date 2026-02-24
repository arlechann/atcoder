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
