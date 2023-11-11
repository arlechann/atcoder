; (if-match (a 'z c) '(1 z 3)
;   (list a c)
;   'unmatched)
; => 1
; => 3
;
; (let ((a 1))
;   (if (eq 'z 'z)
;       (let ((c 3))
;         (list a c))
;       nil))

(eval-when (:compile-toplevel :execute)
  (defun var? (pat) (and (not (null pat)) (symbolp pat)))

  (defun vars-in (pat)
    (cond ((var? pat) (list pat))
          ((atom pat) nil)
          (t (union (vars (car pat))
                    (vars (cdr pat))))))

  (defun match1 (pat expr then else)
    (cond ((var? pat)
           `(let ((,pat ,expr))
             ,then))
          ((eq pat '_) then)
          (t `(if (equal ,pat ,expr) ,then ,else))))
  )

(defmacro if-match (pat expr then)
  (let ((gundef (gensym "GUNDEF"))
        (vars (vars-in pat)))
    `(let ,(mapcar (lambda (v) `(,v ',gundef))
                   vars)
       (declare (ignorable ,@vars))
       ,(match1 pat expr then nil))))
