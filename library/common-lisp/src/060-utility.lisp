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
