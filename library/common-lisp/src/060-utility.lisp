;;; utility
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let* ((sources '(:utility.syntax
                    :utility.number
                    :utility.base
                    :utility.window))
         (pkg (or (find-package :utility)
                  (make-package :utility :use '(:cl)))))
    (dolist (src sources)
      (use-package src pkg)
      (do-external-symbols (sym src)
        (export sym pkg)))))

;;;
