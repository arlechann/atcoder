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

