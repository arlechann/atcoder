(in-package :cl-user)

(require :asdf)
(pushnew (truename "library/common-lisp/") asdf:*central-registry* :test #'equal)
(asdf:load-system "atcoder-library-core")

(defun measure-seconds (thunk &key (rounds 1))
  (let ((start (get-internal-real-time)))
    (dotimes (i rounds)
      (declare (ignore i))
      (funcall thunk))
    (/ (- (get-internal-real-time) start)
       internal-time-units-per-second)))

(defun maybe-gc ()
  #+sbcl (sb-ext:gc :full t)
  #-sbcl nil)

(defun bench-runtime-init (n rounds)
  (measure-seconds
   (lambda ()
     (setf math::*pascal-max-n* n
           math::*pascal-table* (math::%make-pascal-table n)))
   :rounds rounds))

(defun bench-macro-compile (n rounds)
  (measure-seconds
   (lambda ()
     ;; Macroexpansion/build happens while compiling this lambda.
     (compile nil `(lambda () (math:init-pascal-nck ,n))))
   :rounds rounds))

(defun bench-macro-runtime (n rounds)
  (let ((fn (compile nil `(lambda () (math:init-pascal-nck ,n)))))
    (measure-seconds fn :rounds rounds)))

(defun run-benchmark (&key (n 300) (rounds 3))
  (format t "n=~D rounds=~D~%" n rounds)

  (maybe-gc)
  (let ((t-runtime (bench-runtime-init n rounds)))
    (format t "runtime table build      : ~,6Fs~%" t-runtime)

    (maybe-gc)
    (let ((t-macro-compile (bench-macro-compile n rounds)))
      (format t "macro compile/expand    : ~,6Fs~%" t-macro-compile)

      (maybe-gc)
      (let ((t-macro-runtime (bench-macro-runtime n rounds)))
        (format t "macro runtime init      : ~,6Fs~%" t-macro-runtime)
        (when (> t-macro-runtime 0)
          (format t "runtime/macro-runtime  : ~,3Fx~%"
                  (/ t-runtime t-macro-runtime)))
        (unless (> t-macro-runtime 0)
          (format t "runtime/macro-runtime  : <timer-resolution~%"))
        (when (> t-macro-compile 0)
          (format t "runtime/macro-compile  : ~,3Fx~%"
                  (/ t-runtime t-macro-compile)))
        (unless (> t-macro-compile 0)
          (format t "runtime/macro-compile  : <timer-resolution~%"))))))

(run-benchmark)
