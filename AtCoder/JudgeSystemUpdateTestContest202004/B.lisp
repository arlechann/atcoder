(defparameter n (read))

(defparameter r '())
(defparameter b '())

(dotimes (i n)
  (let* ((x (read))
         (c (read)))
    (if (string= c "R")
        (push x r)
        (push x b))))

(setf r (sort r #'<))
(setf b (sort b #'<))

(dolist (e (append r b))
  (format t "~A~%" e))

(defun main ())
