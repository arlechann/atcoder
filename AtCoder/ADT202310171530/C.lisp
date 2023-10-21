(defun input () (read-line))

(let* ((s (input))
       (result (sort s #'char<)))
  (format t "~A~%" result))
