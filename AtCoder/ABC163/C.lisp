(defparameter n (read))
(defparameter a '())
(defparameter members (make-array n :adjustable t :fill-pointer 0))

(dotimes (i (1- n))
	(push (read) a))

(dotimes (i n)
	(vector-push '() members))

(dolist (e a)
	(push e (aref members (1- e))))

(map nil (lambda (m) (format t "~A~%" (length m))) members)

(defun main ())