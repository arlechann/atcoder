(defparameter mod-number 1000000007)

(defun unfold (p f g seed &optional (tail-gen (lambda () '())))
  (if (p seed)
      (tail-gen seed)
	  (cons (f seed)
	        (unfold p f g (g seed) tail-gen))))
  
(defun iota (c &optional (s 0) (step 1) (acc nil))
  (if (zerop c)
      (reverse acc)
      (iota (1- c) (+ s step) step (cons s acc))))
 
(defun sum (list &optional (init 0))
  (reduce #'+ list :initial-value init))
 
(defmacro debug-print (x)
  `(let ((y ,x))
    (format t "~A: ~A~%" ',x y)
	y))

(defun main ())
