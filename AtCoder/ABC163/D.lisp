(defvar mod-number 1000000007)
 
(defun main ())
 
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
 
(defparameter n (read))
(defparameter k (read))
 
(defparameter s
  (make-array (+ 2 n)
    :initial-contents (reverse
	                    (reduce
						  (lambda (acc n)
						    (cons (+ n (car acc))
							      acc))
						  (iota (1+ n))
	                      :initial-value '(0)))))
 
(defun high (pick-num)
  (- (aref s (1+ n)) (aref s (- (1+ n) pick-num))))
 
(defun low (pick-num)
  (aref s pick-num))
 
(defun solve (pick-num)
  (1+ (- (high pick-num) (low pick-num))))
 
(format t "~A~%" (reduce (lambda (x y) (rem (+ x y) mod-number)) (mapcar #'solve (iota (- (1+ n) (1- k)) k))))