(defun main ())
 
(defun iota (c &optional (s 0) (step 1) (acc nil))
  (if (zerop c)
      (reverse acc)
      (iota (1- c) (+ s step) step (cons s acc))))
 
(defun sum (list &optional (acc 0))
  (if (null list)
      acc
      (sum (cdr list) (+ (car list) acc))))
 
(defparameter n (read))
(defparameter k (read))
 
(defparameter a (make-array (1+ n) :initial-contents (iota (1+ n))))
(defparameter s (make-array (1+ n) :initial-contents (cdr (reverse (reduce (lambda (acc n) (cons (+ n (car acc)) acc)) a :initial-value '(0))))))
 
(defun low (pick-num)
  (aref s pick-num))
 
(defun high (pick-num)
  (- (aref s n) (aref s (- n pick-num))))
 
(defun solve (pick-num)
  (- (high pick-num) (low pick-num)))
 
(format t "~A~%" (sum (mapcar #'solve (iota (- (1+ n) k k)))))