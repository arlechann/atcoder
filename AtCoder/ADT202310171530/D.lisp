(defun input ()
  (let* ((n (read))
         (m (read))
         (k (make-array m :initial-element 0 :element-type :fixnum))
         (x (make-array m :initial-element nil)))
    (loop for i from 0 below m
          do (setf (aref k i) (read))
          do (setf (aref x i)
                   (let ((size (aref k i)))
                     (make-array size
                                 :initial-contents (loop for j from 0 below size
                                                         collect (1- (read)))))))
    (values n m k x)))

(defun main ()
  (multiple-value-bind (n m k x) (input)
    (let ((part (make-array n :initial-element nil)))
      (loop for i from 0 below m
            do (loop for j from 0 below (aref k i)
                     do (push i (aref part (aref (aref x i) j)))))
      (loop for i from 0 below n
            do (loop for j from (1+ i) below n
                     do (if (null (intersection (aref part i) (aref part j)))
                            (progn (format t "No~%")
                                   (return-from main)))))
      (format t "Yes~%"))))

(main)
