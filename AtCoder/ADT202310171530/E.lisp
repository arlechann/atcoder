(defun input ()
  (let* ((n (read))
         (m (read))
         (a (loop repeat n collect (read)))
         (b (loop repeat m collect (read))))
    (values n m a b)))

(defun meguru-method (ok ng pred)
  (if (<= (abs (- ok ng)) 1)
      ok
      (let ((mid (floor (+ ok ng) 2)))
        (if (funcall pred mid)
            (meguru-method mid ng pred)
            (meguru-method ok mid pred)))))

(defun main ()
  (multiple-value-bind (n m a b) (input)
    (let ((c (sort (make-array (+ n m) :initial-contents (append a b))
                   (lambda (a b) (< a b)))))
      (dolist (elm a)
        (let ((result (meguru-method 0
                                     (length c)
                                     (lambda (index)
                                       (>= elm (aref c index))))))
          (format t "~A " (1+ result))))
      (terpri)
      (dolist (elm b)
        (let ((result (meguru-method 0
                                     (length c)
                                     (lambda (index)
                                       (>= elm (aref c index))))))
          (format t "~A " (1+ result))))
      (terpri))))

(main)
