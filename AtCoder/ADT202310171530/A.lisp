(defun main ()
  (let ((s (read-line)))
    (format t "~A~%" (if (string= s "Hello,World!")
                         "AC"
                         "WA"))))

(main)
