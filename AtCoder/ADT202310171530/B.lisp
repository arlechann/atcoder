(let ((s (read-line))
      (full "0123456789"))
  (let ((result (find-if-not (lambda (c1)
                               (find-if (lambda (c2)
                                          (char= c1 c2))
                                        s))
                             full)))
    (format t "~A~%" result)))