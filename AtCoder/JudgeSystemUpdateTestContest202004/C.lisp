(defun main ())

(defparameter a1 (read))
(defparameter a2 (read))
(defparameter a3 (read))

(defun solve (x y z)
  (if (and (= x a1) (= y a2) (= z a3))
      1
      (+ (if (< x a1)
             (solve (1+ x) y z)
             0)
         (if (and (>= x (1+ y))
                  (< y a2))
             (solve x (1+ y) z)
             0)
         (if (and (>= y (1+ z))
                  (< z a3))
             (solve x y (1+ z))
             0))))

(format t "~A~%" (solve 0 0 0))

