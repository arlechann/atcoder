(eval-when (:compile-toplevel)
  (proclaim '(optimize (speed 3) (debug 3) (safety 0))))

(defparameter inf 2000000000)

(defun split (x str &optional (acc (make-array 100001 :adjustable t :fill-pointer 0)))
  (declare (type string x str) (type vector acc))
  (let ((pos (search x str))
        (size (length x)))
    (declare (type integer size))
    (if pos
        (progn (vector-push (subseq str 0 pos) acc)
               (split x (subseq str (+ pos size)) acc))
        (progn (vector-push str acc)
               (the vector acc)))))

(defmacro minf (field &rest x)
  `(setf ,field (min ,field ,@x)))

(defmacro maxf (field &rest x)
  `(setf ,field (max ,field ,@x)))

(defun solve (stairs n)
  (let ((dp (make-array n :element-type 'integer :initial-element inf)))
    (setf (aref dp 0) 0)
    (dotimes (i (1- n))
      (minf (aref dp (1+ i)) (+ (aref dp i) (abs (- (aref stairs i) (aref stairs (1+ i))))))
      (if (< (+ i 2) n)
          (minf (aref dp (+ i 2)) (+ (aref dp i) (abs (- (aref stairs i) (aref stairs (+ i 2))))))))
    (aref dp (1- n))))

(let* ((n (parse-integer (read-line)))
       (stairs (map 'vector #'parse-integer (split " " (read-line)))))
  (format t "~A~%" (solve stairs n)))
