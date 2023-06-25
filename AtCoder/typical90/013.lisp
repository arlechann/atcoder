(in-package :cl-user)

;;;
;;; utility
;;;
(defpackage :utility
  (:use :cl)
  (:export nlet
           swap
           div
           half
           ))
(in-package :utility)

(declaim (inline div half))

(defmacro nlet (name binds &body body)
  `(labels ((,name ,(mapcar #'car binds) ,@body))
     (,name ,@(mapcar #'cadr binds))))

(defmacro swap (a b)
  (let ((tmp (gensym)))
    `(let ((,tmp ,a))
       (setf ,a ,b)
       (setf ,b ,tmp))))

(defun div (a b) 
  (declare (type fixnum a b))
  (the fixnum (floor a b)))

(defun half (n)
  (declare (type fixnum n))
  (the fixnum (ash n -1)))

;;;
;;; binary-heap
;;;
(defpackage :binary-heap
  (:use :cl :utility)
  (:export :make-binary-heap
           :binary-heap-push
           :binary-heap-pop
           :binary-heap-peek
           :binary-heap-null-p
           ))
(in-package :binary-heap)

(declaim (inline binary-heap-push
                 binary-heap-pop
                 binary-heap-peek
                 binary-heap-null-p
                 ))

(defstruct binary-heap (data (make-array 256 :adjustable t :fill-pointer 0)) (comp #'<))

(defun up-heap (data comp index)
  (unless (zerop index)
          (let ((parent (half (1- index))))
            (when (funcall comp (aref data index) (aref data parent))
                  (swap (aref data index) (aref data parent))
                  (up-heap data comp parent)))))

(defun down-heap (data comp index)
  (let ((left (1+ (* index 2)))
        (right (+ 2 (* index 2))))
    (cond ((>= left (length data)) nil)
          ((>= right (length data))
            (when (funcall comp (aref data left) (aref data index))
                  (swap (aref data left) (aref data index))
                  (down-heap data comp left)))
          (t (let ((child (if (funcall comp (aref data left) (aref data right))
                              left
                              right)))
               (when (funcall comp (aref data child) (aref data index))
                     (swap (aref data child) (aref data index))
                     (down-heap data comp child)))))))

(defun binary-heap-push (heap x)
  (vector-push-extend x (binary-heap-data heap))
  (up-heap (binary-heap-data heap)
           (binary-heap-comp heap)
           (1- (length (binary-heap-data heap)))))

(defun binary-heap-pop (heap)
  (swap (aref (binary-heap-data heap) 0)
        (aref (binary-heap-data heap) (1- (length (binary-heap-data heap)))))
  (let ((value (vector-pop (binary-heap-data heap))))
    (down-heap (binary-heap-data heap)
               (binary-heap-comp heap)
               0)
    value))

(defun binary-heap-peek (heap)
  (aref (binary-heap-data heap) 0))

(defun binary-heap-null-p (heap)
  (zerop (length (binary-heap-data heap))))

;;;
;;; atcoder
;;;
(defpackage :atcoder
  (:use :cl
        :utility
        :binary-heap))
(in-package :atcoder)

(defun dijkstra (edges start goal)
  (let* ((n (length edges))
         (distances (make-array n :initial-element most-positive-fixnum))
         (used (make-array n :initial-element nil))
         (heap (make-binary-heap :comp (lambda (a b) (< (car a) (car b))))))
    (loop never (binary-heap-null-p heap)
          initially (setf (aref distances start) 0)
                    (binary-heap-push heap (cons 0 start))
          do (let* ((top (binary-heap-pop heap))
                    (d (car top))
                    (node (cdr top)))
               (cond ((= node goal) (return))
                     ((aref used node) nil)
                     (t (setf (aref used node) t)
                        (loop for edge across (aref edges node)
                              do (let* ((next (car edge))
                                        (cost (cdr edge)))
                                   (when (< (+ d cost) (aref distances next))
                                         (setf (aref distances next) (+ d cost))
                                         (binary-heap-push heap (cons (+ d cost) next)))))))))
    distances))

(defun solve (n m a b c)
  (let ((edges (loop with edges = (make-array n)
                     for i from 0 below n
                     do (setf (aref edges i) (make-array 0 :adjustable t :fill-pointer 0))
                     finally (return edges))))
    (loop for i below m
          do (let ((from (aref a i))
                   (to (aref b i))
                   (cost (aref c i)))
               (vector-push-extend (cons to cost) (aref edges from))
               (vector-push-extend (cons from cost) (aref edges to))))
    (let ((from-0 (dijkstra edges 0 -1))
          (from-n (dijkstra edges (1- n) -1)))
      (loop for i below n
            collect (+ (aref from-0 i) (aref from-n i))))))

(defun main ()
  ;;(declaim (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((n (read))
         (m (read))
         (a (make-array m))
         (b (make-array m))
         (c (make-array m)))
    (declare (type fixnum n m))
    (declare (type (simple-vector fixnum (*)) a b c))
    (loop for i from 0 below m
          do (setf (aref a i) (1- (read)))
          do (setf (aref b i) (1- (read)))
          do (setf (aref c i) (read)))
    (let ((result (solve n m a b c)))
      (loop for x in result do (format t "~D~%" x)))))

#-swank (main)
