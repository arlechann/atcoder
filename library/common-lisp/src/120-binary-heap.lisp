;;; binary-heap
;;;
(defpackage binary-heap
  (:use :cl :utility :vector-bintree)
  (:export :make-binary-heap
           :binary-heap-size
           :binary-heap-empty-p
           :binary-heap-push
           :binary-heap-top
           :binary-heap-pop
           ))
(in-package binary-heap)

(defconstant +binary-heap-default-buffer-size+ 64)

(defstruct (binary-heap (:constructor %make-binary-heap (op bintree)))
  op bintree)

(defun downheap (bintree index op)
  (labels ((downheap-swap (index child-index)
             (unless (funcall op (bintree-ref bintree index) (bintree-ref bintree child-index))
               (rotatef (bintree-ref bintree index) (bintree-ref bintree child-index))
               (downheap-rec child-index)))
           (downheap-rec (index)
             (let ((size (bintree-size bintree))
                   (left-index (bintree-left-index index))
                   (right-index (bintree-right-index index)))
               (when (>= left-index size)
                 (return-from downheap-rec))
               (when (>= right-index size)
                 (downheap-swap index left-index)
                 (return-from downheap-rec))
               (if (funcall op (bintree-ref bintree left-index) (bintree-ref bintree right-index))
                   (downheap-swap index left-index)
                   (downheap-swap index right-index)))))
    (downheap-rec index)))

(defun make-binary-heap (op &rest args &key initial-contents)
  "Call with #'<, makes ascending order."
  (let* ((len (length initial-contents))
         (bintree (apply #'make-extensible-vector-bintree
                         (if (null initial-contents) +binary-heap-default-buffer-size+ len)
                         args)))
    (unless (null initial-contents)
      (loop for index downfrom (1- len) downto 0
            do (downheap bintree index op)))
    (%make-binary-heap op bintree)))

(defun binary-heap-size (heap) (bintree-size (binary-heap-bintree heap)))
(defun binary-heap-empty-p (heap) (zerop (binary-heap-size heap)))

(defun upheap (bintree index op)
  (when (bintree-root-index-p index)
    (return-from upheap))
  (let ((parent-index (bintree-parent-index index)))
    (unless (funcall op (bintree-ref bintree parent-index) (bintree-ref bintree index))
      (rotatef (bintree-ref bintree parent-index) (bintree-ref bintree index))
      (upheap bintree parent-index op))))

(defun binary-heap-push (obj heap)
  (bintree-push obj (binary-heap-bintree heap))
  (upheap (binary-heap-bintree heap)
          (1- (binary-heap-size heap))
          (binary-heap-op heap))
  heap)

(defun binary-heap-pop (heap)
  (let ((op (binary-heap-op heap))
        (bintree (binary-heap-bintree heap))
        (size (binary-heap-size heap)))
    (rotatef (bintree-ref bintree 0) (bintree-ref bintree (1- size)))
    (bintree-pop bintree)
    (downheap bintree 0 op)
    heap))

(defun binary-heap-top (heap)
  (bintree-ref (binary-heap-bintree heap) 0))

(defun (setf binary-heap-top) (value heap)
  (let ((bintree (binary-heap-bintree heap)))
    (setf (bintree-ref bintree 0) value)
    (downheap bintree 0 (binary-heap-op heap))
    value))

(defun binary-heap-print (heap) (bintree-print (binary-heap-bintree heap)))

;;;
