;;; union-find
;;;
(defpackage union-find
  (:use :cl :utility)
  (:export :make-union-find
           :union-find-size
           :union-find-merge
           :union-find-unite-p
           :union-find-groups
           ))
(in-package union-find)

(defun make-union-find (size)
  (let ((parents (make-array size
                             :element-type 'fixnum
                             :initial-contents (iota size)))
        (ranks (make-array size
                           :element-type 'fixnum
                           :initial-element 0))
        (group-sizes (make-array size
                                 :element-type 'fixnum
                                 :initial-element 1)))
    (list parents ranks group-sizes)))

(defun union-find-size (uf) (length (first uf)))
(defun union-find-parent (uf n) (aref (first uf) n))
(defun union-find-rank (uf n) (aref (second uf) n))
(defun (setf union-find-parent) (parent uf n) (setf (aref (first uf) n) parent))
(defun (setf union-find-rank) (rank uf n)(setf (aref (second uf) n) rank))
(defun (setf union-find-group-size) (group-size uf n) (setf (aref (third uf) n) group-size))

(defun union-find-root (uf n)
  (let ((parent (union-find-parent uf n)))
    (if (= parent n)
        n
        (setf (union-find-parent uf n) (union-find-root uf parent)))))

(defun union-find-group-size (uf n) (aref (third uf) (union-find-root uf n)))

(defun union-find-merge (uf a b)
  (let ((ar (union-find-root uf a))
        (br (union-find-root uf b)))
    (when (= ar br)
      (return-from union-find-merge nil))
    (let ((ar-rank (union-find-rank uf ar))
          (br-rank (union-find-rank uf br)))
      (when (< ar-rank br-rank)
        (rotatef a b)
        (rotatef ar br)
        (rotatef ar-rank br-rank))
      (when (= ar-rank br-rank)
        (incf (union-find-rank uf ar)))
      (incf (union-find-group-size uf ar)
            (union-find-group-size uf br))
      (setf (union-find-parent uf br) ar)
      t)))

(defun union-find-unite-p (uf a b) (= (union-find-root uf a) (union-find-root uf b)))

(defun union-find-groups (uf)
  (let ((size (union-find-size uf)))
    (dotimes (i size)
      (union-find-root uf i))
    (let ((groups (make-array size :initial-element nil)))
      (dotimes (i size)
        (push i (aref groups (union-find-root uf i))))
      (coerce (remove-if #'null groups) 'list))))

;;;
