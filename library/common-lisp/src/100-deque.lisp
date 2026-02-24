;;; deque
;;;
(defpackage deque
  (:use :cl :utility)
  (:export :make-deque
           :deque-size
           :deque-empty-p
           :deque-push-front
           :deque-push-back
           :deque-pop-front
           :deque-pop-back
           :deque-peek-front
           :deque-peek-back
           :deque-ref
           ))
(in-package deque)

(defun make-deque-buffer (size &key (element-type t))
 (make-array size :element-type element-type))

(defconstant +deque-default-buffer-size+ 64)

(defstruct (deque (:constructor make-deque
                      (&aux (buffer (make-deque-buffer +deque-default-buffer-size+)))))
  (size 0 :type fixnum)
  (capacity +deque-default-buffer-size+ :type fixnum)
  (front-index 0 :type fixnum)
  (back-index (1- +deque-default-buffer-size+) :type fixnum)
  (buffer (make-deque-buffer 64) :type simple-array))

(setf (documentation 'make-deque 'function)
      "空の deque を生成して返す。")
(setf (documentation 'deque-size 'function)
      "deque の要素数を返す。")

(defun deque-empty-p (deque)
  "DEQUE が空なら真を返す。"
  (zerop (deque-size deque)))
(defun deque-full-p (deque) (= (deque-size deque) (deque-capacity deque)))

(defun deque-index-in-range-p (deque index)
  (and (integerp index)
       (<= 0 index)
       (< index (deque-size deque))))

(defun deque-buffer-ref (deque index) (aref (deque-buffer deque) index))
(defun (setf deque-buffer-ref) (x deque index) (setf (aref (deque-buffer deque) index) x))
(defun round-index (capacity index) (logand (1- capacity) index))
(defun inc-index (capacity index) (round-index capacity (1+ index)))
(defun dec-index (capacity index) (round-index capacity (1- index)))

(defun deque-front (deque)
  (deque-buffer-ref deque (deque-front-index deque)))

(defun deque-back (deque)
  (deque-buffer-ref deque (deque-back-index deque)))

(defun inc-front-index (deque)
  (setf (deque-front-index deque)
        (inc-index (deque-capacity deque) (deque-front-index deque))))

(defun dec-front-index (deque)
  (setf (deque-front-index deque)
        (dec-index (deque-capacity deque) (deque-front-index deque))))

(defun inc-back-index (deque)
  (setf (deque-back-index deque)
        (inc-index (deque-capacity deque) (deque-back-index deque))))

(defun dec-back-index (deque)
  (setf (deque-back-index deque)
        (dec-index (deque-capacity deque) (deque-back-index deque))))

(defun (setf deque-front) (x deque) (setf (deque-buffer-ref deque (deque-front-index deque)) x))
(defun (setf deque-back) (x deque) (setf (deque-buffer-ref deque (deque-back-index deque)) x))

(defun deque-extends-buffer (deque)
  (let* ((prev-capacity (deque-capacity deque))
         (prev-buffer (deque-buffer deque))
         (new-capacity (* prev-capacity 2))
         (new-buffer (make-deque-buffer new-capacity
                                        :element-type (array-element-type
                                                       prev-buffer))))
    (loop repeat prev-capacity
          for prev-index = (deque-front-index deque)
            then (inc-index prev-capacity prev-index)
          for new-index from 0
          do (setf (aref new-buffer new-index)
                   (aref prev-buffer prev-index))
          finally (setf (deque-capacity deque) new-capacity
                        (deque-buffer deque) new-buffer
                        (deque-front-index deque) 0
                        (deque-back-index deque) (1- prev-capacity))
                  (return deque))))

(defun deque-push-front (deque x)
  "先頭に X を追加し、DEQUE を返す。"
  (when (deque-full-p deque)
    (deque-extends-buffer deque))
  (dec-front-index deque)
  (incf (deque-size deque))
  (setf (deque-front deque) x)
  deque)

(defun deque-push-back (deque x)
  "末尾に X を追加し、DEQUE を返す。"
  (when (deque-full-p deque)
    (deque-extends-buffer deque))
  (inc-back-index deque)
  (incf (deque-size deque))
  (setf (deque-back deque) x)
  deque)

(defun deque-pop-front (deque)
  "先頭要素を削除し、DEQUE を返す。"
  #-atcoder
  (when (deque-empty-p deque)
    (error "DEQUE is empty. Cannot pop any element."))
  (setf (deque-buffer-ref deque (deque-front-index deque)) nil)
  (inc-front-index deque)
  (decf (deque-size deque))
  deque)

(defun deque-pop-back (deque)
  "末尾要素を削除し、DEQUE を返す。"
  #-atcoder
  (when (deque-empty-p deque)
    (error "DEQUE is empty. Cannot pop any element."))
  (setf (deque-buffer-ref deque (deque-back-index deque)) nil)
  (dec-back-index deque)
  (decf (deque-size deque))
  deque)

(defun deque-peek-front (deque)
  "先頭要素を返す（削除しない）。"
  #-atcoder
  (when (deque-empty-p deque)
    (error "DEQUE is empty. Cannot peek front."))
  (deque-front deque))

(defun deque-peek-back (deque)
  "末尾要素を返す（削除しない）。"
  #-atcoder
  (when (deque-empty-p deque)
    (error "DEQUE is empty. Cannot peek back."))
  (deque-back deque))


(defun deque-ref (deque index)
  "先頭から INDEX 番目（0始まり）の要素を返す。"
  #-atcoder
  (unless (deque-index-in-range-p deque index)
    (error "DEQUE-REF index out of range: ~S (size=~S)." index (deque-size deque)))
  (deque-buffer-ref deque
                    (round-index (deque-capacity deque)
                                 (+ (deque-front-index deque)
                                    index))))

(defun (setf deque-ref) (x deque index)
  #-atcoder
  (unless (deque-index-in-range-p deque index)
    (error "(SETF DEQUE-REF) index out of range: ~S (size=~S)." index (deque-size deque)))
  (setf (deque-buffer-ref deque
                          (round-index (deque-capacity deque)
                                       (+ (deque-front-index deque)
                                          index)))
        x))

;;;
