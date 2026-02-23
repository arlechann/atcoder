;;; segment-tree
;;;
(defpackage segment-tree
  (:use :cl :utility :vector-bintree)
  (:export :make-segment-tree
           :segment-tree-ref
           :segment-tree-fold
           :segment-tree-print))
(in-package segment-tree)

(defstruct (segment-tree
            (:constructor %make-st (&key size capacity op id bintree bintree-size)))
  size capacity op id bintree bintree-size)

(defun segment-tree-index-in-range-p (st index)
  (and (integerp index)
       (<= 0 index)
       (< index (segment-tree-size st))))

(defun segment-tree-fold-range-valid-p (st left right)
  (and (integerp left)
       (integerp right)
       (<= 0 left)
       (<= left right)
       (<= right (segment-tree-size st))))

(defun make-segment-tree (size op id &key initial-contents)
  "セグメント木を構築して返す。OP は二項演算、ID は単位元。構築は O(n)。"
  #-atcoder
  (unless (and (integerp size) (> size 0))
    (error "MAKE-SEGMENT-TREE: size must be a positive integer, got ~S." size))
  (let* ((logical-size size)
         (capacity (next-pow2 size))
         (bintree-size (1- (* capacity 2)))
         (bintree (make-vector-bintree bintree-size :initial-element id)))
    (unless (null initial-contents)
      #-atcoder
      (when (> (length initial-contents) logical-size)
        (error "MAKE-SEGMENT-TREE: initial-contents length (~S) must be <= size (~S)."
               (length initial-contents) logical-size))
      (setf (subseq bintree (1- capacity) (+ (1- capacity) (length initial-contents)))
            initial-contents)
      (loop for index downfrom (- capacity 2) downto 0
            do (setf (bintree-ref bintree index)
                     (funcall op
                              (bintree-ref bintree (bintree-left-index index))
                              (bintree-ref bintree (bintree-right-index index))))))
    (%make-st :size logical-size
              :capacity capacity
              :op op
              :id id
              :bintree bintree
              :bintree-size bintree-size)))

(defun %st-index-to-bintree-index (st index) (1- (+ index (segment-tree-capacity st))))

(defun segment-tree-ref (st index)
  "葉 INDEX の値を返す。取得は O(1)。"
  #-atcoder
  (unless (segment-tree-index-in-range-p st index)
    (error "SEGMENT-TREE-REF: index out of range: ~S (size=~S)." index (segment-tree-size st)))
  (bintree-ref (segment-tree-bintree st) (%st-index-to-bintree-index st index)))

(defun %st-fold (st fold-left fold-right index node-left node-right)
  (when (or (<= node-right fold-left)
            (<= fold-right node-left))
    (return-from %st-fold (segment-tree-id st)))
  (when (and (<= fold-left node-left)
             (<= node-right fold-right))
    (return-from %st-fold
      (bintree-ref (segment-tree-bintree st) index)))
  (let ((node-mid (+ node-left (floor (- node-right node-left) 2))))
    (funcall (segment-tree-op st)
             (%st-fold st fold-left fold-right
                       (bintree-left-index index)
                       node-left
                       node-mid)
             (%st-fold st fold-left fold-right
                       (bintree-right-index index)
                       node-mid
                       node-right))))

(defun segment-tree-fold (st left right)
  "半開区間 [LEFT, RIGHT) の畳み込み結果を返す。計算量 O(log n)。"
  #-atcoder
  (unless (segment-tree-fold-range-valid-p st left right)
    (error "SEGMENT-TREE-FOLD: invalid range [~S, ~S) for size ~S."
           left right (segment-tree-size st)))
  (%st-fold st left right 0 0 (segment-tree-capacity st)))

(defun %st-set (st index value)
  (let ((bintree (segment-tree-bintree st)))
    (setf (bintree-ref bintree index) value)
    (loop with index = index
          while (> index 0)
          do (setf index
                   (bintree-parent-index index)
                   (bintree-ref bintree index)
                   (funcall (segment-tree-op st)
                            (bintree-ref bintree (bintree-left-index index))
                            (bintree-ref bintree (bintree-right-index index)))))))

(defun (setf segment-tree-ref) (value st index)
  "O(log(n))"
  #-atcoder
  (unless (segment-tree-index-in-range-p st index)
    (error "(SETF SEGMENT-TREE-REF): index out of range: ~S (size=~S)."
           index (segment-tree-size st)))
  (%st-set st (%st-index-to-bintree-index st index) value)
  value)

(defun segment-tree-print (st)
  "セグメント木の内部構造を標準出力向けに表示する。"
  (bintree-print (segment-tree-bintree st)))

;;;
