;;; trie
;;;
(defpackage trie
  (:use :cl :utility)
  (:export :make-trie
           :trie-size
           :trie-find
           :trie-insert
           :trie-traverse
           :trie-node-char
           :trie-node-endp
           :trie-node-end-count
           :trie-node-prefix-count
           :trie-node-value
           ))
(in-package trie)

(defun make-trie-node (char) (vector char 0 0 (make-hash-table :test 'eql) nil))
(defun trie-node-char (node)
  "NODE が表す文字を返す。根ノードでは通常 NIL。"
  (aref node 0))
(defun trie-node-end-count (node)
  "NODE を終端とする登録キー数を返す。"
  (aref node 1))
(defun trie-node-endp (node)
  "NODE が少なくとも1つのキー終端かどうかを返す。"
  (not (zerop (trie-node-end-count node))))
(defun trie-node-prefix-count (node)
  "NODE を接頭辞として通過するキー数を返す。"
  (aref node 2))
(defun trie-node-next (node char) (values (gethash char (aref node 3) nil)))
(defun trie-node-value (node)
  "NODE に紐づく値を返す（終端ノードで主に利用）。"
  (aref node 4))
(defun (setf trie-node-end-count) (count node) (setf (aref node 1) count))
(defun (setf trie-node-prefix-count) (count node) (setf (aref node 2) count))
(defun (setf trie-node-next) (next node char) (setf (gethash char (aref node 3)) next))
(defun (setf trie-node-value) (value node) (setf (aref node 4) value))

(defun trie-node-find (node str index &key (prefixp nil))
  (when (= index (length str))
    (return-from trie-node-find
      (if (or prefixp (trie-node-endp node))
          (values t (trie-node-value node))
          nil)))
  (when-let ((next (trie-node-next node (aref str index))))
    (trie-node-find next str (1+ index) :prefixp prefixp)))

(defun trie-node-insert (node str index &optional value)
  (incf (trie-node-prefix-count node))
  (when (= index (length str))
    (let ((new-key-p (zerop (trie-node-end-count node))))
      (incf (trie-node-end-count node))
      (setf (trie-node-value node) value)
      (return-from trie-node-insert (values str value new-key-p))))
  (let* ((char (aref str index))
         (next (trie-node-next node char)))
    (trie-node-insert (if (null next)
                          (setf (trie-node-next node char)
                                (make-trie-node char))
                          next)
                      str (1+ index) value)))

(defun trie-node-traverse (node str index fn)
  (funcall fn node str index)
  (when (= index (length str))
    (return-from trie-node-traverse nil))
  (let ((next (trie-node-next node (aref str index))))
    (if (null next)
        nil
        (trie-node-traverse next str (1+ index) fn))))

(defun make-trie ()
  "空の trie を生成して返す。"
  (vector 0 (make-trie-node nil)))
(defun trie-size (trie)
  "trie に登録されたユニークキー数を返す。"
  (aref trie 0))
(defun (setf trie-size) (size trie) (setf (aref trie 0) size))
(defun trie-root (trie) (aref trie 1))

(defun trie-find (trie str &key (prefixp nil))
  "STR を検索する。見つかれば (values t value) を返す。計算量 O(|STR|)。"
  (trie-node-find (trie-root trie) str 0 :prefixp prefixp))

(defun trie-insert (trie str &optional value)
  "STR を trie に登録し、VALUE を終端ノードへ設定する。計算量 O(|STR|)。"
  (multiple-value-bind (inserted-key inserted-value new-key-p)
      (trie-node-insert (trie-root trie) str 0 value)
    (declare (ignore inserted-key inserted-value))
    (when new-key-p
      (incf (trie-size trie)))
    value))

(defun trie-traverse (trie str fn)
  "STR を辿る各ノードで FN を呼ぶ。FN は (node str index) を受ける。O(|STR|)。"
  (trie-node-traverse (trie-root trie) str 0 fn))

;;;
