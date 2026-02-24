;;; ordered-map
;;;
(defpackage ordered-map
  (:use :cl :utility)
  (:export :make-rbtree
           :rbtree-search
           :rbtree-each
           :rbtree-to-list
           :rbtree-lower-bound
           :rbtree-upper-bound
           :rbtree-first
           :rbtree-last
           :rbtree-insert
           :rbtree-remove
           :rbtree-print
           ))
(in-package ordered-map)

;;; node

(declaim (inline node-empty-p node-color node-left node-value node-right node-black-p node-red-p))

(defun make-node (&key color left value right) (list color left value right))
(defun make-empty-node () nil)
(defun node-empty-p (node) (null node))
(defun node-color (node) (if (node-empty-p node) 'black (first node)))
(defun node-left (node) (second node))
(defun node-value (node) (third node))
(defun node-right (node) (fourth node))

(defun (setf node-color) (color node)
  (if (node-empty-p node)
      (progn
        #-atcoder
        (unless (eq color 'black)
          (error "Leaf is must be black."))
        'black)
      (setf (first node) color)))

(defun (setf node-value) (value node) (setf (third node) value))
(defun (setf node-left) (left node) (setf (second node) left))
(defun (setf node-right) (right node) (setf (fourth node) right))
(defun node-black-p (node) (eq (node-color node) 'black))
(defun node-red-p (node) (eq (node-color node) 'red))

(defun node-each (node fn)
  (unless (node-empty-p node)
    (node-each (node-left node) fn)
    (funcall fn (node-value node))
    (node-each (node-right node) fn)))

(defun node-search (node search-key key key-eq-p key-less-p &key default)
  (if (node-empty-p node)
      (values default nil)
      (let ((node-key (funcall key (node-value node))))
        (match:match (list (funcall key-eq-p search-key node-key)
                           (funcall key-less-p search-key node-key))
          ((list t _) (values (node-value node) t))
          ((list nil t)
           (node-search (node-left node)
                        search-key
                        key
                        key-eq-p
                        key-less-p
                        :default default))
          (otherwise
           (node-search (node-right node)
                        search-key
                        key
                        key-eq-p
                        key-less-p
                        :default default))))))

(defun node-lower-bound (node search-key key key-eq-p key-less-p &key end)
  (if (node-empty-p node)
      (values end nil)
      (let ((node-key (funcall key (node-value node))))
        (if (or (funcall key-less-p search-key node-key)
                (funcall key-eq-p search-key node-key))
            (multiple-value-bind (value is-found)
                (node-lower-bound (node-left node)
                                  search-key
                                  key
                                  key-eq-p
                                  key-less-p
                                  :end end)
              (if is-found
                  (values value t)
                  (values (node-value node) t)))
            (node-lower-bound (node-right node)
                              search-key
                              key
                              key-eq-p
                              key-less-p
                              :end end)))))

(defun node-upper-bound (node search-key key key-eq-p key-less-p &key end)
  (if (node-empty-p node)
      (values end nil)
      (let ((node-key (funcall key (node-value node))))
        (if (funcall key-less-p search-key node-key)
            (multiple-value-bind (value is-found)
                (node-upper-bound (node-left node)
                                  search-key
                                  key
                                  key-eq-p
                                  key-less-p
                                  :end end)
              (if is-found
                  (values value t)
                  (values (node-value node) t)))
            (node-upper-bound (node-right node)
                              search-key
                              key
                              key-eq-p
                              key-less-p
                              :end end)))))

(macrolet ((%defun-rotate (a b)
             (let ((node-a (symb 'node- a))
                   (node-b (symb 'node- b))
                   (b-node (symb b '-node)))
               `(defun ,(symb 'rotate- a) (node)
                  (let ((,b-node (,node-b node)))
                    (setf (,node-b node) (,node-a ,b-node)
                          (,node-a ,b-node) node
                          (node-color ,b-node) (node-color node)
                          (node-color node) 'red)
                    ,b-node)))))
  (%defun-rotate right left)
  (%defun-rotate left right))
                  
(defun split-4node (node)
  (setf (node-color node) 'red
        (node-color (node-left node)) 'black
        (node-color (node-right node)) 'black)
  node)

(defun node-first (node)
  (let ((lnode (node-left node)))
    (if (node-empty-p lnode)
        node
        (node-first lnode))))

(defun node-last (node)
  (let ((rnode (node-right node)))
    (if (node-empty-p rnode)
        node
        (node-last rnode))))

(declaim (ftype (function (t t t t t) (values t boolean))
                node-insert-left
                node-insert-right
                node-remove-here
                node-remove-left
                node-remove-right))
(declaim (ftype (function (t boolean) (values t boolean))
                balance-insert-left
                balance-insert-right
                balance-remove-left
                balance-remove-right))

(defun node-insert (node value key key-eq-p key-less-p)
  (if (node-empty-p node)
      (values (make-node :color 'red :value value) t)
      (let ((new-key (funcall key value))
            (node-key (funcall key (node-value node))))
        (match:match (list (funcall key-eq-p new-key node-key)
                           (funcall key-less-p new-key node-key))
          ((list t _)
           (setf (node-value node) value)
           (values node nil))
          ((list nil t)
           (node-insert-left node value key key-eq-p key-less-p))
          (otherwise
           (node-insert-right node value key key-eq-p key-less-p))))))

(macrolet ((%defun-node-insert (a b)
             (declare (ignorable b))
             (let ((node-a (symb 'node- a))
                   (a-node (symb a '-node)))
               `(defun ,(symb 'node-insert- a) (node value key key-eq-p key-less-p)
                  (multiple-value-bind (,a-node needs-balance)
                      (node-insert (,node-a node) value key key-eq-p key-less-p)
                    (setf (,node-a node) ,a-node)
                    (,(symb 'balance-insert- a) node needs-balance))))))
  (%defun-node-insert left right)
  (%defun-node-insert right left))

(defun balance-insert-left (node needs-balance)
  (if (or (not needs-balance) (eq (node-color node) 'red))
      (values node needs-balance)
      (match:match node
        ((list _ (list 'red (list 'red _ _ _) _ _) _ (list 'red _ _ _))
         (setf node (split-4node node))
         (values node t))
        ((list _ (list 'red _ _ (list 'red _ _ _)) _ (list 'red _ _ _))
         (setf node (split-4node node))
         (values node t))
        ((list _ (list 'red (list 'red _ _ _) _ _) _ _)
         (values (rotate-right node) nil))
        ((list _ (list 'red _ _ (list 'red _ _ _)) _ _)
         (setf (node-left node) (rotate-left (node-left node)))
         (values (rotate-right node) nil))
        (otherwise
         (values node nil)))))

(defun balance-insert-right (node needs-balance)
  (if (or (not needs-balance) (eq (node-color node) 'red))
      (values node needs-balance)
      (match:match node
        ((list _ (list 'red _ _ _) _ (list 'red _ _ (list 'red _ _ _)))
         (setf node (split-4node node))
         (values node t))
        ((list _ (list 'red _ _ _) _ (list 'red (list 'red _ _ _) _ _))
         (setf node (split-4node node))
         (values node t))
        ((list _ _ _ (list 'red _ _ (list 'red _ _ _)))
         (values (rotate-left node) nil))
        ((list _ _ _ (list 'red (list 'red _ _ _) _ _))
         (setf (node-right node) (rotate-right (node-right node)))
         (values (rotate-left node) nil))
        (otherwise
         (values node nil)))))

(defun node-remove (node remove-key key key-eq-p key-less-p)
  (if (node-empty-p node)
      (values node nil)
      (let ((node-key (funcall key (node-value node))))
        (match:match (list (funcall key-eq-p remove-key node-key)
                           (funcall key-less-p remove-key node-key))
          ((list t _)
           (node-remove-here node remove-key key key-eq-p key-less-p))
          ((list nil t)
           (node-remove-left node remove-key key key-eq-p key-less-p))
          (otherwise
           (node-remove-right node remove-key key key-eq-p key-less-p))))))

(defun node-remove-here (node remove-key key key-eq-p key-less-p)
  (declare (ignore remove-key))
  (let ((lnode (node-left node))
        (rnode (node-right node)))
    (match:match (list (node-empty-p lnode) (node-empty-p rnode))
      ((list t t) (values (make-empty-node) (node-black-p node)))
      ((list t nil)
       (setf (node-color rnode) 'black)
       (values rnode nil))
      ((list nil t)
       (setf (node-color lnode) 'black)
       (values lnode nil))
      (otherwise
       (let ((first-node (node-first rnode)))
         (setf (node-value node) (node-value first-node))
         (multiple-value-bind (new-rnode needs-balance)
             (node-remove rnode
                          (funcall key (node-value first-node))
                          key
                          key-eq-p
                          key-less-p)
           (setf (node-right node) new-rnode)
           (balance-remove-right node needs-balance)))))))

(defun node-remove-left (node remove-key key key-eq-p key-less-p)
  (multiple-value-bind (new-left needs-balance)
      (node-remove (node-left node) remove-key key key-eq-p key-less-p)
    (setf (node-left node) new-left)
    (balance-remove-left node needs-balance)))

(defun balance-remove-left (node needs-balance)
  (if (not needs-balance)
      (values node nil)
      (match:match node
        ((list _ _ _ nil)
         (values node needs-balance))
        ((list 'black left _ (list 'black rleft _ rright))
         (if (and (match:if-match (or nil (list 'black _ _ _)) left t nil)
                  (match:if-match (or nil (list 'black _ _ _)) rleft t nil)
                  (match:if-match (or nil (list 'black _ _ _)) rright t nil))
             (progn
               (setf (node-color (node-right node)) 'red)
               (values node t))
             (if (and (match:if-match (list 'red _ _ _) rleft t nil)
                      (match:if-match (or nil (list 'black _ _ _)) rright t nil))
                 (progn
                   (setf (node-right node) (rotate-right (node-right node)))
                   (balance-remove-left node t))
                 (let ((new-node (rotate-left node)))
                   (setf (node-color (node-left new-node)) 'black
                         (node-color (node-right new-node)) 'black)
                   (values new-node nil)))))
        ((list 'black left _ (list 'red _ _ _))
         (if (match:if-match (or nil (list 'black _ _ _)) left t nil)
             (let ((new-node (rotate-left node)))
               (multiple-value-bind (new-left needs-balance)
                   (balance-remove-left (node-left new-node) t)
                 (setf (node-left new-node) new-left)
                 (balance-remove-left new-node needs-balance)))
             (let ((new-node (rotate-left node)))
               (setf (node-color (node-left new-node)) 'black
                     (node-color (node-right new-node)) 'black)
               (values new-node nil))))
        ((list 'red _ _ (list _ rleft _ rright))
         (if (and (match:if-match (or nil (list 'black _ _ _)) rleft t nil)
                  (match:if-match (or nil (list 'black _ _ _)) rright t nil))
             (progn
               (rotatef (node-color node) (node-color (node-right node)))
               (values node nil))
             (let ((new-node (rotate-left node)))
               (setf (node-color (node-left new-node)) 'black
                     (node-color (node-right new-node)) 'black)
               (values new-node nil))))
        (otherwise
         (let ((new-node (rotate-left node)))
           (setf (node-color (node-left new-node)) 'black
                 (node-color (node-right new-node)) 'black)
           (values new-node nil))))))

(defun node-remove-right (node remove-key key key-eq-p key-less-p)
  (multiple-value-bind (new-right needs-balance)
      (node-remove (node-right node) remove-key key key-eq-p key-less-p)
    (setf (node-right node) new-right)
    (balance-remove-right node needs-balance)))

(defun balance-remove-right (node needs-balance)
  (if (not needs-balance)
      (values node nil)
      (match:match node
        ((list _ nil _ _)
         (values node needs-balance))
        ((list 'black (list 'black lleft _ lright) _ right)
         (if (and (match:if-match (or nil (list 'black _ _ _)) right t nil)
                  (match:if-match (or nil (list 'black _ _ _)) lleft t nil)
                  (match:if-match (or nil (list 'black _ _ _)) lright t nil))
             (progn
               (setf (node-color (node-left node)) 'red)
               (values node t))
             (if (and (match:if-match (list 'red _ _ _) lright t nil)
                      (match:if-match (or nil (list 'black _ _ _)) lleft t nil))
                 (progn
                   (setf (node-left node) (rotate-left (node-left node)))
                   (balance-remove-right node t))
                 (let ((new-node (rotate-right node)))
                   (setf (node-color (node-left new-node)) 'black
                         (node-color (node-right new-node)) 'black)
                   (values new-node nil)))))
        ((list 'black (list 'red _ _ _) _ right)
         (if (match:if-match (or nil (list 'black _ _ _)) right t nil)
             (let ((new-node (rotate-right node)))
               (multiple-value-bind (new-right needs-balance)
                   (balance-remove-right (node-right new-node) t)
                 (setf (node-right new-node) new-right)
                 (balance-remove-right new-node needs-balance)))
             (let ((new-node (rotate-right node)))
               (setf (node-color (node-left new-node)) 'black
                     (node-color (node-right new-node)) 'black)
               (values new-node nil))))
        ((list 'red (list _ lleft _ lright) _ _)
         (if (and (match:if-match (or nil (list 'black _ _ _)) lleft t nil)
                  (match:if-match (or nil (list 'black _ _ _)) lright t nil))
             (progn
               (rotatef (node-color node) (node-color (node-left node)))
               (values node nil))
             (let ((new-node (rotate-right node)))
               (setf (node-color (node-left new-node)) 'black
                     (node-color (node-right new-node)) 'black)
               (values new-node nil))))
        (otherwise
         (let ((new-node (rotate-right node)))
           (setf (node-color (node-left new-node)) 'black
                 (node-color (node-right new-node)) 'black)
           (values new-node nil))))))

(defun node-print (node depth &key (stream t) show-nil)
  (if (node-empty-p node)
      (when show-nil
        (loop repeat depth do (format stream "    "))
        (format stream "NIL~%"))
      (progn
        (node-print (node-right node) (1+ depth) :stream stream :show-nil show-nil)
        (loop repeat depth do (format stream "    "))
        (format stream "(~A,~S)~%" (symbol-name (node-color node)) (node-value node))
        (node-print (node-left node) (1+ depth) :stream stream :show-nil show-nil))))

;;; rbtree

(defun make-rbtree (&key root (key #'identity) (key-eq-p #'eql) (key-less-p #'<))
  "空または ROOT 指定の赤黒木マップを作成して返す。"
  (list (or root (make-empty-node)) key key-eq-p key-less-p))

(defun rbtree-root (rbtree) (first rbtree))
(defun rbtree-key (rbtree) (second rbtree))
(defun rbtree-key-eq-p (rbtree) (third rbtree))
(defun rbtree-key-less-p (rbtree) (fourth rbtree))
(defun (setf rbtree-root) (root rbtree) (setf (first rbtree) root))
(defun rbtree-each (rbtree fn)
  "キー順（中順）に各要素へ FN を適用する。"
  (node-each (rbtree-root rbtree) fn))

(defun rbtree-to-list (rbtree)
  "木の要素をキー昇順のリストとして返す。"
  (let ((acc nil))
    (rbtree-each rbtree
                 (lambda (v)
                   (push v acc)))
    (nreverse acc)))

(defun rbtree-first (rbtree)
  "最小キーの要素を返す。空なら NIL。"
  (let ((node (node-first (rbtree-root rbtree))))
    (if (node-empty-p node)
        nil
        (node-value node))))

(defun rbtree-last (rbtree)
  "最大キーの要素を返す。空なら NIL。"
  (let ((node (node-last (rbtree-root rbtree))))
    (if (node-empty-p node)
        nil
        (node-value node))))

(defun rbtree-search (rbtree search-key &key default)
  "SEARCH-KEY に一致する要素を返す。未発見時は DEFAULT。"
  (node-search (rbtree-root rbtree)
               search-key
               (rbtree-key rbtree)
               (rbtree-key-eq-p rbtree)
               (rbtree-key-less-p rbtree)
               :default default))

(defun rbtree-lower-bound (rbtree search-key &key end)
  "SEARCH-KEY 以上の最小要素を返す。存在しなければ END。"
  (node-lower-bound (rbtree-root rbtree)
                    search-key
                    (rbtree-key rbtree)
                    (rbtree-key-eq-p rbtree)
                    (rbtree-key-less-p rbtree)
                    :end end))

(defun rbtree-upper-bound (rbtree search-key &key end)
  "SEARCH-KEY より大きい最小要素を返す。存在しなければ END。"
  (node-upper-bound (rbtree-root rbtree)
                    search-key
                    (rbtree-key rbtree)
                    (rbtree-key-eq-p rbtree)
                    (rbtree-key-less-p rbtree)
                    :end end))

(defun rbtree-insert (rbtree value)
  "VALUE を挿入（同一キーは上書き）し、RBTree を返す。"
  (multiple-value-bind (root needs-balance)
      (node-insert (rbtree-root rbtree)
                   value
                   (rbtree-key rbtree)
                   (rbtree-key-eq-p rbtree)
                   (rbtree-key-less-p rbtree))
    (if needs-balance
        (setf (node-color root) 'black))
    (setf (rbtree-root rbtree) root)
    rbtree))

(defun rbtree-remove (rbtree remove-key)
  "REMOVE-KEY の要素を削除し、RBTree を返す。"
  (let ((root (node-remove (rbtree-root rbtree)
                           remove-key
                           (rbtree-key rbtree)
                           (rbtree-key-eq-p rbtree)
                           (rbtree-key-less-p rbtree))))
    (unless (node-empty-p root)
      (setf (node-color root) 'black))
    (setf (rbtree-root rbtree) root)
    rbtree))

(defun rbtree-print (rbtree &key (stream t) show-nil)
  "木構造を整形して STREAM に出力する。"
  (node-print (rbtree-root rbtree) 0 :stream stream :show-nil show-nil))

;;;
