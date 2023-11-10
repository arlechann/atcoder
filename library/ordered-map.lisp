(in-package :cl-user)

;;;
;;; ordered-map
;;;
(defpackage :ordered-map
  (:nicknames :omap)
  (:use :cl)
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
(in-package :ordered-map)

;;; node

(defun make-node (&key color left value right)
  (list color left value right))

(defun make-empty-node () nil)

(defun node-empty-p (node) (null node))

(defun node-color (node)
  (if (node-empty-p node)
      'black
      (first node)))

(defun node-left (node) (second node))

(defun node-value (node) (third node))

(defun node-right (node) (fourth node))

(defun (setf node-color) (color node)
  (if (node-empty-p node)
      (if (eq color 'black)
          'black
          (error "Leaf is must be black."))
      (setf (first node) color)))

(defun (setf node-value) (value node)
  (setf (third node) value))

(defun (setf node-left) (left node)
  (setf (second node) left))

(defun (setf node-right) (right node)
  (setf (fourth node) right))

(defun node-black-p (node) (eq (node-color node) 'black))

(defun node-red-p (node) (eq (node-color node) 'red))

(defun node-each (node fn)
  (unless (node-empty-p node)
    (node-each (node-left node) fn)
    (funcall fn (node-value node))
    (node-each (node-right node) fn)))

(defun node-search (node search-key key key-eq-p key-less-p &key default)
  (if (node-empty-p node)
      default
      (let ((node-key (funcall key (node-value node))))
        (cond ((funcall key-eq-p search-key node-key)
               (values (node-value node) t))
              ((funcall key-less-p search-key node-key)
               (node-search (node-left node)
                            search-key
                            key
                            key-eq-p
                            key-less-p
                            :default default))
              (t (node-search (node-right node)
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

(defun rotate-right (node)
  (let ((lnode (node-left node)))
    (setf (node-left node) (node-right lnode)
          (node-right lnode) node
          (node-color lnode) (node-color node)
          (node-color node) 'red)
  lnode))

(defun rotate-left (node)
  (let ((rnode (node-right node)))
    (setf (node-right node) (node-left rnode)
          (node-left rnode) node
          (node-color rnode) (node-color node)
          (node-color node) 'red)
    rnode))

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

(defun node-insert (node value key key-eq-p key-less-p)
  (if (node-empty-p node)
      (values (make-node :color 'red :value value) t)
      (let ((new-key (funcall key value))
            (node-key (funcall key (node-value node))))
        (cond ((funcall key-eq-p new-key node-key)
               (setf (node-value node) value)
               (values node nil))
              ((funcall key-less-p new-key node-key)
               (node-insert-left node value key key-eq-p key-less-p))
              (t (node-insert-right node value key key-eq-p key-less-p))))))

(defun node-insert-left (node value key key-eq-p key-less-p)
  (multiple-value-bind (lnode needs-balance)
      (node-insert (node-left node) value key key-eq-p key-less-p)
    (setf (node-left node) lnode)
    (balance-insert-left node needs-balance)))

(defun balance-insert-left (node needs-balance)
  (if (or (not needs-balance) (node-red-p node))
      (values node needs-balance)
      (cond ((and (node-red-p (node-right node))
               (or (invalid-red-ll-p node)
                   (invalid-red-lr-p node)))
             (setf node (split-4node node))
             (values node t))
            ((invalid-red-ll-p node)
             (values (rotate-right node) nil))
            ((invalid-red-lr-p node)
             (setf (node-left node) (rotate-left (node-left node)))
             (values (rotate-right node) nil))
            (t (values node nil)))))

(defun invalid-red-ll-p (node)
  (let ((lnode (node-left node)))
    (and (node-red-p lnode)
         (node-red-p (node-left lnode)))))

(defun invalid-red-lr-p (node)
  (let ((lnode (node-left node)))
    (and (node-red-p lnode)
         (node-red-p (node-right lnode)))))

(defun node-insert-right (node value key key-eq-p key-less-p)
  (multiple-value-bind (rnode needs-balance)
      (node-insert (node-right node) value key key-eq-p key-less-p)
    (setf (node-right node) rnode)
    (balance-insert-right node needs-balance)))

(defun balance-insert-right (node needs-balance)
  (if (or (not needs-balance) (node-red-p node))
      (values node needs-balance)
      (cond ((and (node-red-p (node-left node))
                  (or (invalid-red-rr-p node)
                      (invalid-red-rl-p node)))
             (setf node (split-4node node))
             (values node t))
            ((invalid-red-rr-p node)
             (values (rotate-left node) nil))
            ((invalid-red-rl-p node)
             (setf (node-right node) (rotate-right (node-right node)))
             (values (rotate-left node) nil))
            (t (values node nil)))))

(defun invalid-red-rl-p (node)
  (let ((rnode (node-right node)))
    (and (node-red-p rnode)
         (node-red-p (node-left rnode)))))

(defun invalid-red-rr-p (node)
  (let ((rnode (node-right node)))
    (and (node-red-p rnode)
         (node-red-p (node-right rnode)))))

(defun node-remove (node remove-key key key-eq-p key-less-p)
  (if (node-empty-p node)
      (values node nil)
      (let ((node-key (funcall key (node-value node))))
        (cond ((funcall key-eq-p remove-key node-key)
               (node-remove-here node remove-key key key-eq-p key-less-p))
              ((funcall key-less-p remove-key node-key)
               (node-remove-left node remove-key key key-eq-p key-less-p))
              (t (node-remove-right node remove-key key key-eq-p key-less-p))))))

(defun node-remove-here (node remove-key key key-eq-p key-less-p)
  (declare (ignore remove-key))
  (let ((lnode (node-left node))
        (rnode (node-right node)))
    (cond ((and (node-empty-p lnode)
                (node-empty-p rnode))
           (values (make-empty-node) (node-black-p node)))
          ((node-empty-p lnode)
           (setf (node-color rnode) 'black)
           (values rnode nil))
          ((node-empty-p rnode)
           (setf (node-color lnode) 'black)
           (values lnode nil))
          (t (let ((first-node (node-first rnode)))
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
      (let* ((lnode (node-left node))
             (rnode (node-right node))
             (rlnode (node-left rnode))
             (rrnode (node-right rnode)))
        (cond ((node-empty-p rnode) (values node needs-balance))
              ((and (node-black-p node)
                    (node-black-p lnode)
                    (node-black-p rnode)
                    (node-black-p rlnode)
                    (node-black-p rrnode))
               (setf (node-color rnode) 'red)
               (values node t))
              ((and (node-black-p node)
                    (node-black-p lnode)
                    (node-red-p rnode))
               (let ((new-node (rotate-left node)))
                 (multiple-value-bind (new-left needs-balance)
                     (balance-remove-left (node-left new-node) t)
                   (setf (node-left new-node) new-left)
                   (balance-remove-left new-node needs-balance))))
              ((and (node-red-p node)
                    (node-black-p rlnode)
                    (node-black-p rrnode))
               (rotatef (node-color node) (node-color rnode))
               (values node nil))
              ((and (node-black-p rnode)
                    (node-red-p rlnode)
                    (node-black-p rrnode))
               (setf (node-right node) (rotate-right rnode))
               (balance-remove-left node t))
              (t (let ((new-node (rotate-left node)))
                   (setf (node-color (node-left new-node)) 'black
                         (node-color (node-right new-node)) 'black)
                   (values new-node nil)))))))

(defun node-remove-right (node remove-key key key-eq-p key-less-p)
  (multiple-value-bind (new-right needs-balance)
      (node-remove (node-right node) remove-key key key-eq-p key-less-p)
    (setf (node-right node) new-right)
    (balance-remove-right node needs-balance)))

(defun balance-remove-right (node needs-balance)
  (if (not needs-balance)
      (values node nil)
      (let* ((rnode (node-right node))
             (lnode (node-left node))
             (lrnode (node-right lnode))
             (llnode (node-left lnode)))
        (cond ((node-empty-p lnode) (values node needs-balance))
              ((and (node-black-p node)
                    (node-black-p rnode)
                    (node-black-p lnode)
                    (node-black-p lrnode)
                    (node-black-p llnode))
               (setf (node-color lnode) 'red)
               (values node t))
              ((and (node-black-p node)
                    (node-black-p rnode)
                    (node-red-p lnode))
               (let ((new-node (rotate-right node)))
                 (multiple-value-bind (new-right needs-balance)
                     (balance-remove-right (node-right new-node) t)
                   (setf (node-right new-node) new-right)
                   (balance-remove-right new-node needs-balance))))
              ((and (node-red-p node)
                    (node-black-p lrnode)
                    (node-black-p llnode))
               (rotatef (node-color node) (node-color lnode))
               (values node nil))
              ((and (node-black-p lnode)
                    (node-red-p lrnode)
                    (node-black-p llnode))
               (setf (node-left node) (rotate-left lnode))
               (balance-remove-right node t))
              (t (let ((new-node (rotate-right node)))
                   (setf (node-color (node-left new-node)) 'black
                         (node-color (node-right new-node)) 'black)
                   (values new-node nil)))))))

(defun node-print (node depth &key (stream t) show-nil)
  (cond ((node-empty-p node)
         (when show-nil
             (loop repeat depth do (format stream "    "))
             (format stream "NIL~%")))
        (t (node-print (node-right node) (1+ depth) :stream stream :show-nil show-nil)
           (loop repeat depth do (format stream "    "))
           (format stream "(~A,~S)~%" (symbol-name (node-color node)) (node-value node))
           (node-print (node-left node) (1+ depth) :stream stream :show-nil show-nil))))

;;; rbtree

(defun make-rbtree (&key root (key #'identity) (key-eq-p #'eql) (key-less-p #'<))
  (list (or root (make-empty-node)) key key-eq-p key-less-p))

(defun rbtree-root (rbtree) (first rbtree))

(defun rbtree-key (rbtree) (second rbtree))

(defun rbtree-key-eq-p (rbtree) (third rbtree))

(defun rbtree-key-less-p (rbtree) (fourth rbtree))

(defun (setf rbtree-root) (root rbtree)
  (setf (first rbtree) root))

(defun rbtree-each (rbtree fn)
  (node-each (rbtree-root rbtree) fn))

(defun rbtree-to-list (rbtree)
  (let ((acc nil))
    (rbtree-each rbtree
                 (lambda (v)
                   (push v acc)))
    (nreverse acc)))

(defun rbtree-first (rbtree)
  (let ((node (node-first (rbtree-root rbtree))))
    (if (node-empty-p node)
        nil
        (node-value node))))

(defun rbtree-last (rbtree)
  (let ((node (node-last (rbtree-root rbtree))))
    (if (node-empty-p node)
        nil
        (node-value node))))

(defun rbtree-search (rbtree search-key &key default)
  (node-search (rbtree-root rbtree)
               search-key
               (rbtree-key rbtree)
               (rbtree-key-eq-p rbtree)
               (rbtree-key-less-p rbtree)
               :default default))

(defun rbtree-lower-bound (rbtree search-key &key end)
  (node-lower-bound (rbtree-root rbtree)
                    search-key
                    (rbtree-key rbtree)
                    (rbtree-key-eq-p rbtree)
                    (rbtree-key-less-p rbtree)
                    :end end))

(defun rbtree-upper-bound (rbtree search-key &key end)
  (node-upper-bound (rbtree-root rbtree)
                    search-key
                    (rbtree-key rbtree)
                    (rbtree-key-eq-p rbtree)
                    (rbtree-key-less-p rbtree)
                    :end end))

(defun rbtree-insert (rbtree value)
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
  (let ((root (node-remove (rbtree-root rbtree)
                           remove-key
                           (rbtree-key rbtree)
                           (rbtree-key-eq-p rbtree)
                           (rbtree-key-less-p rbtree))))
    (setf (rbtree-root rbtree) root)
    rbtree))

(defun rbtree-print (rbtree &key (stream t) show-nil)
  (node-print (rbtree-root rbtree) 0 :stream stream :show-nil show-nil))

(defun test ()
  (let ((rbtree (make-rbtree))
        (randoms (loop repeat 10 collect (random 100))))
    (dolist (x randoms)
      (format t "~%----------~%~%")
      (format t "insert ~S~%" x)
      (setf rbtree (rbtree-insert rbtree x))
      (terpri)
      (rbtree-print rbtree))
    (labels ((rec ()
               (format *terminal-io* "> ")
               (let ((op (intern (symbol-name (read)) :ordered-map))
                     (x (read)))
                 (cond ((eq op 'quit) 'quit)
                       ((eq op 'print) (rbtree-print rbtree :stream *terminal-io*) (rec))
                       ((eq op 'to-list)
                        (format *terminal-io* "To list: ~S~%" (rbtree-to-list rbtree))
                        (rec))
                       ((eq op 'search)
                        (format *terminal-io* "Search result: ~S~%" (rbtree-search rbtree x))
                        (rec))
                       ((eq op 'lower-bound)
                        (format *terminal-io* "Lower bound: ~S~%" (rbtree-lower-bound rbtree x :end 'end))
                        (rec))
                       ((eq op 'upper-bound)
                        (format *terminal-io* "Upper bound: ~S~%" (rbtree-upper-bound rbtree x :end 'end))
                        (rec))
                       ((eq op 'insert)
                        (setf rbtree (rbtree-insert rbtree x))
                        (rbtree-print rbtree :stream *terminal-io*)
                        (rec))
                       ((eq op 'remove)
                        (setf rbtree (rbtree-remove rbtree x))
                        (rbtree-print rbtree :stream *terminal-io*)
                        (rec))
                       (t (format t "Unrecognized operator: ~S~%" op)
                          (rec))))))
      (rec))))
