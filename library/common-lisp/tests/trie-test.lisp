(in-package :cl-user)

(defpackage :trie-test
  (:use :cl :rove))
(in-package :trie-test)

(deftest trie-insert-find-and-size
  (let ((tr (trie:make-trie)))
    (ok (= 0 (trie:trie-size tr)))

    (ok (equal 10 (trie:trie-insert tr "abc" 10)))
    (ok (= 1 (trie:trie-size tr)))
    (ok (equal 20 (trie:trie-insert tr "abd" 20)))
    (ok (= 2 (trie:trie-size tr)))

    ;; same key update must not increase unique-key size
    (ok (equal 99 (trie:trie-insert tr "abc" 99)))
    (ok (= 2 (trie:trie-size tr)))

    (multiple-value-bind (found value) (trie:trie-find tr "abc")
      (ok found)
      (ok (= 99 value)))
    (multiple-value-bind (found value) (trie:trie-find tr "abd")
      (ok found)
      (ok (= 20 value)))
    (ok (null (trie:trie-find tr "abe")))))

(deftest trie-prefix-find
  (let ((tr (trie:make-trie)))
    (trie:trie-insert tr "abc" :v-abc)
    (trie:trie-insert tr "ab" :v-ab)
    (multiple-value-bind (found value) (trie:trie-find tr "ab")
      (ok found)
      (ok (eq :v-ab value)))
    (multiple-value-bind (found value) (trie:trie-find tr "a" :prefixp t)
      (ok found)
      ;; prefix node itself is not an end node, so value is usually NIL.
      (ok (null value)))
    (ok (null (trie:trie-find tr "x" :prefixp t)))))

(deftest trie-traverse-and-node-attributes
  (let ((tr (trie:make-trie))
        (seen nil))
    (trie:trie-insert tr "ab" 1)
    (trie:trie-insert tr "abc" 2)
    (trie:trie-insert tr "abd" 3)
    (trie:trie-traverse
     tr "abc"
     (lambda (node str index)
       (declare (ignore str))
       (push (list index
                   (trie:trie-node-char node)
                   (trie:trie-node-endp node)
                   (trie:trie-node-end-count node)
                   (trie:trie-node-prefix-count node)
                   (trie:trie-node-value node))
             seen)))
    (setf seen (nreverse seen))
    ;; root / a / b / c should be visited
    (ok (= 4 (length seen)))
    ;; index=2 node corresponds to "ab" and is an end node
    (destructuring-bind (_idx _char endp end-count prefix-count value) (nth 2 seen)
      (declare (ignore _idx _char))
      (ok endp)
      (ok (= 1 end-count))
      (ok (= 3 prefix-count))
      (ok (= 1 value)))
    ;; last node corresponds to "abc"
    (destructuring-bind (_idx _char endp end-count _prefix-count value) (nth 3 seen)
      (declare (ignore _idx _char _prefix-count))
      (ok endp)
      (ok (= 1 end-count))
      (ok (= 2 value)))))
