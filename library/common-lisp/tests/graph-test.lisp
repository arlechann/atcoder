(in-package :cl-user)

(defpackage :graph-test
  (:use :cl :rove))
(in-package :graph-test)

(defun edge->triple (e)
  (list (graph:edge-from e) (graph:edge-to e) (graph:edge-cost e)))

(defun edges->triples (edges)
  (sort (mapcar #'edge->triple edges)
        (lambda (a b)
          (or (< (second a) (second b))
              (and (= (second a) (second b))
                   (< (third a) (third b)))))))

(deftest graph-adlist-single-edge-basic
  (let ((g (graph:make-adlist-single-edge-graph 4)))
    (setf (graph:graph-node-ref g 1) :n1)
    (ok (eq :n1 (graph:graph-node-ref g 1)))

    (graph:graph-add-edge g 0 1 :cost 5)
    (graph:graph-add-edge g 0 1 :cost 3) ; single-edge keeps smaller cost
    (let ((e (graph:graph-edge-ref g 0 1)))
      (ok (= 0 (graph:edge-from e)))
      (ok (= 1 (graph:edge-to e)))
      (ok (= 3 (graph:edge-cost e))))

    (ok (equal '((0 1 3))
               (mapcar #'edge->triple (graph:graph-multi-edges-ref g 0 1))))
    (ok (equal '((0 1 3))
               (edges->triples (graph:graph-neighbors g 0))))

    (graph:graph-delete-edges g 0 1)
    (ok (null (graph:graph-edge-ref g 0 1)))
    (ok (null (graph:graph-neighbors g 0)))
    (ok (handler-case
            (progn
              (setf (graph:graph-multi-edges-ref g 0 1)
                    (list (graph:make-edge 0 1 :cost 1)
                          (graph:make-edge 0 1 :cost 2)))
              nil)
          (error () t)))))

(deftest graph-adlist-multigraph-basic
  (let ((g (graph:make-adlist-multigraph 3)))
    (graph:graph-add-edge g 0 1 :cost 10)
    (graph:graph-add-edge g 0 1 :cost 20)
    (graph:graph-add-edge g 0 2 :cost 30)
    (let ((m01 (graph:graph-multi-edges-ref g 0 1)))
      (ok (= 2 (length m01)))
      (ok (equal '((0 1 10) (0 1 20))
                 (sort (mapcar #'edge->triple m01)
                       (lambda (a b) (< (third a) (third b)))))))
    ;; graph-edge-ref on multigraph returns one arbitrary edge (current impl: latest).
    (let ((e (graph:graph-edge-ref g 0 1)))
      (ok (= 0 (graph:edge-from e)))
      (ok (= 1 (graph:edge-to e))))
    (push (graph:make-edge 0 1 :cost 5) (graph:graph-multi-edges-ref g 0 1))
    (ok (= 3 (length (graph:graph-multi-edges-ref g 0 1))))
    (graph:graph-delete-edges g 0 1)
    (ok (null (graph:graph-multi-edges-ref g 0 1)))))

(deftest graph-matrix-basic-and-shortest-path
  (let ((g (graph:make-matrix-graph 3)))
    ;; diagonal exists with cost 0 by default
    (ok (= 0 (graph:edge-cost (graph:graph-edge-ref g 0 0))))

    (graph:graph-add-edge g 0 1 :cost 2)
    (graph:graph-add-edge g 1 2 :cost 3)
    (graph:graph-add-edge g 0 2 :cost 10)

    (ok (= 2 (graph:edge-cost (graph:graph-edge-ref g 0 1))))
    (ok (= 3 (graph:edge-cost (graph:graph-edge-ref g 1 2))))

    (let ((dist (graph:dijkstra g :start 0)))
      (ok (= 0 (aref dist 0)))
      (ok (= 2 (aref dist 1)))
      (ok (= 5 (aref dist 2))))

    (let ((fw (graph:floyd-warshall g)))
      (ok (= 0 (aref fw 0 0)))
      (ok (= 2 (aref fw 0 1)))
      (ok (= 5 (aref fw 0 2))))

    (graph:graph-delete-edges g 0 1)
    (ok (null (graph:graph-edge-ref g 0 1)))))

(deftest graph-bfs-and-find-leaf
  (let ((g (graph:make-adlist-single-edge-graph 5)))
    (graph:graph-add-edge g 0 1 :cost 1)
    (graph:graph-add-edge g 1 2 :cost 1)
    (graph:graph-add-edge g 2 3 :cost 1)
    (let ((dist (graph:bfs g :start 0)))
      (ok (= 0 (aref dist 0)))
      (ok (= 1 (aref dist 1)))
      (ok (= 2 (aref dist 2)))
      (ok (= 3 (aref dist 3)))
      (ok (= graph:*graph-cost-infinity* (aref dist 4))))
    (ok (integerp (graph:find-leaf g)))))

(deftest graph-grid-basic
  (let* ((lines (list "..#"
                      ".#."
                      "..."))
         (g (graph:make-grid-graph 3 3 lines)))
    (ok (= 9 (graph:graph-size g)))
    ;; nodes=nil on default grid graph
    (ok (handler-case (progn (graph:graph-node-ref g 0) nil)
          (error () t)))
    ;; from (0,0): right(0,1) and down(1,0)
    (let ((neighbors (graph:graph-neighbors g 0)))
      (ok (equal '(1 3)
                 (sort (mapcar #'graph:edge-to neighbors) #'<))))
    (ok (handler-case
            (progn (setf (graph:graph-edge-ref g 0 1)
                         (graph:make-edge 0 1 :cost 1))
                   nil)
          (error () t)))
    (let* ((nodes (make-array 9 :initial-element nil))
           (g2 (graph:make-grid-graph 3 3 lines :nodes nodes)))
      (setf (graph:graph-node-ref g2 4) :center)
      (ok (eq :center (graph:graph-node-ref g2 4))))))
