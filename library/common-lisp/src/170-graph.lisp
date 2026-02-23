;;; graph
;;;
(defpackage graph
  (:use :cl :utility :deque :binary-heap)
  (:export :<graph>
           :<single-edge-graph>
           :<multigraph>
           :<edge>
           :graph-size
           :graph-node-ref
           :graph-edge-ref
           :graph-multi-edges-ref
           :graph-neighbors
           :call-with-graph-neighbors
           :graph-add-edge
           :graph-delete-edges
           :do-graph-neighbors
           :edge-from
           :edge-to
           :edge-cost
           :make-edge
           :<adlist-graph>
           :<adlist-multigraph>
           :<adlist-single-edge-graph>
           :make-adlist-single-edge-graph
           :make-adlist-multigraph
           :graph-adlist
           :<matrix-graph>
           :make-matrix-graph
           :graph-matrix
           :<grid-graph>
           :make-grid-graph
           :grid-height
           :grid-width
           :graph-grid
           :*graph-cost-infinity*
           :bfs
           :dijkstra
           :floyd-warshall
           :find-leaf))
(in-package graph)

(defparameter *graph-cost-infinity* (ash most-positive-fixnum -1))

(defgeneric graph-size (graph))
(defgeneric graph-node-ref (graph node))
(defgeneric (setf graph-node-ref) (value graph node))
(defgeneric graph-edge-ref (graph from to)
  (:documentation
   "Returns one edge from FROM to TO.
For multigraph, an arbitrary edge is returned (current implementations usually return the latest one)."))
(defgeneric (setf graph-edge-ref) (edge graph from to))
(defgeneric graph-multi-edges-ref (graph from to)
  (:documentation
   "Returns all edges from FROM to TO as a list.
For single-edge graphs, the list length is at most 1."))
(defgeneric (setf graph-multi-edges-ref) (edges graph from to)
  (:documentation
   "Replaces the whole edge collection from FROM to TO with EDGES.
For single-edge graphs, EDGES must be empty or singleton."))
(defgeneric graph-neighbors (graph node)
  (:documentation "Returns <edge> list."))
(defgeneric call-with-graph-neighbors (graph node fn)
  (:documentation "Calls fn by <edge>."))
(defgeneric graph-add-edge (graph from to &key cost))
(defgeneric graph-delete-edges (graph from to))
(defgeneric graph-low-level-edge-ref (graph from to))
(defgeneric (setf graph-low-level-edge-ref) (edge graph from to))
(defgeneric graph-low-level-multi-edges-ref (graph from to))
(defgeneric (setf graph-low-level-multi-edges-ref) (edges graph from to))

(defgeneric edge-from (edge))
(defgeneric edge-to (edge))
(defgeneric edge-cost (edge))

(defmacro do-graph-neighbors ((var graph node &optional result) &body body)
  `(progn (call-with-graph-neighbors ,graph ,node (lambda (,var) ,@body))
          ,result))

(defclass <graph> () ())

(defclass <edge> ()
  ((from :initarg :from :accessor edge-from)
   (to :initarg :to :accessor edge-to)
   (cost :initarg :cost :initform 1 :accessor edge-cost)))

(defun make-edge (from to &key (cost 1)) (make-instance '<edge> :from from :to to :cost cost))

(defun graph-node-index-valid-p (graph node)
  (and (integerp node)
       (<= 0 node)
       (< node (graph-size graph))))

(defun assert-graph-node-index (graph node who)
  #-atcoder
  (unless (graph-node-index-valid-p graph node)
    (error "~A: node index out of range: ~S (size=~S)." who node (graph-size graph))))

(defun assert-graph-edge-index (graph from to who)
  (assert-graph-node-index graph from who)
  (assert-graph-node-index graph to who))

(defclass <single-edge-graph> (<graph>) ())

(defmethod graph-edge-ref ((graph <single-edge-graph>) from to)
  (assert-graph-edge-index graph from to "GRAPH-EDGE-REF")
  (ensure-car (graph-low-level-edge-ref graph from to)))

(defmethod (setf graph-edge-ref) (edge (graph <single-edge-graph>) from to)
  (assert-graph-edge-index graph from to "(SETF GRAPH-EDGE-REF)")
  (setf (graph-low-level-edge-ref graph from to) edge)
  edge)

(defmethod graph-multi-edges-ref ((graph <single-edge-graph>) from to)
  (assert-graph-edge-index graph from to "GRAPH-MULTI-EDGES-REF")
  (let ((edge (graph-low-level-edge-ref graph from to)))
    (ensure-list edge)))

(defmethod (setf graph-multi-edges-ref) (edges (graph <single-edge-graph>) from to)
  (assert-graph-edge-index graph from to "(SETF GRAPH-MULTI-EDGES-REF)")
  #-atcoder
  (when (> (length edges) 1)
    (error "(SETF GRAPH-MULTI-EDGES-REF): single-edge-graph accepts at most one edge."))
  (setf (graph-low-level-edge-ref graph from to) (car edges))
  edges)

(defmethod graph-add-edge ((graph <single-edge-graph>) from to &key (cost 1))
  (assert-graph-edge-index graph from to "GRAPH-ADD-EDGE")
  (let ((current-edge (graph-edge-ref graph from to)))
    (when (or (null current-edge)
              (< cost (edge-cost current-edge)))
      (setf (graph-edge-ref graph from to) (make-edge from to :cost cost)))))

(defclass <multigraph> (<graph>) ())

(defmethod graph-edge-ref ((graph <multigraph>) from to)
  (assert-graph-edge-index graph from to "GRAPH-EDGE-REF")
  (car (graph-low-level-multi-edges-ref graph from to)))

(defmethod (setf graph-edge-ref) (edge (graph <multigraph>) from to)
  (assert-graph-edge-index graph from to "(SETF GRAPH-EDGE-REF)")
  (setf (graph-low-level-multi-edges-ref graph from to)
        (ensure-list edge))
  edge)

(defmethod graph-multi-edges-ref ((graph <multigraph>) from to)
  (assert-graph-edge-index graph from to "GRAPH-MULTI-EDGES-REF")
  (graph-low-level-multi-edges-ref graph from to))

(defmethod (setf graph-multi-edges-ref) (edges (graph <multigraph>) from to)
  (assert-graph-edge-index graph from to "(SETF GRAPH-MULTI-EDGES-REF)")
  (setf (graph-low-level-multi-edges-ref graph from to) edges)
  edges)

(defmethod graph-add-edge ((graph <multigraph>) from to &key (cost 1))
  (assert-graph-edge-index graph from to "GRAPH-ADD-EDGE")
  (push (make-edge from to :cost cost)
        (graph-multi-edges-ref graph from to)))

(defclass <adlist-graph> (<graph>)
  ((size :initarg :size :accessor graph-size)
   (adlist :initarg :adlist :accessor graph-adlist)
   (nodes :initarg :nodes :accessor graph-nodes)))

(defclass <adlist-multigraph> (<adlist-graph> <multigraph>) ())

(defclass <adlist-single-edge-graph> (<adlist-graph> <single-edge-graph>) ())

(defun make-adlist-single-edge-graph (size)
  "Adjacency-list graph. Multiple edges between the same (from,to) are prohibited."
  (make-instance '<adlist-single-edge-graph>
                 :size size
                 :adlist (make-array size :initial-element nil)
                 :nodes (make-array size :initial-element nil)))

(defun make-adlist-multigraph (size)
  "Adjacency-list graph. Multiple edges between the same (from,to) are allowed."
  (make-instance '<adlist-multigraph>
                 :size size
                 :adlist (make-array size :initial-element nil)
                 :nodes (make-array size :initial-element nil)))

(defmethod graph-node-ref ((graph <adlist-graph>) node) (aref (graph-nodes graph) node))

(defmethod (setf graph-node-ref) (value (graph <adlist-graph>) node)
  (assert-graph-node-index graph node "GRAPH-NODE-REF")
  (setf (aref (graph-nodes graph) node) value))

(defmethod graph-low-level-edge-ref ((graph <adlist-graph>) from to)
  (assert-graph-edge-index graph from to "GRAPH-LOW-LEVEL-EDGE-REF")
  (find-if #'(lambda (edge) (= (edge-to edge) to))
           (aref (graph-adlist graph) from)))

(defmethod (setf graph-low-level-edge-ref) (edge (graph <adlist-graph>) from to)
  (assert-graph-edge-index graph from to "(SETF GRAPH-LOW-LEVEL-EDGE-REF)")
  (setf (graph-low-level-multi-edges-ref graph from to)
        (ensure-list edge))
  edge)

(defmethod graph-low-level-multi-edges-ref ((graph <adlist-graph>) from to)
  (assert-graph-edge-index graph from to "GRAPH-LOW-LEVEL-MULTI-EDGES-REF")
  (remove-if #'(lambda (edge) (/= (edge-to edge) to))
             (aref (graph-adlist graph) from)))

(defmethod (setf graph-low-level-multi-edges-ref) (edges (graph <adlist-graph>) from to)
  (assert-graph-edge-index graph from to "(SETF GRAPH-LOW-LEVEL-MULTI-EDGES-REF)")
  #-atcoder
  (unless (every (lambda (edge)
                   (and (= from (edge-from edge))
                        (= to (edge-to edge))))
                 edges)
    (error "(SETF GRAPH-LOW-LEVEL-MULTI-EDGES-REF): all edges must satisfy FROM=~S, TO=~S."
           from to))
  (graph-delete-edges graph from to)
  (dolist (edge (nreverse edges))
    (push edge (aref (graph-adlist graph) from)))
  edges)

(defmethod graph-neighbors ((graph <adlist-graph>) node)
  (assert-graph-node-index graph node "GRAPH-NEIGHBORS")
  (aref (graph-adlist graph) node))

(defmethod call-with-graph-neighbors ((graph <adlist-graph>) node fn)
  (assert-graph-node-index graph node "CALL-WITH-GRAPH-NEIGHBORS")
  (dolist (edge (graph-neighbors graph node))
    (funcall fn edge)))

(defmethod graph-delete-edges ((graph <adlist-graph>) from to)
  (assert-graph-edge-index graph from to "GRAPH-DELETE-EDGES")
  (setf (aref (graph-adlist graph) from)
        (delete-if (lambda (edge) (and (= (edge-from edge) from)
                                       (= (edge-to edge) to)))
                   (aref (graph-adlist graph) from))))

(defclass <matrix-graph> (<single-edge-graph>)
  ((size :initarg :size :accessor graph-size)
   (matrix :initarg :matrix :accessor graph-matrix)
   (nodes :initarg :nodes :accessor graph-nodes)))

(defun make-matrix-graph (size)
  "Adjacency-matrix graph. Only one edge is stored per (from,to)."
  (make-instance '<matrix-graph>
                 :size size
                 :matrix (let ((matrix (make-array (list size size) :initial-element nil)))
                           (dotimes (node size matrix)
                             (setf (aref matrix node node) (make-edge node node :cost 0))))
                 :nodes (make-array size :initial-element nil)))

(defmethod graph-node-ref ((graph <matrix-graph>) node) (aref (graph-nodes graph) node))

(defmethod (setf graph-node-ref) (value (graph <matrix-graph>) node)
  (assert-graph-node-index graph node "GRAPH-NODE-REF")
  (setf (aref (graph-nodes graph) node) value))

(defmethod graph-low-level-edge-ref ((graph <matrix-graph>) from to)
  (assert-graph-edge-index graph from to "GRAPH-LOW-LEVEL-EDGE-REF")
  (aref (graph-matrix graph) from to))

(defmethod (setf graph-low-level-edge-ref) (edge (graph <matrix-graph>) from to)
  (assert-graph-edge-index graph from to "(SETF GRAPH-LOW-LEVEL-EDGE-REF)")
  (setf (aref (graph-matrix graph) from to) edge))

(defmethod graph-low-level-multi-edges-ref ((graph <matrix-graph>) from to)
  (assert-graph-edge-index graph from to "GRAPH-LOW-LEVEL-MULTI-EDGES-REF")
  (ensure-list (graph-low-level-edge-ref graph from to)))

(defmethod (setf graph-low-level-multi-edges-ref) (edges (graph <matrix-graph>) from to)
  (assert-graph-edge-index graph from to "(SETF GRAPH-LOW-LEVEL-MULTI-EDGES-REF)")
  #-atcoder
  (unless (length<= edges 1)
    (error "(SETF GRAPH-LOW-LEVEL-MULTI-EDGES-REF): matrix-single-edge-graph accepts at most one edge."))
  #-atcoder
  (unless (every (lambda (edge)
                   (and (= from (edge-from edge))
                        (= to (edge-to edge))))
                 edges)
    (error "(SETF GRAPH-LOW-LEVEL-MULTI-EDGES-REF): all edges must satisfy FROM=~S, TO=~S."
           from to))
  (setf (aref (graph-matrix graph) from to) (car edges))
  edges)

(defmethod graph-neighbors ((graph <matrix-graph>) node)
  (assert-graph-node-index graph node "GRAPH-NEIGHBORS")
  (let ((neighbors nil))
    (do-graph-neighbors (edge graph node (nreverse neighbors))
      (push edge neighbors))))

(defmethod call-with-graph-neighbors ((graph <matrix-graph>) node fn)
  (assert-graph-node-index graph node "CALL-WITH-GRAPH-NEIGHBORS")
  (let ((matrix (graph-matrix graph)))
    (dotimes (to-node (graph-size graph))
      (when-let ((edge (aref matrix node to-node)))
        (funcall fn edge)))
    (values)))

(defmethod graph-delete-edges ((graph <matrix-graph>) from to)
  (assert-graph-edge-index graph from to "GRAPH-DELETE-EDGES")
  (setf (aref (graph-matrix graph) from to) nil))

(defclass <grid-graph> (<single-edge-graph>)
  ((height :initarg :height :accessor grid-height)
   (width :initarg :width :accessor grid-width)
   (grid :initarg :grid :accessor graph-grid)
   (nodes :initarg :nodes :accessor graph-nodes)))

(defun make-grid-graph (height width lines &key (wall #\#) (nodes nil))
  (make-instance '<grid-graph>
                 :height height
                 :width width
                 :grid (make-array (list height width)
                                   :initial-contents
                                   (map 'list
                                        (lambda (row)
                                          (map 'list
                                               (lambda (e)
                                                 (if (eql e wall) *graph-cost-infinity* 1))
                                               row))
                                        lines))
                 :nodes nodes))

(defun grid-row (graph node) (floor node (grid-width graph)))
(defun grid-column (graph node) (mod node (grid-width graph)))
(defun grid-point-to-index (graph row column) (+ (* (grid-width graph) row) column))
(defun grid-cost (graph row column) (aref (graph-grid graph) row column))
(defun grid-node-wall-p (graph node)
  (= (grid-cost graph (grid-row graph node) (grid-column graph node))
     *graph-cost-infinity*))
(defmethod graph-size ((graph <grid-graph>)) (* (grid-height graph) (grid-width graph)))
(defmethod graph-node-ref ((graph <grid-graph>) node)
  (assert-graph-node-index graph node "GRAPH-NODE-REF")
  (let ((nodes (graph-nodes graph)))
    #-atcoder
    (when (null nodes)
      (error "GRID-GRAPH nodes is NIL. Pass :NODES to MAKE-GRID-GRAPH before using GRAPH-NODE-REF."))
    (aref nodes node)))

(defmethod (setf graph-node-ref) (value (graph <grid-graph>) node)
  (assert-graph-node-index graph node "(SETF GRAPH-NODE-REF)")
  (let ((nodes (graph-nodes graph)))
    #-atcoder
    (when (null nodes)
      (error "GRID-GRAPH nodes is NIL. Pass :NODES to MAKE-GRID-GRAPH before using (SETF GRAPH-NODE-REF)."))
    (setf (aref nodes node) value)))

(defmethod graph-low-level-edge-ref ((graph <grid-graph>) from to)
  (assert-graph-edge-index graph from to "GRAPH-EDGE-REF")
  (let ((from-row (grid-row graph from))
        (from-col (grid-column graph from))
        (to-row (grid-row graph to))
        (to-col (grid-column graph to)))
    (cond ((or (grid-node-wall-p graph from)
               (grid-node-wall-p graph to))
           nil)
          ((and (= from-row to-row) (= from-col to-col)) (make-edge from to :cost 0))
          ((and (= 1 (+ (abs (- from-row to-row))
                        (abs (- from-col to-col))))
                (/= (grid-cost graph to-row to-col) *graph-cost-infinity*))
           (make-edge from to :cost (grid-cost graph to-row to-col)))
          (t nil))))

(defmethod (setf graph-edge-ref) (edge (graph <grid-graph>) from to) (error "Unsupported method."))

(defmethod graph-neighbors ((graph <grid-graph>) node)
  (assert-graph-node-index graph node "GRAPH-NEIGHBORS")
  (let ((neighbors nil))
    (call-with-graph-neighbors graph node (lambda (edge) (push edge neighbors)))
    (nreverse neighbors)))

(defmethod call-with-graph-neighbors ((graph <grid-graph>) node fn)
  (assert-graph-node-index graph node "CALL-WITH-GRAPH-NEIGHBORS")
  (when (grid-node-wall-p graph node)
    (return-from call-with-graph-neighbors nil))
  (do-4neighbors ((y x) ((grid-row graph node) (grid-column graph node)))
    (when (and (< -1 y (grid-height graph)) (< -1 x (grid-width graph))
               (/= (aref (graph-grid graph) y x) *graph-cost-infinity*))
      (funcall fn (make-edge node (grid-point-to-index graph y x)
                             :cost (grid-cost graph y x))))))

(defmethod graph-add-edge ((graph <grid-graph>) from to &key cost)
  (declare (ignore graph from to cost))
  (error "Unsupported method."))

(defmethod graph-delete-edges ((graph <grid-graph>) from to) (error "Unsupported method."))

(defun bfs (graph &key (start 0) end)
  (assert-graph-node-index graph start "BFS")
  (when end
    (assert-graph-node-index graph end "BFS"))
  (loop with size = (graph-size graph)
        with distances = (make-array size :initial-element *graph-cost-infinity*)
        and usedp = (make-array size :initial-element nil)
        and deque = (make-deque)
        initially (setf (aref distances start) 0
                        (aref usedp start) t)
                  (deque-push-back deque start)
        until (deque-empty-p deque)
        do (let ((node (deque-peek-front deque)))
             (deque-pop-front deque)
             (do-graph-neighbors (edge graph node)
               (let ((next-node (edge-to edge)))
                 (unless (aref usedp next-node)
                   (setf (aref distances next-node) (1+ (aref distances node))
                         (aref usedp next-node) t)
                   (when (and end (= next-node end))
                     (return-from bfs distances))
                   (deque-push-back deque next-node)))))
        finally (return distances)))

(defun floyd-warshall (graph)
  (let* ((size (graph-size graph))
         (distances (make-array (list size size) :initial-element *graph-cost-infinity*)))
    (dotimes (i size)
      (dotimes (j size)
        (when-let ((edge (graph-edge-ref graph i j)))
          (setf (aref distances i j) (edge-cost edge)))))
    (dotimes (k size)
      (dotimes (i size)
        (dotimes (j size)
          (minf (aref distances i j) (+ (aref distances i k) (aref distances k j))))))
    distances))

(defun dijkstra-next-distance (distance edge) (+ distance (edge-cost edge)))

(defun dijkstra (graph &key (start 0) end (next-distance #'dijkstra-next-distance))
  (assert-graph-node-index graph start "DIJKSTRA")
  (when end
    (assert-graph-node-index graph end "DIJKSTRA"))
  (labels ((make-search-node (from to distance) (vector from to distance))
           (search-node-to (search-node) (elt search-node 1))
           (search-node-distance (search-node) (elt search-node 2))
           (search-node-less (search-node1 search-node2)
             (< (search-node-distance search-node1) (search-node-distance search-node2))))
    (loop with size = (graph-size graph)
          with distances = (make-array size :initial-element *graph-cost-infinity*)
          and usedp = (make-array size :initial-element nil)
          and heap = (make-binary-heap #'search-node-less)
                initially (setf (aref distances start) 0)
                          (binary-heap-push (make-search-node start start 0) heap)
          until (binary-heap-empty-p heap)
          do (let* ((top (binary-heap-top heap))
                    (node (search-node-to top))
                    (distance (search-node-distance top)))
               (binary-heap-pop heap)
               (unless (aref usedp node)
                 (when (and end (= node end))
                   (return-from dijkstra distances))
                 (setf (aref usedp node) t)
                 (do-graph-neighbors (edge graph node)
                   (let ((next-node (edge-to edge))
                         (next-distance (funcall next-distance distance edge)))
                     (when (< next-distance (aref distances next-node))
                       (setf (aref distances next-node) next-distance)
                       (binary-heap-push (make-search-node node next-node next-distance)
                                         heap))))))
          finally (return distances))))

(defun find-leaf (graph)
  (dotimes (node (graph-size graph))
    (when (singlep (graph-neighbors graph node))
      (return-from find-leaf node))))

;;;
