(define nil '())
(define (id x) x)
(define (inc n) (+ n 1))
(define (dec n) (- n 1))
(define (ceil-quotient a b) (quotient (dec (+ a b)) b))
(define (diff a b) (abs (- a b)))

(define (read-times n)
  (let rec ((i 0) (ret nil))
    (if (= i n)
        (reverse! ret)
        (rec (inc i) (cons (read) ret)))))

(define (sum ls) (reduce + 0 ls))

(define (maplist-1 proc ls)
  (if (null? ls)
      nil
      (cons (proc ls) (maplist-1 proc (cdr ls)))))

;; TODO
(define (maplist proc ls)
  (maplist-1 proc ls))

(define (meguru-method ok ng pred)
  (let ((mid (quotient (+ ok ng) 2)))
    (cond ((< (diff ok ng) 2) ok)
          ((pred mid) (meguru-method mid ng pred))
          (else (meguru-method ok mid pred)))))

(define (main args)
  (let* ((n (read))
         (ab (map (lambda (_) (cons (dec (read)) (dec (read)))) (iota (dec n)))))
    (let ((result (solve n ab)))
      (display result)
      (newline)))
  0)

(define (solve n ab)
  (let ((graph (build-graph n ab)))
    (inc (cdr (farthest-node-and-distance graph (car (farthest-node-and-distance graph 0 -1 0)) -1 0)))))

(define (graph-size graph) (car graph))
(define (graph-adlist graph) (cdr graph))

(define (build-graph size edges)
  (let ((adlist (make-hash-table)))
    (let rec ((edges edges))
      (if (null? edges)
          (cons size adlist)
          (let ((edge (car edges)))
            (adlist-add-edge! adlist (car edge) (cdr edge))
            (rec (cdr edges)))))))

(define (graph-add-edge! graph from to) (adlist-add-edge! (graph-adlist graph) from to))
(define (adlist-add-edge! adlist from to)
  (hash-table-push! adlist from to)
  (hash-table-push! adlist to from))

(define (graph-neighbors graph node)
  (hash-table-get (graph-adlist graph) node nil))

(define (farthest-node-and-distance graph node prev dist)
  (let ((neighbors (graph-neighbors graph node)))
    (fold (lambda (x acc)
            (if (> (cdr x) (cdr acc))
                x
                acc))
          (cons node dist)
          (map (lambda (next) (farthest-node-and-distance graph next node (inc dist)))
               (remove (lambda (next) (= next prev)) neighbors)))))

