(use gauche.collection)
(use gauche.uvector)
(use util.match)
(use binary.io)
(use srfi-1)
(use srfi-13)
(use srfi-42)

(define-constant nil '())
(define-syntax nil! (syntax-rules () ((_ x) (set! x '()))))
(define-inline (identity x) x)
(define-inline (inc n) (+ n 1))
(define-inline (dec n) (- n 1))
(define-syntax inc!
  (syntax-rules ()
    ((_ n) (set! n (+ n 1)))
    ((_ n x) (set! n (+ n x)))))
(define-syntax dec!
  (syntax-rules ()
    ((_ n) (set! n (- n 1)))
    ((_ n x) (set! n (- n x)))))
(define-inline (atom? x) (not (pair? x)))

(define-inline (half n) (quotient n 2))
(define-inline (ceil-quotient a b) (quotient (dec (+ a b)) b))
(define-inline (diff a b) (abs (- a b)))

(define-inline (display-error x) (display x (current-error-port)))
(define-inline (newline-error) (newline (current-error-port)))

(define-syntax debug-print
  (syntax-rules ()
    ((_ f args ...)
      (let ((ret (f args ...)))
        (display-error #\()
        (display-error 'f)
        (begin
          (display-error #\space)
          (display-error args))
        ...
        (display-error ") => ")
        (display-error ret)
        (newline-error)
        ret))))

(define (skip-empty-line)
  (let loop ((c (peek-char)))
    (when (char=? c #\lf)
        (read-char)
        (loop (peek-char)))))
(define (read-no-empty-line)
  (let loop ((line (read-line)))
    (if (string-null? line)
        (loop (read-line))
        line)))

(define-inline (write-space) (write-u8 #x20))
(define-inline (write-lf) (write-u8 #x0a))

(define-inline (sum ls) (fold + 0 ls))

(define (flatten ls)
  (cond ((null? ls) nil)
        ((atom? (car ls)) ls)
        (else (append (flatten (car ls)) (flatten (cdr ls))))))

(define (flatten-1 ls)
  (cond ((null? ls) nil)
        ((atom? ls) ls)
        (else (append (car ls) (flatten (cdr ls))))))

(define (maplist proc . ls)
  (maplist-1 proc (car ls)))
(define (maplist-1 proc ls)
  (if (null? ls)
      nil
      (cons (proc ls) (maplist-1 proc (cdr ls)))))

(define (meguru-method ok ng pred)
  (let1 mid (quotient (+ ok ng) 2)
    (cond ((< (diff ok ng) 2) ok)
          ((pred mid) (meguru-method mid ng pred))
          (else (meguru-method ok mid pred)))))

(define (char-min a b) (if (char<? a b) a b))
(define (char-max a b) (if (char>? a b) a b))

(define (next-power-of-2 n)
  (let loop ((x 1))
    (if (>= x n)
        x
        (loop (ash x 1)))))

(define (make-monoid op e)
  (define (self msg . args)
    (cond ((eq? msg 'id) e)
          ((eq? msg 'op) op)
          ((eq? msg 'apply) (apply op args))
          (else (error "Unknown method: MONOID" op))))
  self)

(define-constant bintree-null
  (lambda (op)
    (cond ((eq? op 'null?) #t)
          (else (error "Unknown method: BINTREE-NULL" op)))))
(define (make-bintree-node left value right)
  (define (leaf? node)
    (and ((node 'left) 'null?)
         ((node 'right) 'null?)))
  (define (rotate-right node)
    (let* ((l (node 'left))
           (ll (l 'left))
           (lr (l 'right))
           (r (node 'right)))
      (make-bintree-node ll (l 'value) (make-bintree-node lr (node 'value) r))))
  (define (rotate-left node)
    (let* ((l (node 'left))
           (r (node 'right))
           (rl (r 'left))
           (rr (r 'right)))
      (make-bintree-node (make-bintree-node l (node 'value) rl) (l 'value) rr)))
  (define (show node level)
    (if (node 'leaf?)
        (begin (dotimes (i level)
                 (display-error #\tab))
               (display-error (node 'value))
               (newline-error))
        (begin (show (node 'left) (inc level))
               (dotimes (i level)
                 (display-error #\tab))
               (display-error (node 'value))
               (newline-error)
               (show (node 'right) (inc level)))))
  (define (self op . args)
    (cond ((eq? op 'left) left)
          ((eq? op 'value) value)
          ((eq? op 'right) right)
          ((eq? op 'null?) #f)
          ((eq? op 'leaf?) (leaf? self))
          ((eq? op 'rotate-right) (rotate-right self))
          ((eq? op 'rotate-left) (rotate-left self))
          ((eq? op 'show) (show self 0) (newline-error))
          (else (error "Unknown method: BINTREE" op))))
  self)

(define (make-segment-tree size monoid)
  (define make-node make-bintree-node)
  (define (make-tree size)
    (if (zero? size)
        bintree-null
        (let1 center (half size)
          (make-node (make-tree center) (monoid 'id) (make-tree center)))))
  (define (get tree size index)
    (if (= size 1)
        (tree 'value)
        (let1 center (half size)
          (if (<  index center)
              (get (tree 'left) center index)
              (get (tree 'right) center (- index center))))))
  (define (query tree size monoid lindex rindex)
    (if (= size (- rindex lindex))
        (tree 'value)
        (let1 center (half size)
          (cond ((<= rindex center)
                  (query (tree 'left) center monoid lindex rindex))
                ((>= lindex center)
                  (query (tree 'right) center monoid (- lindex center) (- rindex center)))
                (else
                  (monoid 'apply (query (tree 'left) center monoid lindex center)
                                 (query (tree 'right) center monoid 0 (- rindex center))))))))
  (define (update tree size monoid index value)
    (if (= size 1)
        (make-node bintree-null value bintree-null)
        (let1 center (half size)
          (if (< index center)
              (let ((left (update (tree 'left) center monoid index value))
                    (right (tree 'right)))
                (make-node left
                           (monoid 'apply (left 'value) (right 'value))
                           right))
              (let ((left (tree 'left))
                    (right (update (tree 'right) center monoid (- index center) value)))
                (make-node left
                           (monoid 'apply (left 'value) (right 'value))
                           right))))))
  (define (make-object tree size monoid)
    (define (self op . args)
      (cond ((eq? op 'tree) tree)
            ((eq? op 'size) size)
            ((eq? op 'monoid) monoid)
            ((eq? op 'get)
              (let1 index (car args)
                (get tree size index)))
            ((eq? op 'query)
              (let ((lindex (car args))
                    (rindex (cadr args)))
                (query tree size monoid lindex rindex)))
            ((eq? op 'update)
              (let ((index (car args))
                    (value (cadr args)))
                (make-object (update tree size monoid index value) size monoid)))
            ((eq? op 'show) (tree 'show))
            (else (error "Unknown method: SEGMENT-TREE" op))))
    self)
  (let1 size (next-power-of-2 sizea)
    (make-object (make-tree size) size monoid)))

(define (main args)
  (let* ((n (read))
         (a (vector-of-length-ec n (:range i n) (read)))
         (q (read))
         (b (list-ec (:range i q) (read))))
    (let1 result (solve n a q b)
      (for-each print result)))
  0)

(define (solve n a q b)
  (update! a sort!)
  (map (lambda (b) (query n a b)) b))

(define (lower-bound vec value)
  (meguru-method (vector-length vec) 0
    (lambda (i) (>= (vector-ref vec i) value))))

(define (upper-bound vec value)
  (meguru-method (vector-length vec) 0
    (lambda (i) (> (vector-ref vec i) value))))

(define (less-bound vec value)
  (meguru-method -1 (dec (vector-length vec))
    (lambda (i) (if (= i -1) #t (< (vector-ref vec i) value)))))

(define (query n a b)
  (let ((min-a (vector-ref a 0))
        (max-a (vector-ref a (dec (vector-length a)))))
    (cond ((<= b min-a) (- min-a b))
          ((>= b max-a) (- b max-a))
          (else (let ((le-value (vector-ref a (less-bound a b)))
                      (beq-value (vector-ref a (lower-bound a b))))
                  (min (- b le-value) (- beq-value b)))))))

