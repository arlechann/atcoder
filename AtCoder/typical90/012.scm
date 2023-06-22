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
(define-syntax swap!
  (syntax-rules ()
    ((_ a b)
      (let ((tmp a))
        (set! a b)
        (set! b tmp)))))
(define-inline (atom? x) (not (pair? x)))

(define-inline (half n) (quotient n 2))
(define-inline (ceil-quotient a b) (quotient (dec (+ a b)) b))
(define-inline (diff a b) (abs (- a b)))

(define-inline (display-error x) (display x (current-error-port)))
(define-inline (newline-error) (newline (current-error-port)))
(define-inline (print-error . args)
  (dolist (arg args) (display-error arg))
  (newline-error))

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

(define-syntax set-min!
  (syntax-rules ()
    ((_ a b) (begin (if (< b a)
                        (begin (set! a b) #t)
                        #f)))))

(define-syntax set-max!
  (syntax-rules ()
    ((_ a b) (begin (if (> b a)
                        (begin (set! a b) #t)
                        #f)))))

(define-syntax define-memoize
  (syntax-rules ()
    ((_ name (arg ...) body)
      (define name 
        (let ((memo (make-hash-table equal-comparator)))
          (lambda (arg ...)
            (let* ((key (list arg ...))
                   (value (hash-table-get memo key '())))
              (if (not (null? value))
                  value
                  (let ((ret body))
                    (hash-table-set! memo key ret)
                    ret)))))))
    ((_ (name arg ...) body)
      (define name 
        (let ((memo (make-hash-table equal-comparator)))
          (lambda (arg ...)
            (let* ((key (list arg ...))
                   (value (hash-table-get memo key '())))
              (if (not (null? value))
                  value
                  (let ((ret body))
                    (hash-table-set! memo key ret)
                    ret)))))))))

(define (lower-bound vec value)
  (meguru-method (vector-length vec) 0
    (lambda (i) (>= (vector-ref vec i) value))))

(define (upper-bound vec value)
  (meguru-method (vector-length vec) 0
    (lambda (i) (> (vector-ref vec i) value))))

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
          (else (error "Unknown message: MONOID" msg))))
  self)

(define (make-group op e inv)
  (define (self msg . args)
    (cond ((eq? msg 'id) e)
          ((eq? msg 'op) op)
          ((eq? msg 'inv) (apply inv args))
          ((eq? msg 'apply) (apply op args))
          (else (error "Unknown message: GROUP" msg))))
  self)

(define-constant +int64-max+ (- (ash 1 63) 1))
(define-constant +int64-min+ (lognot (- (ash 1 63) 1)))
(define-constant +uint64-max+ (- (ash 1 64) 1))
(define-constant sum-group (make-group + 0 -))
(define-constant prod-monoid (make-monoid * 1))
(define-constant int64-min-monoid (make-monoid min +int64-max+))
(define-constant int64-max-monoid (make-monoid max +int64-min+))

(define-constant bintree-null
  (lambda (op)
    (cond ((eq? op 'null?) #t)
          (else (error "Unknown message: BINTREE-NULL" op)))))
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
               (print-error (node 'value)))
        (begin (show (node 'left) (inc level))
               (dotimes (i level)
                 (display-error #\tab))
               (print-error (node 'value))
               (show (node 'right) (inc level)))))
  (define (self op . args)
    (cond ((eq? op 'left) left)
          ((eq? op 'value) value)
          ((eq? op 'right) right)
          ((eq? op 'null?) #f)
          ((eq? op 'leaf?) (leaf? self))
          ((eq? op 'rotate-right) (rotate-right self))
          ((eq? op 'rotate-left) (rotate-left self))
          ((eq? op 'set-left!) (set! left (car args)))
          ((eq? op 'set-value!) (set! value (car args)))
          ((eq? op 'set-right!) (set! right (car args)))
          ((eq? op 'show) (show self 0) (newline-error))
          (else (error "Unknown message: BINTREE" op))))
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
  (define (update! tree size monoid index value)
    (if (= size 1)
        (tree 'set-value! value)
        (let1 center (half size)
          (if (< index center)
              (update! (tree 'left) center monoid index value)
              (update! (tree 'right) center monoid (- index center) value))
          (tree 'set-value! (monoid 'apply ((tree 'left) 'value) ((tree 'right) 'value))))))
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
            ((eq? op 'update!)
              (let ((index (car args))
                    (value (cadr args)))
                (update! tree size monoid index value)))
            ((eq? op 'show) (tree 'show))
            (else (error "Unknown message: SEGMENT-TREE" op))))
    self)
  (let1 size (next-power-of-2 size)
    (make-object (make-tree size) size monoid)))

(define (make-cumsum list-or-vec)
  (define (show v)
    (display-error v)
    (newline-error))
  (define (make-object v)
    (define (self op . args)
      (cond ((eq? op 'vector) v)
            ((eq? op 'size) (dec (vector-length v)))
            ((eq? op 'get) (let1 index (car args) (self 'query index (inc index))))
            ((eq? op 'query)
              (let ((lindex (car args))
                    (rindex (cadr args)))
                (- (vector-ref v rindex) (vector-ref v lindex))))
            ((eq? op 'show) (show v))
            (else (error "Unknown message: CUMSUM" op))))
    self)
  (let1 v (if (list? list-or-vec)
              (list->vector (cons 0 list-or-vec))
              (vector-copy list-or-vec -1 (vector-length list-or-vec) 0))
    (dotimes (i (dec (vector-length v)))
      (inc! (vector-ref v (inc i)) (vector-ref v i)))
    (make-object v)))

(define (make-unionfind n)
  (define (root parents n)
    (if (= (vector-ref parents n) -1)
        n
        (let1 new-parent (root parents (vector-ref parents n))
          (set! (vector-ref parents n) new-parent)
          new-parent)))
  (define (unite? parents a b) (= (root parents a) (root parents b)))
  (define (merge parents ranks a b)
    (let ((aroot (root parents a))
          (broot (root parents b)))
      (cond ((= aroot broot) #f)
            ((< (vector-ref ranks aroot) (vector-ref ranks broot))
              (merge parents ranks broot aroot))
            (else (set! (vector-ref parents broot) aroot)
                  (inc! (vector-ref ranks a))
                  #t))))
  (define (make-object n parents ranks)
    (define (self op . args)
      (cond ((eq? op 'unite?) (unite? parents (car args) (cadr args)))
            ((eq? op 'merge) (merge parents ranks (car args) (cadr args)))
            (else (error "Unknown message: UNIONFIND" op))))
    self)
  (let ((parents (make-vector n -1))
        (ranks (make-vector n 1)))
    (make-object n parents ranks)))

(define (print-yn b)
  (if b (print "Yes") (print "No")))

(define (main args)
  (let* ((h (read))
         (w (read))
         (q (read))
         (t (make-vector q))
         (ra (make-vector q))
         (ca (make-vector q))
         (rb (make-vector q))
         (cb (make-vector q)))
    (dotimes (i q)
      (set! (vector-ref t i) (read))
      (set! (vector-ref ra i) (dec (read)))
      (set! (vector-ref ca i) (dec (read)))
      (when (= (vector-ref t i) 2)
            (set! (vector-ref rb i) (dec (read)))
            (set! (vector-ref cb i) (dec (read)))))
    (let1 result (solve h w q t ra ca rb cb)
      (for-each (lambda (p)
                  (print-yn p))
                result)))
  0)

(define-constant dx (list 1 0 -1 0))
(define-constant dy (list 0 1 0 -1))

(define (solve h w q t ra ca rb cb)
  (define (index y x) (+ (* w y) x))
  (define (point-valid? y x) (not (or (negative? y) (negative? x) (>= y h) (>= x w))))
  (let ((uf (make-unionfind (* h w)))
        (board (make-vector (* h w) 0)))
    (define (fill! y x) (set! (vector-ref board (index y x)) 1))
    (define (fill? y x) (= (vector-ref board (index y x)) 1))
    (define (query-t1 r c)
      (fill! r c)
      (let loop ((dy dy) (dx dx))
        (if (null? dy)
            #f
            (let ((y (+ r (car dy)))
                  (x (+ c (car dx))))
              (when (and (point-valid? y x) (fill? y x))
                    (uf 'merge (index r c) (index y x)))
              (loop (cdr dy) (cdr dx))))))
    (define (query-t2 ra ca rb cb)
      (and (fill? ra ca)
           (fill? rb cb)
           (uf 'unite? (index ra ca) (index rb cb))))
    (let loop ((i 0) (acc nil))
      (if (< i q)
          (let ((t (vector-ref t i))
                (ra (vector-ref ra i))
                (ca (vector-ref ca i))
                (rb (vector-ref rb i))
                (cb (vector-ref cb i)))
            (if (= t 1)
                (begin (query-t1 ra ca) (loop (inc i) acc))
                (loop (inc i) (cons (query-t2 ra ca rb cb) acc))))
          (reverse! acc)))))
            
