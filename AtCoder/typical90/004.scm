(use gauche.collection)
(use srfi-1)
(use srfi-13)
(use srfi-42)
(use binary.io)

(define-constant nil '())
(define-syntax nil! (syntax-rules () ((_ x) (set! x '()))))
(define-inline (id x) x)
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

(define-inline (ceil-quotient a b) (quotient (dec (+ a b)) b))
(define-inline (diff a b) (abs (- a b)))

(define-inline (display-error x) (display x (current-error-port)))
(define-inline (newline-error) (newline (current-error-port)))

(define-syntax debug-print
  (syntax-rules ()
    ((_ body)
      (begin
        (let ((ret body))
          (display-error 'body)
          (display-error " => ")
          (display-error ret)
          (newline-error)
          ret)))))

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
  (let ((mid (quotient (+ ok ng) 2)))
    (cond ((< (diff ok ng) 2) ok)
          ((pred mid) (meguru-method mid ng pred))
          (else (meguru-method ok mid pred)))))

(define (main args)
  (let* ((h (read))
         (w (read))
         (a (vector-of-length-ec (* h w) (:port i (current-input-port)) i)))
    (let ((result (solve h w a)))
      (dotimes (i h)
        (dotimes (j w)
          (write (vector-ref result (+ (* i w) j)))
          (write-space))
        (write-lf))))
  0)

(define (solve h w a)
  (let ((row-sum (make-vector h 0))
        (column-sum (make-vector w 0))
        (b (make-vector (* h w))))
    (dotimes (i h)
      (dotimes (j w)
        (inc! (vector-ref row-sum i) (vector-ref a (+ (* i w) j)))
        (inc! (vector-ref column-sum j) (vector-ref a (+ (* i w) j)))))
    (dotimes (i h)
      (dotimes (j w)
        (vector-set! b (+ (* i w) j) (- (+ (vector-ref row-sum i)
                                           (vector-ref column-sum j))
                                        (vector-ref a (+ (* i w) j))))))
    b))

