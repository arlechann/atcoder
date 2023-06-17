(define nil '())
(define (id x) x)
(define (inc n) (+ n 1))
(define (dec n) (- n 1))
(define (ceil-quotient a b) (quotient (dec (+ a b)) b))
(define (diff a b) (abs (- a b)))
(define (atom? x) (not (pair? x)))

(define (display-error x) (display x (current-error-port)))
(define (newline-error) (newline (current-error-port)))

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

(define (read-times n)
  (let rec ((i 0) (ret nil))
    (if (= i n)
        (reverse! ret)
        (rec (inc i) (cons (read) ret)))))

(define (sum ls) (reduce + 0 ls))

(define (flatten ls)
  (cond ((null? ls) nil)
        ((atom? (car ls)) ls)
        (else (append (flatten (car ls)) (flatten (cdr ls))))))

(define (flatten-1 ls)
  (cond ((null? ls) nil)
        ((atom? ls) ls)
        (else (append (car ls) (flatten (cdr ls))))))

(define (maplist-1 proc ls)
  (if (null? ls)
      nil
      (cons (proc ls) (maplist-1 proc (cdr ls)))))

(define (maplist proc ls)
  (maplist-1 proc ls))

(define (meguru-method ok ng pred)
  (let ((mid (quotient (+ ok ng) 2)))
    (cond ((< (diff ok ng) 2) ok)
          ((pred mid) (meguru-method mid ng pred))
          (else (meguru-method ok mid pred)))))

(define (main args)
  (let* ((n (read)))
    (let ((result (solve n)))
      (let loop ((result result))
        (when (not (null? result))
              (display (car result))
              (newline)
              (loop (cdr result))))))
  0)

(define (solve n)
  (make-parenthes-sequence n))

(define (make-parenthes-sequence n)
  (define (rec i level chars)
    (if (= i n)
        (if (zero? level)
            (list (list->string (reverse chars)))
            nil)
        (if (not (zero? level))
            (append (rec (inc i) (inc level) (cons #\( chars))
                    (rec (inc i) (dec level) (cons #\) chars)))
            (rec (inc i) (inc level) (cons #\( chars)))))
  (rec 0 0 nil))

