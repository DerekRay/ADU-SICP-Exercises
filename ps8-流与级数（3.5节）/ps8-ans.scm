;Exercise1
;s1: (cons 1 (lambda () 2))
;s2: (1 2 4 8 16 ...)

;Exercise2
(define (partial-sums s)
  (cons-stream (stream-car s)
    (add-streams (stream-cdr s) (partial-sums s))))
(define ones (cons-stream 1 ones))
(define integers (partial-sums ones))

;Exercise3
;x: (1 2 4 6 8 10 ...)
;mul-streams: every element of a multify the element of b in same position
;y: (1 1*1 1*1*2 1*1*2*3 1*1*2*3*4 1*1*2*3*4*5 ...)

;Exercise4
(define ones (cons-stream 1 ones))
(define non-neg-integers (cons-stream 0 integers))
(define alt-ones (cons-stream 1 (stream-map - alt-ones)))
(define zeros (add-streams alt-ones (stream-cdr alt-ones)))
(define s1 ones)
(define s2 integers)

;Exercise5
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2)) 
    (add-series (scale-stream (stream-car s2) (stream-cdr s1)) (mul-series s1 (stream-cdr s2)))))
(newline)
(show-series (mul-series ones ones) 5)
(display (series-coeff (mul-series s2 s2) 10)) ;286 is the coefficient of x10

;Exercise6
(define (invert-unit-series s) ;s is a unit power series
  (cons-stream 1 (negate-series (mul-series (stream-cdr s) (invert-unit-series s)))))
(newline)
(show-series (invert-unit-series s1) 10) ;Because of the delayed evaluation of cons-stream for the second argument

;Exercise7
(define (div-series s1 s2)
  (if (= (stream-car s2) 0)
    (error "The denominator has a zero constant term!")
    (mul-series s1 (invert-unit-series s2))))
(newline)
(show-series (div-series ones ones) 5) ;(1 0 0 0 0 ...)
(newline)
;(show-series (div-series ones non-neg-integers) 5) ;error case
(newline)
(show-series (div-series integers ones) 5) ;(1 1 1 1 1 ...)
(newline)
(show-series (div-series integers alt-ones) 5) ;(1 3 5 7 9 ...)

;Exercise8
(define integrate-item
  (let ((ref 0))
    (lambda (i)
      (begin (set! ref (+ ref 1)) (/ i ref)))))
(define (integrate-series-tail s)
  (define (iter s n) 
    (cons-stream (/ (stream-car s) n) 
      (iter (stream-cdr s) (+ n 1))))
  (iter s 1))
(newline)
(show-series (integrate-series-tail s2) 5) ;integrated series of integers

;Exercise9
;Because e^x = 1 + x + (1/2!)x2 + (1/3!)x3 + ...  the integrated series of exp-series(which is equal to exp-series itself)
; is equal to 1 plus exp-series 
(define cosine-series 
  (cons-stream 1 (integrate-series-tail (negate-series sine-series))))
(define sine-series 
  (cons-stream 0 (integrate-series-tail cosine-series)))
(newline)
(show-series sine-series 10) ;0 1 0 -1/6 0 1/120 0 -1/5040 0 1/362880
(newline)
(show-series cosine-series 10) ;1 0 -1/2 0 1/24 0 -1/720 0 1/40320 0

;Exercise10
(define (approximate x0 f) ;x0 is a value and f is a serie
  (define x0-series0 
    (cons-stream x0 (scale-stream x0 x0-series0)))
  (define x0-series (cons-stream 1 x0-series0))
  (define (product s1 s2)
    (cons-stream (* (stream-car s1) (stream-car s2)) (product (stream-cdr s1) (stream-cdr s2))))
  (define product-series (product f x0-series))
  (define (sum-series s) (cons-stream (stream-car s) (add-streams (stream-cdr s) (sum-series s))))
  (sum-series product-series)
)
(define exp-series
  (cons-stream 1 (integrate-series-tail exp-series)))
(newline)
(show-series (approximate 1 exp-series) 20)