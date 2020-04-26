#lang sicp
;exercise1
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ (term a) result))))
  (iter a 0))

;exercise2
;a
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
      result
        (iter (next a) (* (term a) result))))
  (iter a 1))
(define (factorial n)
  (product (lambda (x) x) 1 inc n))
;pi/4
(define (square_al x) (/ (* x (+ x 2)) (* (+ x 1) (+ x 1))))
(define (compute_pi n)
  (* 4 (product square_al 2 (lambda (x) (+ x 2)) n)))

;b
;alter product to recursive process
(define (product_al term a next b)
  (if (> a b)
    1
    (* (term a) (product term (next a) next b))))

;exercise3
;a
(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (sum term a next b)
  (accumulate * 1 term a next b))

;b accmulate in iterative version(not recursive version above)
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner (term a) result))))
  (iter a init))

;exercise4
(define (filtered-accumulate filter combiner null-value term a next b)
(cond ((> a b) null-value)
  ((filter a) (combiner (term a) (filtered-accumulate filter combiner null-value term (next a) next b)))
  (else (combiner (term a) (filtered-accumulate filter combiner null-value term (next a) next b)))))

;a
(define (sum_square_prime a b)
  (filtered-accumulate primes? + 0 square a inc b))

;b
(define (product_integers n)
  (filtered-accumulate (lambda (x) (= 1 (GCD x n))) * 1 (lambda (x) x) 1 inc n))

;exercise5
;my answer: 625, 4294967296
;check
(define (repeated p n)
  (cond ((= n 0) (lambda (x) x))
	((= n 1) p)
	(else (lambda (x) (p ((repeated p (- n 1)) x))))))
((repeated square 2) 5)
;Value: 625

((repeated square 5) 2)
;Value: 4294967296

;exercise6
(define (cont-frac-r n d k)
  (define (recur i)
    (if (> i k) 0
      (/ (n i) (+ (d i) (recur (+ i 1))))))
  (recur 1))

(define (cont-frac-i n d k)
  (define (iter i result)
    (if (< i 1)
      result
      (iter ((lambda (x) (- x 1)) i) (/ (n i) (+ (d i) result)))))
  (iter k 0))
(cont-frac-r (lambda (x) 1) (lambda (x) 1) 10) ; the result is 0.6179775280898876
(cont-frac-r (lambda (x) 1) (lambda (x) 1) 11) ; the result is 0.618033985017358
;k=10, this is the result which is accurate to 4 decimal places 

;exercise7
(define (estimate-pi k)
  (/ 4 (+ (brouncker k) 1 )))
(define (square x) (* x x))
(define (brouncker k)
  (cont-frac-i (lambda (i) (square (- (* 2 i) 1)))
                (lambda (i) 2)
                k
  ))
(estimate-pi 2500) ;result is 3.141992493637787, for k=2500, get 3 decimal places accuracy
;when we use cont-frac-i
(estimate-pi 20000) ;Aborting!: maximum recursion depth exceeded
;for cont-frac-r
(estimate-pi 20000) ;3.1416426510898874
;why? Because cont-frac-r is a recursibe procesure, there is a limit that storing later operations

;exercise8
(define (atan-cf k x)
  (cont-frac-i (lambda (i) 
   (if (= i 1) x
       (square (* (- i 1) x)))) 
       (lambda (i) (- (* 2 i) 1)) 
       k))

;exercise9
(define (atan x)
  (atan-cf x 1))     ;The pset means computing arctan 1? pi/4?
(atan 0) ;0
(atan 1) ;1
(atan 3) ;0.7916666666666666
(atan 10) ;0.7916666666666666
(atan 30) ;0.7853981633974483
(atan 100) ;0.7853981633974483

;exercise10
(define (nested-acc op r term k)
  (define (na i)
    (if (> i k)
      r
      ((op i) (term i) (na (+ i 1)))))
  (na 1))
(define (fx k x)
  (nested-acc (lambda (i) (if (even? i) +
                            (lambda (x y) (sqrt y))))
                 0 
                 (lambda (i) x)
                 k))
;exercise11
(fx 20 1) ;value is 1.6180165422314876, for k=20, which is accurate to 4 decimal places

;exercise12
(define (repeated p n)
  (cond ((= n 0) (lambda (x) x))
	((= n 1) p)
	(else (lambda (x) (p ((repeated p (- n 1)) x))))))
((repeated square 2) 5)
;Value: 625

((repeated square 5) 2)
;Value: 4294967296
(define (build n d b)
  (/ n (+ d b)))
(define (repeated-build k n d b)
  ((repeated (lambda (x) (build n d x)) k) b))

;exercise13
(repeated-build k 1 1 0) 

;exercise14
(define (r k b)
  (repeated-build k 1 1 b))
(r 2 0);0.5
