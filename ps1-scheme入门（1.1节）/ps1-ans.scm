;exercise1
(define fold
  (lambda (x y)
    ( (spindle x)
       (+ (mutilate y)
	  (spindle x)))))

(define spindle
  (lambda (w) ( w w)))

(define mutilate
  (lambda (z)
    (+ (spindle z) z)))

;exercise2
(define fact
  (lambda (n)
    (if (= n 0)		
	1
	(* n (fact (- n 1))))))

;exercise3
(define comb
  (lambda (n k)
    (/ (fact n)
       (* (fact k) (fact (- n k))))))

;exercise10
(define cube
  (lambda (x)
    (* x x x)))

;exercise11
(define cubes
  (lambda (a b)
    (+ (cube a) (cube b))))

;exercise12
(define biggest-of-three
  (lambda (x y z)
    (cond ((>= x y) (if (>= x z) x z))
      (else (if (>= y z) y z)))))

;exercise13
(define biggest-of-threecubes
  (lambda (x y z)
    (cube (biggest-of-three x y z))))

;exercise14
(define (square x) (* x x))
(define f
  (lambda (x a0 a1 a2 a3)
    (+ a0 (* a1 x) (* a2 (square x)) (* a3 (cube x)))))