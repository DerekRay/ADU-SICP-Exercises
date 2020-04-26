;Exercise1 & Exercise2
(define (equ? x y) (apply-generic 'equ? x y))
;(RepNum RepNum) -> SchBool
(define (=number x y) (= x y))
(put 'equ? '(number number) =number)
;(define x (create-number 10))
;define y (create-number 10))
;(equ? x y)
;Value: #t
;(define y (create-number 11))
;(equ? x y)
;Value: #f

;Exercise3
(define r5/13 (create-rational (create-number 5) (create-number 13)))
(define r2 (create-rational (create-number 2) (create-number 1)))
(define rsq (square (add r5/13 r2)))
;rsq
;(rational (number . 961) number . 169)
;(con 'rational (cons (cons 'number 961) (cons 'number 169)))

;Exerxise4
;(RepRat RepRat) -> SchBool
(define (equ-rat? x y)
  (equ? (mul (numer x) (denom y)) (mul (denom x) (numer y))))
(put 'equ? '(rational rational) equ-rat?)
(define x (create-rational (create-number 5) (create-number 10)))
(define y (create-rational (create-number 1) (create-number 2)))
(equ? x y)
;Value: #t
(equ? r5/13 r2) 
;Value: #f

;Exercise5
;RepNum->RepRat
(define (repnum->reprat x)
  (make-rat (create-number x) (create-number 1)))
;(repnum->reprat 2) 
;Value 13: ((number . 2) number . 1)

;Exercise6
(put 'add '(number rational) (RRmethod->NRmethod +rational))
(put 'sub '(number rational) (RRmethod->NRmethod -rational))
(put 'mul '(number rational) (RRmethod->NRmethod *rational))
(put 'div '(number rational) (RRmethod->NRmethod /rational))
(put 'equ? '(number rational) (RRmethod->NRmethod equ-rat?))
;;;  ((RepRat,RepRat) --> T) --> ((RepNum,RepRat) --> T)
(define (RRmethod->RNmethod method)
  (lambda (rat num)
    (method
     rat
     (repnum->reprat num))))
(put 'add '(rational number) (RRmethod->RNmethod +rational))
(put 'sub '(rational number) (RRmethod->RNmethod -rational))
(put 'mul '(rational number) (RRmethod->RNmethod *rational))
(put 'div '(rational number) (RRmethod->RNmethod /rational))
(put 'equ? '(rational number ) (RRmethod->RNmethod equ-rat?))
(equ? n2 r2) 
;Value: #t
(equ? (sub (add n2 r5/13) r5/13) n2) 
;Value: #t

;Exercise7
;;;    (Variable, List(Sch-Num)) --> ({polynomial} X RepPoly)
(define (inc x) (+ x 1))
(define (create-numerical-polynomial x lst)
  (create-polynomial x (map create-number lst)))
(define p1 (create-numerical-polynomial 'x (list 1 5 0 -2))) 
;The solution is wrong, so below here for the value of polynomial
;(polynomial x (3 (number . 1)) (2 (number . 5)) (0 (number . -2)))

;Exercise8
;(proc Repterms) -> Repterms
(define (map-terms proc lst)
  (if (empty-termlist? lst)
    (the-empty-termlist)
    (adjoin-term (proc (first-term lst))
      (map-terms proc (rest-terms lst)))))
(define (square-polynomial p)
  (make-polynomial (mul-poly p p)))
(put 'square '(polynomial) square-polynomial)
(pp (square p1))
;(polynomial x (6 (number . 1)) (5 (number . 10)) (4 (number . 25)) (3 (number . -4)) (2 (number . -20)) (0 (number . 4)))

;Exercise9
(define p2
  (create-polynomial 
    'z 
    (list 
      p1
      (create-polynomial 'x (list (create-number 3)))
      (create-polynomial 'x (list (create-number 5))))))
(pp p2)
;(polynomial z
;            (2 (polynomial x (3 (number . 1)) (2 (number . 5)) (0 (number . -2))))
;            (1 (polynomial x (0 (number . 3))))
;            (0 (polynomial x (0 (number . 5)))))

;3/y
(define 3/y 
  (create-rational 
    (create-numerical-polynomial 'y (list 3))
    (create-numerical-polynomial 'y (list 1 0))
    ))
;y2+1/y
(define y2+1/y 
  (create-rational 
    (add (create-numerical-polynomial 'y (list 1 0 0)) (create-numerical-polynomial 'y (list 1)))
    (create-numerical-polynomial 'y (list 1 0))
  ))
;1/y+-1
(define 1/y+-1 
  (create-rational 
    (create-numerical-polynomial 'y (list 1))
    (add (create-numerical-polynomial 'y (list 1 0)) (create-numerical-polynomial 'y (list -1)))
    ))
;2
(define the2 
  (create-rational 
    (create-numerical-polynomial 'y (list 2))
    (create-numerical-polynomial 'y (list 1))
  ))
;p3
(define p3
  (create-polynomial 
    'x  
    (list  
      3/y 
      (create-rational (create-number 0) (create-number 1))
      y2+1/y 
      1/y+-1 
      the2)))
(pp p3)
;(polynomial x
;            (4 (rational (polynomial y (0 (number . 3))) polynomial y (1 (number . 1))))
;            (2 (rational (polynomial y (2 (number . 1)) (0 (number . 1))) polynomial y (1 (number . 1))))
;            (1 (rational (polynomial y (0 (number . 1))) polynomial y (1 (number . 1)) (0 (number . -1))))
;            (0 (rational (polynomial y (0 (number . 2))) polynomial y (0 (number . 1)))))

;Exercise10
(pp (square p2))
;(polynomial z
;           (4 (polynomial x (6 (number . 1)) (5 (number . 10)) (4 (number . 25)) (3 (number . -4)) (2 (number . -20)) (0 (number . 4))))
;           (3 (polynomial x (3 (number . 6)) (2 (number . 30)) (0 (number . -12))))
;           (2 (polynomial x (3 (number . 10)) (2 (number . 50)) (0 (number . -11))))
;           (1 (polynomial x (0 (number . 30))))
;           (0 (polynomial x (0 (number . 25)))))
(pp (square p3))
;(polynomial
;x
;(8 (rational (polynomial y (0 (number . 9))) polynomial y (2 (number . 1))))
;(6 (rational (polynomial y (4 (number . 6)) (2 (number . 6))) polynomial y (4 (number . 1))))
;(5 (rational (polynomial y (2 (number . 6)) (1 (number . -6))) polynomial y (4 (number . 1)) (3 (number . -2)) (2 (number . 1))))
;(4 (rational (polynomial y (6 (number . 1)) (4 (number . 2)) (3 (number . 12)) (2 (number . 1))) polynomial y (4 (number . 1))))
;(3
; (rational (polynomial y (4 (number . 2)) (3 (number . -2)) (2 (number . 2)) (1 (number . -2)))
;           polynomial
;           y
;           (4 (number . 1))
;           (3 (number . -2))
;           (2 (number . 1))))
;(2
; (rational (polynomial y (5 (number . 4)) (4 (number . -8)) (3 (number . 8)) (2 (number . -7)) (1 (number . 4)))
;           polynomial
;           y
;           (4 (number . 1))
;           (3 (number . -2))
;           (2 (number . 1))))
;(1 (rational (polynomial y (1 (number . 4)) (0 (number . -4))) polynomial y (2 (number . 1)) (1 (number . -2)) (0 (number . 1))))
;(0 (rational (polynomial y (0 (number . 4))) polynomial y (0 (number . 1)))))
(pp (square (square p2)))
;(polynomial
;z
;(8
; (polynomial x
;             (12 (number . 1))
;             (11 (number . 20))
;             (10 (number . 150))
;             (9 (number . 492))
;             (8 (number . 505))
;             (7 (number . -600))
;             (6 (number . -976))
;             (5 (number . 240))
;             (4 (number . 600))
;             (3 (number . -32))
;             (2 (number . -160))
;             (0 (number . 16))))
;(7
; (polynomial x
;             (9 (number . 12))
;             (8 (number . 180))
;             (7 (number . 900))
;             (6 (number . 1428))
;             (5 (number . -720))
;             (4 (number . -1800))
;             (3 (number . 144))
;             (2 (number . 720))
;             (0 (number . -96))))
;(6
; (polynomial x
;             (9 (number . 20))
;             (8 (number . 300))
;             (7 (number . 1500))
;             (6 (number . 2434))
;             (5 (number . -660))
;             (4 (number . -1650))
;             (3 (number . 24))
;             (2 (number . 120))
;             (0 (number . 56))))
;(5 (polynomial x (6 (number . 180)) (5 (number . 1800)) (4 (number . 4500)) (3 (number . -612)) (2 (number . -3060)) (0 (number . 504))))
;(4 (polynomial x (6 (number . 150)) (5 (number . 1500)) (4 (number . 3750)) (3 (number . -60)) (2 (number . -300)) (0 (number . -399))))
;(3 (polynomial x (3 (number . 900)) (2 (number . 4500)) (0 (number . -1260))))
;(2 (polynomial x (3 (number . 500)) (2 (number . 2500)) (0 (number . 350))))
;(1 (polynomial x (0 (number . 1500))))
;(0 (polynomial x (0 (number . 625)))))

;Exercise11
;RepTerms --> RepTerms
(define (negate-terms lst)
  (map-terms (lambda (term) (make-term (order term) (negate (coeff term)))) lst))
;;;   RepPoly --> RepPoly
(define (negate-poly p)
  (make-poly (variable p) (negate-terms (term-list p))))
;negate-polynomial has been difined in generic.scm

;Exercise12
;;;   (RepPoly, RepPoly) --> RepPoly
(define (sub-poly p1 p2)
  (add-poly p1 (negate-poly p2)))
;;;   (RepPoly, RepPoly) --> ({polynomial} X RepPoly)
(define (sub-polynomial p1 p2)
  (make-polynomial (sub-poly p1 p2)))
;;;   (RepPoly, RepPoly) --> RepPoly
(define (equ-poly? p1 p2)
  (=zero-poly? (sub-poly p1 p2)))
;;;   (RepPoly, RepPoly) --> Bool
(define (equ-polynomial? p1 p2)
  (equ-poly? p1 p2))

;Exercise13
(put 'negate '(polynomial) negate-polynomial)
(put 'sub '(polynomial polynomial) sub-polynomial)
(put 'equ? '(polynomial polynomial) equ-polynomial?)
