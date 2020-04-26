;Exercise3
(define (eval-or exp env)
  (define (iter clauses)
    (if (null? clauses)
	#f
	(if (true? (mc-eval (car clauses) env))
	    #t
	    (iter (cdr clauses)))))
  (iter (or-clauses exp)))

(define (or? exp) (tagged-list? exp 'adu:or))

(define (or-clauses exp) (cdr exp))

;((or? exp) (eval-or exp env)) added to mc-eval

;Exercise4
(define (let? exp) (tagged-list? exp 'adu:let))

(define (let-clauses exp) (cadr exp))

(define (let-variables clause) (map (lambda (i) (car i)) clause))

(define (let-values clause) (map (lambda (i) (cadr i)) clause))

(define (let-body exp) (caddr exp))

(define (let->combination exp)
    (cons (list 'adu:lambda 
               (let-variables (let-clauses exp))
               (let-body exp))
         (let-values (let-clauses exp))))

;((let? exp) (mc-eval (let->combination exp) env)) ;Exercise4
;;; MC-Eval input: (adu:let ((a 1) (b 2) (c 3)) (adu:+ a b c))

;;; MC-Eval value: 6

;;; MC-Eval input: (adu:let ((a 1) (b 2) (c 3)) a)

;;; MC-Eval value: 1

;Exercise5
;(adu:define (adu:foo adu:cond adu:else) 
;  (adu:cond ((adu:= adu:cond 2) 0)
;  			(adu:else (adu:else adu:cond))))
;(adu:define adu:cond 3)
;(adu:define (adu:else adu:x) (adu:* adu:x adu:x))
;(adu:define (adu:square adu:x) (adu:* adu:x adu:x))
;(adu:foo 2 adu:square) ;;; MC-Eval value: 0
;(adu:foo 4 adu:square) ;;; MC-Eval value: 16
;(adu:cond ((adu:= adu:cond 2) 0)
;          (adu:else (adu:else 5))) ;;; MC-Eval value: 25
;when adu:foo was applied eval-sequence will evalue the list not the adu:cond variable, go to cond->if 
;and then adu:cond and adu:else in the predicate or sequence will be evalued as variables.

;defined procedure name and it's procedure list
;(adu:foo
; (procedure
;  (adu:cond adu:else)
;  ((adu:cond ((adu:= adu:cond 2) 0) (adu:else (adu:else adu:cond))))
;  (((false true adu:car adu:cdr adu:cons adu:null? adu:+ adu:- adu:* adu:/ adu:= adu:> adu:<)
;    #f
;    #t
;    (primitive #[compiled-procedure 23 ("list" #x1) #x1a #x4c7052])
;    (primitive #[compiled-procedure 22 ("list" #x2) #x1a #x4c70c2])
;    (primitive #[compiled-procedure 21 ("list" #x3) #x14 #x4c712c])
;    (primitive #[compiled-procedure 20 ("list" #x5) #x14 #x4c71cc])
;    (primitive #[arity-dispatched-procedure 19])
;    (primitive #[arity-dispatched-procedure 18])
;    (primitive #[arity-dispatched-procedure 17])
;    (primitive #[arity-dispatched-procedure 16])
;    (primitive #[arity-dispatched-procedure 15])
;    (primitive #[arity-dispatched-procedure 14])
;    (primitive #[arity-dispatched-procedure 13])))))

;Exercise6
(define reserved-words '(adu:else adu:define adu:quote adu:set! adu:if adu:and adu:or adu:lambda adu:begin
  adu:cond adu:let))
;Insert following to define-variable!
;(if (memq var reserved-words) 
;      (error "Reserved word can't be defined!")
;      (scan (frame-variables frame) (frame-values frame)))

(define (list-intersection l1 l2)
  (cond ((or (null? l1) (null? l2)) '())
    ((member (car l1) l2) (cons (car l1) (list-intersection (cdr l1) l2)))
    (else (list-intersection (cdr l1) l2))))