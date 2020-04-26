;Exercise3
;(adu:define (adu:fib n)
;  (adu:cond ((adu:= n 0) 0)
;            ((adu:= n 1) 1)
;            (adu:else (adu:+ (adu:fib (adu:- n 1)) (adu:fib (adu:- n 2))))))
;mc-eval running time
;(show-time (lambda () (current-evaluator '(adu:fib 10) the-global-environment)))
;process time: 30 (30 RUN + 0 GC); real time: 37
;analyzing eval running time
;(show-time (lambda () (current-evaluator '(adu:fib 10) the-global-environment)))
;process time: 20 (20 RUN + 0 GC); real time: 21

;Exercise4
;mc-eval running time
;(show-time (lambda () (current-evaluator '(adu:fib 25) the-global-environment)))
;process time: 44200 (43880 RUN + 320 GC); real time: 47027
;Value: 75025
;analyzing eval running time
;(show-time (lambda () (current-evaluator '(adu:fib 25) the-global-environment)))
;process time: 21490 (21350 RUN + 140 GC); real time: 22695

;Exercise7
;(controller
;  (assign num (const n))
;  (assign product (const 1))
;  (assign counter (const 1))
;  Iter
;    test-counter
;      (test (op >) (reg counter) (reg num))
;      (branch (label factorial-done))
;      (assign t1 (op *) (reg product) (reg counter))
;      (assign t2 (op +) (reg counter) (const 1))
;      (assign product (reg t1))
;      (assign counter (reg t2))
;      (goto (label Iter))
;  factorial-done)
;((lambda (a b) (/ 2 (+ b a))) 2 1)