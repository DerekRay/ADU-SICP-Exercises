;;; Scheme code for Twenty-One Simulator 

(define (twenty-one player-strategy house-strategy)
  (let ((house-initial-hand (make-new-hand (deal))))
    (let ((player-hand
           (play-hand player-strategy
                      (make-new-hand (deal))
                      (hand-up-card house-initial-hand))))
      (if (> (hand-total player-hand) 21)
          0                                ; ``bust'': player loses
          (let ((house-hand 
                 (play-hand house-strategy
                            house-initial-hand
                            (hand-up-card player-hand))))
            (cond ((> (hand-total house-hand) 21)
                   1)                      ; ``bust'': house loses
                  ((> (hand-total player-hand)
                      (hand-total house-hand))
                   1)                      ; house loses
                  (else 0)))))))           ; player loses

(define (play-hand strategy my-hand opponent-up-card)
  (cond ((> (hand-total my-hand) 21) my-hand) ; I lose... give up
        ((strategy my-hand opponent-up-card) ; hit?
         (play-hand strategy
                    (hand-add-card my-hand (deal))
                    opponent-up-card))
        (else my-hand)))                ; stay


(define (deal) (+ 1 (random 10)))

;(define (make-new-hand first-card)
;  (make-hand first-card first-card))
(define (make-new-hand first-card)
  (make-hand first-card first-card (list first-card)))

;(define (make-hand up-card total)
;  (cons up-card total))
;exercise8
(define (make-hand up-card total card-list)
  (list up-card total card-list))

;(define (hand-up-card hand)
;  (car hand))
;exercise12
(define (hand-up-card hand)
  (car hand))

(define (hand-total hand)
  (car (cdr hand)))

(define (list-of-cards hand)
  (car (cdr (cdr hand))))

;(define (hand-add-card hand new-card)
;  (make-hand (hand-up-card hand)
;             (+ new-card (hand-total hand))))
;exercise9
(define (hand-add-card hand new-card)
  (make-hand (hand-up-card hand)
             (+ new-card (hand-total hand))
             (cons new-card (list-of-cards hand))))

(define (hit? your-hand opponent-up-card)
  (newline)
  (display "Opponent up card ")
  (display opponent-up-card)
  (newline)
  (display "Your Total: ")
  (display (hand-total your-hand))
  (newline)
  (display "Your Cards: ")
  (display (list-of-cards your-hand)) ;exercise14
  (newline)
  (user-wants-hit?))

(define (user-wants-hit?) 
  (let ((x (prompt-for-command-char "Hit? ")))
    (display x)
    (eq? x '#\y))) ;prompt-for-command char returns #\ before char

;exercise3
(define (stop-at n)
  (lambda (your-hand opponent-up-card) 
    (< (hand-total your-hand) n)))

;exercise4
(define (test-strategy strategy1 strategy2 frequency)
  (define (iter frequency result)
    (if (= 0 frequency)
      result
      (iter (dec frequency) (+ result (twenty-one strategy1 strategy2)))
      ))
  (iter frequency 0))

;exercise5
(define (watch-player strategy)
  (lambda (your-hand opponent-up-card) 
    (let ((a (strategy your-hand opponent-up-card)))
      (display "Total of player's hand: ")
      (hand-total your-hand)
      (newline)
      (display "Opponent's up card: ")
      (display opponent-up-card)
      (newline)
      (if (eq? a #t) 
        (display "Player's move: Hit")
        (display "Player's move: hold")
        )
        (newline)
        (newline)
        (newline)
        (newline)
        a)))
;exercise6
(define (louis your-hand opponent-up-card)
  (cond ((< (hand-total your-hand) 12) #t)
    ((> (hand-total your-hand) 16) #f) 
    ((and (= (hand-total your-hand) 12) (< opponent-up-card 4)) #t) 
    ((and (= (hand-total your-hand) 16) (= opponent-up-card 10)) #f)
    (else (> opponent-up-card 6))))

;exercise7
(define (both strategy1 strategy2)
  (lambda (your-hand opponent-up-card) 
    (if (and (strategy1 your-hand opponent-up-card) (strategy2 your-hand opponent-up-card)) #t)))

