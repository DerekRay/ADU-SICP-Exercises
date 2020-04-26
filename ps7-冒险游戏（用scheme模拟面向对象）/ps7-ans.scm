;Warm up Exercise1
;a
(define (make-flip)
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      (if (even? count)
          0
          1))))
(define flip1 (make-flip))
(define flip2 (make-flip))
;c
;(flip1)
;1
;(flip2)
;1

;Warm up Exercise2
(define flip (make-flip))
(define flap1 (flip))           ;flap1 is just the value that flip first invorked returning 
(define (flap2) (flip))         ;flap2 is a procedure with no parameters returns flip
(define flap3 flip)             ;flap3 is the alia of flip
(define (flap4) flip)           ;flap4 is a procedure returns flip
;flap1
;Value: 1
;flap2
;Value 13: #[compound-procedure 13 flap2]
;flap3
;Value 14: #[compound-procedure 14]
;flap4
;Value 15: #[compound-procedure 15 flap4]
;(flap1)
;The object 1 is not applicable.
;To continue, call RESTART with an option number:
; (RESTART 2) => Specify a procedure to use in its place.
; (RESTART 1) => Return to read-eval-print level 1.
;(flap2)
;Value: 0
;(flap3)
;Value: 1
;flap4
;Value 15: #[compound-procedure 15 flap4]
;flap1
;Value: 1
;(flap3)
;Value: 0
;(flap2)
;Value: 1

;Warm up Exercise3 saw the image
;Warm up Exercise4 saw the image
;Warm up Exercise5 saw the image

;Exercise1
;saw the solution
;Exercise2
;a) John is more restless   b)They moved simultaneously about about 1 in every 4 ticks
;Exercise3
(define derek (make&install-person 'derek student-lab 100))
(define late-homework
  (make&install-thing 'late-homework student-lab))
(ask derek 'take late-homework)
(ask derek 'move-to faculty-office)
(ask derek 'lose late-homework)
(ask john 'take late-homework)
(go-to-heaven derek)
;Exercise4
;Exercise5
(define (make-card-locked-place name)
  (let ((place (make-place name)))
    (lambda (message)
      (cond ((eq? message 'accept-person?)
              (lambda (self person)
                (not (null? (filter (lambda (thing) (is-a thing 'id-card?)) (ask person 'possessions))))))
            ((eq? message 'card-locked-place?)
              (lambda () #t))
            (else (get-method place message))))))
(ask holly 'move-to faculty-office)
(ask holly 'take holly-card)
(define tmp (make-card-locked-place 'tmp))
(ask tmp 'accept-person? holly)
;Exercise7
(define (ppl-own-idcard? id possessions)
  (cond ((null? possessions) #f)
    ((and (is-a (car possessions) 'id-card?) (eqv? id (ask (car possessions) 'id))) #t)
    (else (ppl-own-idcard? id (cdr possessions)))))
(define (find-ppl id plst)
  (filter (lambda (person) (ppl-own-idcard? id (ask person 'possessions))) plst))
(define (make-ogre name id threshold)
  (let ((person (make-person name dungeon threshold)))
    (lambda (message)
      (cond ((eq? message 'act)
             (lambda (self)  
               (let ((others (other-people-at-place self (ask self 'place))))
                 (for-each (lambda (p) (ask self 'eat-person p)) (find-ppl id others))
                 ((get-method person 'act) self))))
            ((eq? message 'eat-person)
             (lambda (self person)
               (ask self 'say
                    (list "Growl.... I'm going to eat you,"
                          (ask person 'name)))
               (go-to-heaven person)
               (ask self 'say
                    (list "Chomp chomp." (ask person 'name)
                          "tastes yummy!"))
               '*burp*))
            (else (get-method person message))))))

(define (make&install-ogre name id threshold)
  (let ((ogre (make-ogre name id threshold)))
    (ask ogre 'install)
    ogre))
(define (report-stolen-card id)
  (make&install-ogre 'ogre id 1))
;Exercise8
(define big-brother
  (let ((card-list '()))
    (lambda (message) 
      (cond ((eq? message 'inform) 
        (lambda (self card-id place) 
          (let ((report (list card-id place (current-time))))
            (if (filter (lambda (card) 
                            (and (eqv? (car report) (car card))
                              (not (eq? (cdar report) (cdar card))) 
                              (eqv? (caddr report) (caddr card)))) card-list)
              (begin
		            (report-stolen-card card-id)
		            (display-message (list "Big Brother is watching"))
		          #t)
              (begin
		            (set! card-list (cons report card-list))
		          #t)))))))))
;Exercise6
(define (id-in-list item lst)
  (cond ((null? lst) #f)
    ((eqv? item (car lst)) #t)
    (else (id-in-list item (cdr lst)))))
(define (make-protected-student-lab name)
  (let ((place (make-place name))
        (id-numbers '())) ;initial registered id
    (lambda (message)
      (cond ((eq? message 'accept-person?)
              (lambda (self person)
                (filter (lambda (thing)
			            (and (is-a thing 'id-card?)
			              (filter (lambda (id) 
					            (if (eqv? id (ask thing 'id))
					              (ask big-brother 'inform (ask thing 'id) self)
					              #f)) id-numbers)))
		              (ask person 'possessions))))
            ((eq? message 'protected-student-lab?)
              (lambda () #t))
            ((eq? message 'register-card)
              (lambda (self card) 
                (if (and (is-a card 'id-card?) (eq? (ask card 'place) self))
                  (begin (set! id-numbers (cons (ask card 'id) id-numbers))
                    (display "ACCESS GRANTED\n"))
                  (begin (display "ACCESS DENIED\n") #f))))
            (else (get-method place message))))))
(define lab1 (make-protected-student-lab 'lab1))
(define lab2 (make-protected-student-lab 'lab2))
(can-go-both-ways student-lab   'south     'north lab1)
(can-go-both-ways lab1          'east      'west     lab2)
(define card1 (make&install-id-card 'card1 lab1 99))
(define card2 (make&install-id-card 'card2 lab1 100))
(define dr (make&install-person 'dr lab1 3))
(ask lab1 'register-card card1)
(ask dr 'take card1)
(ask lab1 'accept-person? dr)
(ask dr 'go 'east)
(ask lab2 'accept-person? dr)
(ask dr 'take card2)
(ask dr 'go 'east)
(ask lab2 'accept-person? dr)
(define temp-card (make&install-id-card 'temp-card supply-closet 100))
(define temp-person (make&install-person 'temp-person supply-closet 3))
(ask temp-person 'take temp-card)
(report-stolen-card 100) ;ogre is going to eat temp-person
(clock)
(clock)
;Exercise9
(define (make-student name birthplace threshold)
  (let ((person (make-person name birthplace threshold)))
    (lambda (message)
      (cond ((eq? message 'act)
	     (lambda (self) 
	       (let ((wanted (pick-random 
			      (filter (lambda (thing) (and (is-a thing 'ownable?)
							   (not (is-a thing 'owned?))))
				      (ask (ask self 'place) 'things))))
		     (forsale (pick-random (ask self 'possessions))))
		 (cond ((and wanted (= (random 2) 0))
			(ask self 'take wanted))
		       ((and forsale (= (random 4) 0))
			(ask self 'lose forsale))
		       (else ((get-method person 'act) self))))))
	     (else (get-method person message))))))