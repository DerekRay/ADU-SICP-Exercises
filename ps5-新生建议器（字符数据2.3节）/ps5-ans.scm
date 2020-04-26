;;; Scheme code for Twenty-One Simulator 
;Exercise7


;conventional-wisdom
** (I want to attend eecs101 this year)
(eecs101 is too much work for freshmen -- wait until next year)

** (how about phys101 first year?)
(students really enjoy phys101)

** (I expect the seminar of the year)
(i hear that snorkeling in boston harbor is a really exciting seminar)

** (I want to take math next season)
(too bad -- math is not offered next season)

** (I want to double major in math and stamp collecting)      
(math is fascinating and you can make a living doing it if stamp collecting does not work out)
** (I want to double major in math)
(doing a double major is a lot of work)

;subject-knowledge
** (what is geo104 about)
(geo104 is about planets)

** (what are eecs101 and geo104 about)
(eecs101 is about scheming with yanco and pezaris)
(geo104 is about planets)

** (how many units is eecs101)
(eecs101 is a 5 unit subject)

** (how many units are eecs101 and geo104)
(eecs101 is a 5 unit subject)
(geo104 is a 4 unit subject)

** (what are the prerequisites for eecs101 and geo104)
(the prerequisites for eecs101 are true-grit)
(the prerequisites for geo104 are math203 phys102)

** (can I take geo104)
(the prerequisites for geo104 are math203 phys102)

;Exercise8
(define (all-prerequisites subject)
  (let ((entry (find subject catalog)))
    (if (null? entry) '()
	(let ((a (entry-prerequisites entry)))
	  (if (null? a) '()
	      (list-union a
			  (reduce list-union
				  '()
				  (map all-prerequisites a))))))))

;Exercise9
   (make-rule
    `(can I take (? s ,in-catalog) if I have not taken (? t ,in-catalog))
    (lambda (dict)
      (let ((course1 (entry-subject (value 's dict)))
	    (course2 (entry-subject (value 't dict))))
	(if (member course2 (all-prerequisites course1))
	    (write-line (append '(No way Jose!)
				(list course2)
				'(is a prerequisite for)
				(list course1)))
	    (write-line '(Sure! Be my guest.))))))

;Exercise10
(define (check-circular-prerequisite lst)
  (null? (list-intersection lst (reduce list-union '() (map all-prerequisites lst)))))

;Exercise11
(define (total-credits lst)
  (define (get-credits course)
  	(entry-units (find course catalog)))
  (reduce + 0 (map get-credits lst)))

;Exercise12
(define (check-subject-list lst)
  (let ((prerequisites (reduce list-union '() (map all-prerequisites lst))))
  (cond ((not (check-circular-prerequisite lst))
  			(write-line '(You are not allowed to take a course and a prerequisite simultaneously)))
	  ((> (total-credits lst) 18)
	  	(write-line '(The total credits exceed the limit!)))
	  (else 
	  	(write-line (append '(You need the prerequisites: ) prerequisites))))))

;Exercise13
(make-rule
    `(I want to take (?? s ,subjects))
    (lambda (dict)
      (check-subject-list (map entry-subject (value 't dict)))))