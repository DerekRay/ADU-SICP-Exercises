
;Exercise 2.44
(define (up-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split painter (- n 1))))
     (below painter (beside smaller smaller)))))
;Exercise 2.45
(define (split op1 op2)
  (lambda (painter n) 
  (if (= n 0)
    painter
    (let ((smaller ((split op1 op2) painter (- n 1))))
      (op1 painter (op2 smaller smaller))))))
;Exercise 2.46
(define (make-vect x y)
  (list x y))
(define (xcor-vect v)
  (car v))
(define (ycor-vect v)
  (cadr v))
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v1)) (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v1)) (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect n v2)
  (make-vect (* n (xcor-vect v2)) (* n (ycor-vect v2))))
;Exercise 2.47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (frame-origin f)
  (car f))
(define (frame-edge1 f)
  (cadr f))
(define (frame-edge2 f)
  (caddr f))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (frame-origin f)
  (car f))
(define (frame-edge1 f)
  (cadr f))
(define (frame-edge2 f)
  (cddr f))
;Exercise 2.48
(define (make-segment startpoints endpoints)
  (list startpoints endpoints))
(define (start-segment s)
  (car s))
(define (end-segment s)
  (cadr s))
;Exercise 2.49
;a
(define top-left (make-vect 0.0 1.0))
(define top-right (make-vect 1.0 1.0))
(define bottom-left (make-vect 0.0 0.0))
(define bottom-right (make-vect 0.0 1.0))
(define top (make-segment top-left top-right))
(define left (make-segment bottom-left top-left))
(define right (make-segment bottom-right top-right))
(define bottom (make-segment bottom-left bottom-right))
(define a_painter (segments-painter (list top left right bottom)))
;b
(define x1 (make-segment top-left bottom-right))
(define x2 (make-segment bottom-left top-right))
(define b_painter (segments-painter (list x1 x2)))
;c
;d
  
;Exercise 2.50
(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
          (transform-painter
          painter1
          (make-vect 0.0 0.0)
          split-point
          (make-vect 0.0 1.0)))
          (paint-right
          (transform-painter
          painter2
          split-point
          (make-vect 1.0 0.0)
          (make-vect 0.5 1.0))))
          (lambda (frame)
          (paint-left frame)
          (paint-right frame)))))
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                  new-origin
                  (sub-vect (m corner1) new-origin)
                  (sub-vect (m corner2) new-origin)))))))
(define (flip-horiz painter)
  (transform-painter painter
  (make-vect 1.0 0.0)
  (make-vect 0.0 0.0)
  (make-vect 1.0 1.0)))
(define (rotate180 painter)
  (transform-painter painter
  (make-vect 1.0 1.0)
  (make-vect 0.0 1.0)
  (make-vect 1.0 0.0)))
(define (rotate270 painter)
  (transform-painter painter
  (make-vect 0.0 1.0)
  (make-vect 0.0 0.0)
  (make-vect 1.0 1.0)))
;Exercise 2.51
(define (blow1 painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
          (transform-painter
          painter1
          (make-vect 0.0 0.0)
          (make-vect 1.0 0.0)
          split-point))
          (paint-top
          (transform-painter
          painter2
          split-point
          (make-vect 1.0 0.5)
          (make-vect 0.0 1.0))))
          (lambda (frame)
          (paint-bottom frame)
          (paint-top frame)))))
(define (rotate90 painter)
  (transform-painter painter
    (make-vect 1.0 0.0)
    (make-vect 1.0 1.0)
    (make-vect 0.0 0.0)))
(define (blow2 painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter1))))
;Exercise 2.52
;a
(define diagnal (make-segment bottom-left top-right))
(segments-painter (list diagnal ))
;b
(define (corner-split painter n) ;original
  (if (= n 0)
    painter
    (let ((up (up-split painter (- n 1)))
      (right (right-split painter (- n 1))))
      (let ((top-left (beside up up))
      (bottom-right (below right right))
      (corner (corner-split painter (- n 1))))
      (beside (below painter top-left)
      (below bottom-right corner))))))
(define (corner-split painter n) 
  (if (= n 0)
    painter
    (let ((up (up-split painter (- n 1)))
      (right (right-split painter (- n 1))))
      (corner (corner-split painter (- n 1))))
      (beside (below painter up) (below right corner))))
;c
(define (square-limit painter n) ;original
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
    (below (flip-vert half) half))))
(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside quarter (flip-horiz quarter))))
    (below (flip-vert half) half))))