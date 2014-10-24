#lang racket

(provide (struct-out drawing-area) 
         push-matrix
         push-translation
         pop-matrix
         in-drawing-area
         out-drawing-area
         drawing-area-on-mouse-event
         drawing-area-on-char-event
         draw-area-list
         set-drawing-area-list)

; drawing area
; x/y origin 
; dx/dy width and height
; dpx/dpx borders
; drawing function (1 arg: dc)
; matrix list of transformations for push/pop operations
(struct drawing-area 
  (name x y dx dy dpx dpy func fmouse fchar [matrix #:mutable #:auto] ) #:transparent #:auto-value '())

; list of drawing areas
(define drawing-area-list '())

; setting the list of drawing areas
(define (set-drawing-area-list l)
  (set! drawing-area-list l))

; memorize current transformation
(define (push-matrix da dc)
  (let ((matrix-list (drawing-area-matrix da))
        (matrix (send dc get-transformation)))
    (if (null? matrix-list)
        (set-drawing-area-matrix! da (list matrix))
        (set-drawing-area-matrix! da (cons matrix matrix-list)))))

; memorize then translate 
(define (push-translation da dc dx dy )
  (push-matrix da dc)
  (send dc translate dx dy))

; remove last transformation and apply the previous one
(define (pop-matrix da dc)
  (let ((matrix-list (drawing-area-matrix da)))
    (when (not (null? matrix-list))
      (send dc set-transformation (first matrix-list))
      (set-drawing-area-matrix! da (cdr matrix-list)))))

; memorize initial transformation and translate into the area
(define (in-drawing-area da dc)
  (push-translation da dc (drawing-area-x da) (drawing-area-y da)))

; go back to initial state
(define (out-drawing-area da dc)
  (let ((matrix-list (drawing-area-matrix da)))
    (when (not (null? matrix-list))
      (pop-matrix da dc)
      (out-drawing-area da dc))))

; draw a border around a particular area
(define (draw-border da dc)
  (send dc draw-rectangle 0 0 (drawing-area-dx da) (drawing-area-dy da)))

; draw a list of areas
(define (draw-area-list dc)
  (map (lambda (da)
         (in-drawing-area da dc)
         (draw-border da dc)
         ((drawing-area-func da) da dc)
         (out-drawing-area da dc))
       drawing-area-list))

; verify coordinates
(define (inside-area x y x1 y1 x2 y2)
  (and (>= x x1)
       (>= y y1)
       (< x x2)
       (< y y2)))

; verify coordinates are inside a specific drawing area
(define (inside-drawing-area da x y)
  (inside-area x y
               (drawing-area-x da) (drawing-area-y da)
               (+ (drawing-area-x da) (drawing-area-dx da))
               (+ (drawing-area-y da) (drawing-area-dy da))))

; manage mouse event (mouse or char)
(define (drawing-area-on-mouse-event event)
  (let ((x (send event get-x))
        (y (send event get-y)))
    (map (lambda (da) 
           (let ((f (drawing-area-fmouse da)))
             (when (not (null? f))   
               (when (inside-drawing-area da x y) 
                 (f event (- x (drawing-area-x da)) (- y (drawing-area-y da)))))))
         drawing-area-list)))


; manage char event (mouse or char)
(define (drawing-area-on-char-event event)
  (map (lambda (da) 
         (let ((f (drawing-area-fchar da)))
           (when (not (null? f))   
             (f event 0 0))))
       drawing-area-list))

