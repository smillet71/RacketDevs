#lang racket/gui

;
(require "../base/grid.rkt")
(require "gui.rkt")
(require "drawing-area.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; gui frame
(define gui%
  (class frame%
    (super-new [label "test-gui"] [style (list 'no-caption 'no-system-menu)] [min-width 500] [min-height 500])))

; frame creation and initial state
(define tw (create-gui))
(define-values (width height) (send tw  get-client-size))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; default pens and brushes
(define no-pen (new pen% [style 'transparent]))
(define no-brush (new brush% [style 'transparent]))
(define blue-brush (new brush% [color "blue"]))
(define yellow-brush (new brush% [color "yellow"]))
(define red-pen (new pen% [color (make-color 255 0 0 )] [width 1] [style 'solid]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; draw cell
(define (draw-cell dc x y i j)
  (let ((c (cell grid i j)))
    (if (= c 0)
        (send dc set-brush no-brush)
        (send dc set-brush yellow-brush))
    (send dc draw-rectangle (- x dcell2) (- y dcell2) dcell4 dcell4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; draw Grid Area
(define (draw-grid-area da dc)
  ; set defaults, pen and brushes
  (send dc set-scale 1 1)
  (send dc set-smoothing 'aligned)
  (send dc set-pen red-pen)
  (send dc set-brush no-brush)
  ; 
  (push-translation da1 dc (+ (/ dcell 2)(drawing-area-dpx da1)) (+ (/ dcell 2)(drawing-area-dpy da1)))
  ; draw point
  (for ([a (range ng)])
    (for ([ b (range ng)])
      (send dc draw-point (* a dcell) (* b dcell))))
  ; draw englobing rectangle
  ; draw grid
  (for ([x (range ng)])
    (for ([ y (range ng)])
      (draw-cell dc (* x dcell) (* y dcell) (+ gx0 x) (+ gy0 y))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; draw Status Area
(define (draw-status-area da dc)
  ; set defaults, pen and brushes
  (send dc set-scale 1 1)
  (send dc set-smoothing 'aligned)
  (send dc set-pen red-pen)
  (send dc set-brush no-brush)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; grid drawing area definition
(define ng 30)
(define h1 (* (floor (/ (- height 10) 10.0)) 10.0))
(define dh1 (floor (/ (- height h1) 2)))
(define dp1 5)
(define da1 (drawing-area dh1 dh1 h1 h1 dp1 dp1 draw-grid-area))
(define dcell (floor (/ (- h1 (* 2 dp1)) ng)))
(define dcell2 (floor (/ (- dcell 4) 2)))
(define dcell4 (* dcell2 2))

; status area
(define dp2 dp1)
(define dh2 dh1)
(define dw2 (+ h1 dp1 dp1 dp2))
(define w2 (- width dw2))
(define h2 50)
(define da2 (drawing-area dw2 dh2 w2 h2 dp2 dp2 draw-status-area))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; grid definition
(define grid (make-grid 1000))
(cell-set! grid 10 15 11234)
(cell-set! grid 1 1 11234)

; grid representation, position and number of represented rows/columns
(define gx0 0)
(define gy0 0)

; drawing area list
(define drawing-area-list (list da1 da2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
(define (paint-canvas canvas dc)
  (send dc set-scale 1 1)
  ; set default pen and brushes
  (send dc set-smoothing 'aligned)
  (send dc set-pen red-pen)
  (send dc set-brush no-brush)
  ; draw all areas
  (draw-area-list drawing-area-list dc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (on-mouse-event event)
  '())

(define (on-char-event event)
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; canvas drawing function
(define cv (create-canvas tw on-mouse-event on-char-event paint-canvas))
(send cv  set-canvas-background [make-object color% "black"])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 
;(new timer%	 
;     [notify-callback 
;      (lambda () 
;        (send cv refresh-now))
;      ]	 
;     [interval 100]	 
;     [just-once? #f])



