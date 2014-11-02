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
        (send dc set-brush blue-brush))
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
(define cell-size 32)
(define ng (floor (- (quotient (- height 10) cell-size) 1)))
(define h1 (+ 10 (* cell-size ng)))
(define dh1 5)
(define dp1 5)
(define dcell cell-size)
(define dcell2 (floor (/ (- dcell 4) 2)))
(define dcell4 (* dcell2 2))

; status area
(define dp2 dp1)
(define dh2 dh1)
(define dw2 (+ h1 dp1 dp1 dp2))
(define w2 (- width dw2))
(define h2 50)

; grid definition*
(define N 1000)
(define grid (make-grid N))

; grid representation, position and number of represented rows/columns
(define gx0 (quotient N 2))
(define gy0 (quotient N 2))

; manage mouse dragging to create an area
(define dgx0 0)
(define dgy0 0)
(define dgx1 1)
(define dgy1 1)
(define dragging #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (select-area x0 y0 x1 y1)
  (for ([i (range x0 (+ 1 x1))])
    (for ([j (range y0 (+ 1 y1))])
      (cell-set! grid i j 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (grid-mouse-event event x y)
  (let ((gx (inexact->exact (+ gx0 (quotient x dcell))))
        (gy (inexact->exact (+ gy0 (quotient y dcell)))))
    ; simple clic gauche
    (when (send event button-down? 'left)
      (if (> (cell grid gx gy) 0) 
          (cell-set! grid gx gy 0)
          (cell-set! grid gx gy 1)))
    ; zone "dragging"
    (if (send event dragging?)
        (if (not dragging)
            (begin
              (set! dragging #t)
              (set! dgx0 gx)
              (set! dgy0 gy))
            (begin
              (set! dgx1 gx)
              (set! dgy1 gy)))
        (when dragging
          (set! dragging #f)
          (select-area dgx0 dgy0 dgx1 dgy1))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (grid-char-event event x y)
  (let ((key (send event get-key-code)))
    (cond
      [(equal? key 'down) (when (<= gy0 (- N 2)) (set! gy0 (+ gy0 1)))]
      [(equal? key 'up) (when (> gy0 0) (set! gy0 (- gy0 1)))]
      [(equal? key 'left) (when (> gx0 0) (set! gx0 (- gx0 1)))]
      [(equal? key 'right) (when (<= gx0 (- N 2)) (set! gx0 (+ gx0 1)))])
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; drawing area list
(define da1 (drawing-area "grid" dh1 dh1 h1 h1 dp1 dp1 draw-grid-area grid-mouse-event grid-char-event))
(define da2 (drawing-area "status" dw2 dh2 w2 h2 dp2 dp2 draw-status-area '() '()))
(set-drawing-area-list (list da1 da2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;
(define (paint-canvas canvas dc)
  (send dc set-scale 1 1)
  ; set default pen and brushes
  (send dc set-smoothing 'aligned)
  (send dc set-pen red-pen)
  (send dc set-brush no-brush)
  ; draw all areas
  (draw-area-list dc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; canvas drawing function
(define cv (create-canvas tw drawing-area-on-mouse-event drawing-area-on-char-event paint-canvas))
(send cv  set-canvas-background [make-object color% "black"])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 
(new timer%	 
     [notify-callback 
      (lambda () 
        (send cv refresh-now))
      ]	 
     [interval 100]	 
     [just-once? #f])



