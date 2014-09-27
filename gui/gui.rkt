#lang racket/gui

(provide gui%)

; 
(define gui%
  (class frame%
    
    ;
    (super-new [label "test-gui"] [style (list 'no-caption 'no-system-menu)] [min-width 500] [min-height 500])))

;
(define tw (new gui% ))
(send tw maximize #t)
(send tw show #t)
(send tw fullscreen #t)

;
(define-values (width height) (send tw  get-client-size))
(define h (* (floor (/ height 100.0)) 100.0))
(define dh (floor (/ (- height h) 2)))

;
(define no-pen (new pen% [style 'transparent]))
(define no-brush (new brush% [style 'transparent]))
(define blue-brush (new brush% [color "blue"]))
(define yellow-brush (new brush% [color "yellow"]))
(define red-pen (new pen% [color "red"] [width 2]))

(define i 100)
(define j 50)

(define (get-i) i)
(define (get-j) j)

(define tt '())

;
(define cv 
  (new canvas% 
       [parent tw]
       [paint-callback
        (lambda (canvas dc)
          
          (send dc set-smoothing 'aligned)
          (send dc set-pen red-pen)
          (send dc set-brush no-brush)
          (for ([a (range 20)])
            (for ([ b (range 20)])
              (send dc draw-point (* a 50) (* b 50))))
          (send dc draw-rectangle dh dh h h)
          (send dc set-brush blue-brush)
          (for ([x (range 5)])
            (for ([ y (range 15)])
              (send dc draw-rectangle 
                    (+ (get-i) (* x 20))
                    (+ (* y 50) (get-j)) 
                    10 10))))
            ])
)

(send cv  set-canvas-background [make-object color% "black"])


(new timer%	 
     [notify-callback 
      (lambda () 
        (set! i (+ 1 i))
        (send cv refresh-now))
      ]	 
     [interval 50]	 
     [just-once? #f])


