#lang racket

(require rackunit rackunit/gui
         "world.rkt")

(require math)
;
(provide world-test-suite)

;; ------------------------------------------
;; some tests
;; ------------------------------------------

;
(define w (new world%))

;
(define e1 (new entity%
                [ etype 'struct ] [ esubtype 'beacon ]
                [ eposition (new world-position%)] [ ebbox (new entity-bounding-box%)]
                [ the-world w]))

;
(define e2 (new entity%
                [ etype 'ship ] [ esubtype 'scout ]
                [ eposition (new world-position% [x 100.0] [y 1000.0] [ z 500.0] [vx 1.0] [vy -0.05] [vz 2.0]
                                 [psi 180.0] [theta 10.0] [phi 20.0])] [ ebbox (new entity-bounding-box%)]
                [ the-world w]))

;
(define e3 (new entity%
                [ etype 'ship ] [ esubtype 'cargo ]
                [ eposition (new world-position% [x 300] [y -2000] [vy 0.01])] [ ebbox (new entity-bounding-box%)]
                [ the-world w]))

; 
(define e4 (new entity%
                [ etype 'ship ] [ esubtype 'cargo ]
                [ eposition (new world-position%)] [ ebbox (new entity-bounding-box%)]
                [ the-world w]))

(define m1 (new entity-mover% [entity e4]))

;
;(send w init)

;; world tests
(define world-test-suite
 (test-suite 
  "test-world"
  
  (test-case "world and entities creation"
             (begin
               (check-equal? (hash-count (send w get-entities)) 4)
               (check-equal? (send e1 entity-type) 'struct)
               (check-equal? (send e1 entity-subtype) 'beacon)
               (check-equal? (send e1 state) 'init)
               (let ([pos (send (send e2 position) position)]
                     [spd (send (send e2 position) speed)]
                     [att (send (send e2 position) attitudes)])
                 (check-equal? (array-ref pos #(0)) 100.0) 
                 (check-equal? (array-ref pos #(1)) 1000.0) 
                 (check-equal? (array-ref pos #(2)) 500.0) 
                 (check-equal? (array-ref spd #(0)) 1.0) 
                 (check-equal? (array-ref spd #(1)) -0.05) 
                 (check-equal? (array-ref spd #(2)) 2.0) 
                 (check-equal? (array-ref att #(0)) 180.0) 
                 (check-equal? (array-ref att #(1)) 10.0) 
                 (check-equal? (array-ref att #(2)) 20.0)) 
               (let ([pos (send (send e3 position) position)]
                     [spd (send (send e3 position) speed)]
                     [att (send (send e3 position) attitudes)])
                 (check-equal? (array-ref pos #(2)) 0.0) 
                 (check-equal? (array-ref spd #(0)) 0.0) 
                 (check-equal? (array-ref spd #(1)) 0.01) 
                 (check-equal? (array-ref spd #(2)) 0.0) 
                 (check-equal? (array-ref att #(0)) 0.0) 
                 (check-equal? (array-ref att #(1)) 0.0) 
                 (check-equal? (array-ref att #(2)) 0.0))))
  (test-case "entities initialization"
             (begin
               (send w init)
               (check-equal? (hash-count (send w get-entities)) 4)
               (check-equal? (send e1 state) 'run)
               (check-equal? (send e1 last-time) 0)
               (check-equal? (send e1 id) 100)
               (check-equal? (send e2 id) 101)
               (check-equal? (send e3 id) 102)
               (check-equal? (send e4 id) 103)
               (check-equal? (send w ids) 104)
               ))
  (test-case "integration 1"
             (let* ([epos (send e4 position)])
               (begin
                 ; initialize
                 (send epos set-position (array #[0.0 0.0 0.0]))
                 (send epos set-speed (array #[0.0 0.0 0.0]))
                 (send epos set-attitudes (array #[0.0 0.0 0.0]))
                 ; integrate
                 (send m1 move 1 (array #[0 0 0]) 0.0)
                 ;
                 (check-equal? (array-ref (send epos position) #(0)) 0.0)
                 (check-equal? (array-ref (send epos position) #(1)) 0.0)
                 (check-equal? (array-ref (send epos position) #(2)) 0.0)
               )))
  (test-case "integration 2"
             (let* ([epos (send e4 position)])
               (begin
                 ; initialize
                 (send epos set-position (array #[0.0 0.0 0.0]))
                 (send epos set-speed (array #[1.0 2.0 3.0]))
                 (send epos set-attitudes (array #[0.0 0.0 0.0]))
                 ; integrate
                 (send m1 move 1 (array #[0 0 0]) 0.0)
                 ;
                 (check-equal? (array-ref (send epos position) #(0)) 1.0)
                 (check-equal? (array-ref (send epos position) #(1)) 2.0)
                 (check-equal? (array-ref (send epos position) #(2)) 3.0)
               ))
             (let* ([epos (send e4 position)])
               (begin
                 ; initialize
                 (send epos set-position (array #[0.0 0.0 0.0]))
                 (send epos set-speed (array #[0.0 0.0 0.0]))
                 (send epos set-attitudes (array #[0.0 0.0 0.0]))
                 ; integrate
                 (send m1 move 1 (array #[1 -1 2]) 0.0)
                 ; verify
                 (check-equal? (array-ref (send epos position) #(0)) 0.5)
                 (check-equal? (array-ref (send epos position) #(1)) -0.5)
                 (check-equal? (array-ref (send epos position) #(2)) 1.0)
                 (check-equal? (array-ref (send epos speed) #(0)) 1.0)
                 (check-equal? (array-ref (send epos speed) #(1)) -1.0)
                 (check-equal? (array-ref (send epos speed) #(2)) 2.0)
                 ; integrate
                 (send m1 move 1 (array #[1 -1 0]) 0.0)
                 ; verify
                 (check-equal? (array-ref (send epos position) #(0)) 2.0)
                 (check-equal? (array-ref (send epos position) #(1)) -2.0)
                 (check-equal? (array-ref (send epos position) #(2)) 3.0)
                 (check-equal? (array-ref (send epos speed) #(0)) 2.0)
                 (check-equal? (array-ref (send epos speed) #(1)) -2.0)
                 (check-equal? (array-ref (send epos speed) #(2)) 2.0)
               )))               
  ))