#lang racket

(require "world.rkt")

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
                [ eposition (new world-position% [x 100] [y 1000] [vy -0.05])] [ ebbox (new entity-bounding-box%)]
                [ the-world w]))

;
(define e3 (new entity%
                [ etype 'ship ] [ esubtype 'cargo ]
                [ eposition (new world-position% [x 300] [y -2000] [vy 0.01])] [ ebbox (new entity-bounding-box%)]
                [ the-world w]))

;
(send w init)
