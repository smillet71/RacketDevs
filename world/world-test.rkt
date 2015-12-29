#lang racket

(require "world.rkt")

;; ------------------------------------------
;; some tests
;; ------------------------------------------
(define w (new world%))
(define e1 (new entity%
                [ etype 'static ] [ esubtype 'marker ]
                [ eposition (new world-position%)] [ ebbox (new entity-bounding-box%)]
                [ the-world w]))

(send e1 add-to-world)

(send w init)
