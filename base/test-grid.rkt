#lang racket

(require rackunit
         "grid.rkt")
;
(provide grid-test-suite)

; grid tests
(define grid-test-suite
  (test-suite 
   "test-grid"
   ;
   (test-case "grid tests"
              (let ((a (make-grid 10)))
                (check-equal? (cell a 1 1) 0)
                (cell-set! a 1 1 2)
                (check-equal? (cell a 1 1) 2)))))
