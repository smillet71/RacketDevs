#lang racket

(require rackunit rackunit/gui
         "databus.rkt")
;
(provide databus-test-suite)

; component creation
(define (databus)
  (new databus%))


;; automata tests
(define databus-test-suite
  (test-suite 
   "test-databus"
   ;
   (test-case "databus creation"
              (let ((c (databus)))
                (check-equal? #t #t)))))
