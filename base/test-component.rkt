#lang racket

(require rackunit rackunit/gui
         "component.rkt")
;
(provide component-test-suite)

; component creation
(define (component nid tid parent)
  (new component% [the-nid nid] [the-tid tid] [the-parent parent]))


;; automata tests
(define component-test-suite
  (test-suite 
   "test-component"
   ;
   (test-case "component creation"
              (let ((c (component)))
                (check-equal? #t #t)))))
