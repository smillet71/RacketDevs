#lang racket

(require rackunit rackunit/gui
         "automata.rkt")


; automata creation
(define (automaton states istate fstates)
  (new automata% [the-states states] [the-initial-state istate] [the-final-states fstates]))

;;; tests
(define a (new automata% [the-states '(red green blue)] [the-initial-state 'red] [the-final-states '(blue)]))
(send a add-action-on-state 'blue (lambda (x) '()))
(send a get-on-state-actions)

;; automata tests
(test/gui 
 (test-suite 
  "test-automata"
  
  (test-case "automata creation"
             (check-true (is-a? (automaton '(red green blue) 'red '(green)) automata%)))
  
  (test-case "automata creation failure on initial state does not exist"
             (check-exn exn:fail? 
                        (lambda () (automaton '(red green blue) 'yellow '(green)))))
  
  (test-case "automata creation failure on final state does not exist"
             (check-exn exn:fail? 
                        (lambda () (automaton '(red green blue) 'red '(green yellow)))))
  
  (test-case "automata creation failure on final state does not exist"
             (check-exn exn:fail? 
                        (lambda () (automaton '(red green blue) 'red '(green yellow)))))
  
  (test-case "automata creation failure on final state equal to initial state"
             (check-exn exn:fail? 
                        (lambda () (automaton '(red green blue) 'red '(red)))))
  
  (test-case "Another test"
             check-eq? 1 1))
 )
