#lang racket

(require rackunit rackunit/gui
         "automata.rkt")


; automata creation
(define (automaton states istate fstates)
  (new automata% [the-states states] [the-initial-state istate] [the-final-states fstates]))

;;; test-dependancies
(define (action-fail) '())
(define (action-fail-2 x y) '())
(define (action-fail-3 x y z) '())
(define (action-ok x) '())
(define (condition-ok x) '())
(define (condition-fail) '())

;; automata tests
(test/gui 
 (test-suite 
  "test-automata"
  
  (test-case "automata creation"
             (let ((a (automaton '(red green blue) 'red '(green))))
               (check-equal? (send a get-current-state) 'red)
               (check-equal? (send a get-initial-state) 'red)
               (check-equal? (send a get-final-states) '(green))))
  
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
  
  (test-case "add an inadequate action on a state"
             (let ((a (automaton '(red green blue) 'red '(green))))
               (check-exn exn:fail? 
                          (lambda () (send a add-action-on-state 'red action-fail)))
               (check-exn exn:fail? 
                          (lambda () (send a add-action-on-state 'red action-fail-2)))
               (check-exn exn:fail? 
                          (lambda () (send a add-action-on-state 'red action-fail-3)))
               ))
  
  (test-case "add an action on an existing state"
             (let ((a (automaton '(red green blue) 'red '(green))))
               (check-not-exn (lambda () (send a add-action-on-state 'red action-ok)))
               (check-equal? 1 (length (send a get-on-state-actions 'red)))
               (check-equal? 0 (length (send a get-on-state-actions 'green)))
               (check-equal? 0 (length (send a get-on-state-actions 'blue)))
               (check-exn exn:fail? 
                          (lambda () (send a get-on-state-actions 'yellow)))
               ))
  
  (test-case "add an action before an existing state"
             (let ((a (automaton '(red green blue) 'red '(green))))
               (check-not-exn (lambda () (send a add-action-before-state 'green action-ok)))
               (check-equal? 1 (length (send a get-before-state-actions 'green)))
               (check-equal? 0 (length (send a get-before-state-actions 'red)))
               (check-equal? 0 (length (send a get-before-state-actions 'blue)))
               (check-exn exn:fail? 
                          (lambda () (send a get-before-state-actions 'yellow)))
               ))
  
  (test-case "add an action after an existing state"
             (let ((a (automaton '(red green blue) 'red '(green))))
               (check-not-exn (lambda () (send a add-action-after-state 'blue action-ok)))
               (check-equal? 1 (length (send a get-after-state-actions 'blue)))
               (check-equal? 0 (length (send a get-after-state-actions 'red)))
               (check-equal? 0 (length (send a get-after-state-actions 'green)))
               (check-exn exn:fail? 
                          (lambda () (send a get-after-state-actions 'yellow)))
               ))
  
  (test-case "add a transition between 2 states"
             (let ((a (automaton '(red green blue) 'red '(green))))
               (check-not-exn(lambda () (send a add-transition 'red 'green condition-ok)))
               (check-exn exn:fail? 
                          (lambda () (send a add-transition 'yellow 'green condition-ok)))
               (check-exn exn:fail? 
                          (lambda () (send a add-transition 'red 'yellow condition-ok)))
               (check-exn exn:fail? 
                          (lambda () (send a add-transition 'green 'red condition-ok)))
               (check-exn exn:fail? 
                          (lambda () (send a add-transition 'green 'red condition-fail)))
               (check-exn exn:fail? 
                          (lambda () (send a add-transition 'green 'red '())))
               (check-equal? 1 (hash-count (send a get-transitions-from 'red)))
               (check-equal? '() (send a get-transitions-from 'green))
               (check-equal? '() (send a get-transitions-from 'blue))
               ))
  ))
