#lang racket 

; definition d'un automate à états
(define automata%
  (class object% 
    
    ; initialization arguments (possible states, initial states, final states)
    (init the-states the-initial-state the-final-states)                
    
    ; fields ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ; list of the possible states
    (define states the-states)
    ; initial state (when starting)
    (define initial-state                              
      (if (member the-initial-state states)           
          the-initial-state
          (error "instantiate: initial state can't be part of final states")))
    ; final states 
    (define final-states 
      (begin
        (when (member initial-state the-final-states)
          (error "instantiate: final state can not be part of possible states"))
        (map (lambda (state) 
               (if  (member state states)
                    state
                    (error "instantiate: final state is not part of possible states"))) the-final-states)))
    
    ; current state of the automata
    (define current-state the-initial-state)     
    ; liste des actions à effectuer sur un état
    (define on-state-actions (make-hash))
    ; liste des actions à effectuer en arrivant sur un état
    (define before-state-actions (make-hash))
    ; liste des actions à effectuer en sortant d'un état
    (define after-state-actions (make-hash))
    ; liste des transitions entre états
    (define transitions (make-hash))
    
    ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; superclass initialization
    (super-new)                
    
    ; methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ; is part of possible states
    (define/public (is-state? state) (member state states))
    ; get state list
    (define/public (get-states) states)
    ; get initial state
    (define/public (get-initial-state) initial-state)
    ; get final states
    (define/public (get-final-states) final-states)
    ; get current state
    (define/public (get-current-state) current-state)
    
    ; add an action associated to a state
    (define/private (add-action state action action-list)
      (when (not (procedure? action))
        (error "add-action: action must be a procedure"))
      (when (not (equal? (procedure-arity action) 1))
        (error "add-action: action must be a procedure with 1 argument"))
      (if (is-state? state) 
          (if (hash-has-key? action-list state)
              (hash-set! action-list state (cons (hash-ref action-list state) action))
              (hash-set! action-list state (list action)))
          (error "add-action: state is not part of possible states")))
    ; add an on-state action
    (define/public (add-action-on-state state action)
      (add-action state action on-state-actions))
    ; add a before-state action
    (define/public (add-action-before-state state action)
      (add-action state action before-state-actions))
    ; add an after-state action
    (define/public (add-action-after-state state action)
      (add-action state action after-state-actions))
    
    ; get on-state actions
    (define/public (get-on-state-actions) on-state-actions)
    ; get before-state actions
    (define/public (get-before-state-actions) before-state-actions)
    ; get after-state actions
    (define/public (get-after-state-actions) after-state-actions)
    
    ))

;;; tests
(define a (new automata% [the-states '(red green blue)] [the-initial-state 'red] [the-final-states '(blue)]))
(send a add-action-on-state 'blue (lambda (x) '()))
(send a get-on-state-actions)