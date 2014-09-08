#lang racket 

;
(provide automata%)

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
    ; previous state 
    (define previous-state initial-state)
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
    
    ; gestion des action sur les états
    
    ; add an action to a state (before, on, after)
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
      (when (equal? initial-state state)
        (error "add-action-before-state: can't add an action to be executed before initial state"))
      (add-action state action before-state-actions))
    ; add an after-state action
    (define/public (add-action-after-state state action)
      (when (member state final-states)
        (error "add-action-after-state can't add an action to be executed after a final state"))
      (add-action state action after-state-actions))
    
    ; get state actions for a particular state
    (define/private (get-state-actions state action-list)
      (if (is-state? state) 
          (if (hash-has-key? action-list state)          
              (hash-ref action-list state)
              '())
          (error "get-state-actions: state is not part of possible states")))
    
    ; get on state actions for a particular state
    (define/public (get-on-state-actions state)
      (get-state-actions state on-state-actions))
    ; get before state actions for a particular state
    (define/public (get-before-state-actions state)
      (get-state-actions state before-state-actions))
    ; get after state actions for a particular state
    (define/public (get-after-state-actions state)
      (get-state-actions state after-state-actions))
    
    ; transitions' management 
    
    ; add a transition between 2 states
    (define/public (add-transition from-state to-state on-condition)
      (when (not (procedure? on-condition))
        (error "add-transition: action must be a procedure"))
      (when (not (equal? (procedure-arity on-condition) 1))
        (error "add-transition: action must be a procedure with 1 argument"))
      (if (and (is-state? from-state) (is-state? to-state) (not (member from-state final-states)))
          (if (hash-has-key? transitions from-state)
              (let ((h (hash-ref transitions from-state)))
                (hash-set! h to-state on-condition))
              (let ((h (make-hash)))
                (hash-set! h to-state on-condition)
                (hash-set! transitions from-state h)))
          (error "add-transition: incorrect arguments")))
    
    ; access to defined transitions
    (define/public (get-transitions-from state)
      (if (is-state? state) 
          (if (hash-has-key? transitions state)          
              (hash-ref transitions state)
              '())
          (error "get-transitions-from: state is not part of possible states")))
    
    ; execution management
    
    ; force a particular state
    (define/public (force-state state) 
      (if (is-state? state) 
          (begin (set! current-state state) 
                 (set! previous-state state))
          (error "get-transitions-from: state is not part of possible states")))
    
    ; force initial state
    (define/public (init) 
      (set! current-state initial-state) 
      (set! previous-state initial-state))
    
    ; execute actions before/on/after a state
    (define/private (execute-actions context state-actions)
      (when (hash-has-key? state-actions current-state)          
        (let ((list-of-actions (hash-ref state-actions current-state)))
          (for ([action list-of-actions])
            (action context)))))
    
    ; execute actions associated with current state test transitions
    (define/public (step context) 
      ; execute before state actions ?
      (when (not (equal? previous-state current-state))
        (execute-actions context on-state-actions)
        (set! previous-state current-state))
      ; execute actions on current state
      (execute-actions context on-state-actions)
      ; test transitions
      (let ((list-transitions (get-transitions-from current-state))
            (finished #f))
        (when (not (null? list-transitions))
          (for ([ transition (hash->list list-transitions)] #:when (not finished))
            (let* ((to-state (car transition))
                   (condition (cdr transition))
                   (transition-ok (condition context)))
              (when transition-ok
                (begin
                  (execute-actions context after-state-actions)
                  (set! previous-state current-state)
                  (set! current-state to-state)
                  (set! finished #t))))))) 
      )
    ))
