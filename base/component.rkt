#lang racket 

;
(provide component
         add-child
         remove-child
         get-child
         get-parent
         send-message
         tick
         component%)

; definition of a simulation component
(define component%
  (class object% 
    
    ; initialization arguments ( numerical id / text id / parent component )
    (init the-nid the-tid)                
    
    ; fields ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define nid the-nid)
    (define tid the-tid)
    (define parent '())
    (define children (make-hash))
    (define tick-sema (make-semaphore 1))
    (define msg-queue-wr (make-hash))
    (define msg-queue-rd (make-hash))
    
    ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; superclass initialization
    (super-new)                
    
    ; get ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define/public (get-nid) nid)
    (define/public (get-tid) tid)
    (define/public (get-parent) parent)
    (define/public (get-children) children)
    (define/public (get-child id) 
      (if (hash-has-key? children id)
          (hash-ref children id)
          '()))
    ; should be used from update-to !!
    (define/public (get-topic topic)
      (if (hash-has-key? msg-queue-rd topic)
          (hash-ref msg-queue-rd topic)
          '()))
    
    ; set ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ; set the parent of this component
    (define/public (remove-parent) 
      (when (not (null? parent))
        (send parent remove-child this)
        (set! parent '())))
    
    ; set the parent of this component
    (define/public (remove-child c) 
      (let ((nid (send c get-nid)))
        (when (hash-has-key? children nid)
          (hash-remove! children nid)
          (send c remove-parent))))
    
    ; set the parent of this component
    (define/public (set-parent p)
      (set! parent p))
    
    ; add a child to this comp
    (define/public (add-child c)
      (when (not (is-a? c component%))
        (error "component:add-child: arg should be a component"))
      (when (not (null? (send c get-parent)))
        (error "component:add-child: child should not have a parent"))
      (send c set-parent this)
      (hash-set! children (send c get-nid) c))
    
    ; before 
    (define/public (before-tick)
      (semaphore-wait tick-sema)
      (let ((memo msg-queue-wr))
        (set! msg-queue-wr msg-queue-rd)
        (set! msg-queue-rd memo))
      (semaphore-post tick-sema))
    
    (define/public (after-tick)
      (hash-clear! msg-queue-rd))
    
    ; update to particular time
    (define/public (tick t)
      (before-tick)
      (map (lambda (component) 
             (send component tick t)) 
           children)
      (update-to t)
      (after-tick))
    
    ; receive msg by databus
    (define/public (receive topic msg) 
      (semaphore-wait tick-sema)
      (if (hash-has-key? msg-queue-wr topic)
          (let ((msg-list (hash-ref msg-queue-wr topic)))
            (hash-set! msg-queue-wr topic (cons msg msg-list)))
          (hash-set! msg-queue-wr topic (list msg)))
      (semaphore-post tick-sema))

    ; update current state to time T
    (define/public (update-to t) '())
    
    ))

;
(define (add-child c1 c2)
  (send c1 add-child c2))
;
(define (remove-child c1 c2)
  (send c1 remove-child c2))

;
(define (get-child c1 nid)
  (send c1 get-child nid))

;
(define (get-parent c1)
  (send c1 get-parent))

;
(define (send-message c1 topic msg)
  (send c1 receive topic msg))

;
(define (tick c1 t)
  (send c1 tick t))

; component creation
(define (component nid tid)
  (new component% [the-nid nid] [the-tid tid]))
