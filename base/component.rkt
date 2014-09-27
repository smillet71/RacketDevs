#lang racket 

;
(provide component%)

; definition of a simulation component
(define component%
  (class object% 
    
    ; initialization arguments ( numerical id / text id / parent component )
    (init the-nid the-tid the-parent)                
    
    ; fields ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define nid the-nid)
    (define tid the-tid)
    (define parent the-parent)
    (define children '())
    
    ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; superclass initialization
    (super-new)                
    
    ; methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define/public (get-nid) nid)
    (define/public (get-tid) tid)
    (define/public (get-parent) parent)
    (define/public (get-children) children)
    (define/public (get-child id) '())
    
    ; receive msg by databus
    (define/public (receive topic msg) '())
    
    ))
