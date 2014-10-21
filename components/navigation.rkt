#lang racket


; 
(require "../base/component.rkt")

;
(provide navigation%)

; definition of a engineering sub part of platform
(define navigation%
  (class component% 
    
    ; initialization arguments ()
    (init nid tid)                
    
    ; fields ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; superclass initialization
    (super-new [the-nid nid] [the-tid tid])                
    
    ; methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ))
