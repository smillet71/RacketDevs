#lang racket


; 
(require "../base/component.rkt")

;
(provide platform%)

; definition of a engineering sub part of platform
(define platform%
  (class component% 
    
    ; initialization arguments ()
    (init nid tid)                
    
    ; fields ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; position 3d + attitudes (4d quaternion)
    (define pose (make-vector 7 0))
    ; velocity linear (3d) + angular (3d)
    (define velocity (make-vector 6 0))
    
    ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; superclass initialization
    (super-new [the-nid nid] [the-tid tid])                
    
    ; methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ))
