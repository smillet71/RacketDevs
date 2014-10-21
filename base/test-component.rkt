#lang racket

(require rackunit rackunit/gui
         "component.rkt")
;
(provide component-test-suite)

; 
(define count 0)
(define received-msg '())

;
; definition of a simulation component
(define test-component%
  (class component% 
    
    ; initialization arguments ( numerical id / text id / parent component )
    (init nid)                
    
    ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; superclass initialization
    (super-new [the-nid nid] [the-tid "noname"])                
    
    ; receive msg by databus
    (define (update-to t) 
      '())
    (override update-to)
    
    ))

; component creation
(define (test-component n)
  (new test-component% [nid n]))


;; automata tests
(define component-test-suite
  (test-suite 
   "test-component"
   
   ;
   (test-case "component creation"
              (let ((c1 (component 1001 'c1001 ))
                    (c2 (component 1002 'c1002 ))
                    (c3 (component 1003 'c1003 ))
                    (c4 (component 1004 'c1004 )))
                ;
                (check-equal? (hash-count (send c1 get-children)) 0)
                (add-child c1 c2)
                (check-equal? (get-parent c2) c1)
                (check-equal? (hash-count (send c1 get-children)) 1)
                (add-child c1 c3)
                (check-equal? (get-parent c3) c1)
                (check-equal? (hash-count (send c1 get-children)) 2)
                (check-equal? (get-child c1 1002) c2)
                (check-equal? (get-child c1 1003) c3)
                (check-equal? (get-child c1 1004) '())
                (remove-child c1 c2)
                (check-equal? (hash-count (send c1 get-children)) 1)
                (check-equal? (get-child c1 1002) '())
                (check-equal? (get-parent c2) '())
                ))
   ; 
   (test-case "component msg queues"
              (let ((c1 (test-component 1001)))
                (send-message c1 'topic1 'msg1)
                (send-message c1 'topic1 'msg2)
                (send-message c1 'topic2 'msg3)
                (check-equal? (length (send c1 get-topic 'topic1)) 0)
                (send c1 before-tick)
                (check-equal? (length (send c1 get-topic 'topic1)) 2)
                (check-equal? (send c1 get-topic 'topic1) '(msg2 msg1))
                (check-equal? (length (send c1 get-topic 'topic2)) 1)
                (send c1 after-tick)
                (check-equal? (length (send c1 get-topic 'topic1)) 0)
                (check-equal? (length (send c1 get-topic 'topic2)) 0)
                ))))


