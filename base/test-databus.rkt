 #lang racket

(require rackunit
         "component.rkt"
         "databus.rkt")
;
(provide databus-test-suite)

; 
(define count 0)
(define received-msg '())

;
; definition of a simulation component
(define test-component%
  (class component% 
    
    ; initialization arguments ( numerical id / text id / parent component )
    (init nid )                
  
    ; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; superclass initialization
    (super-new [the-nid nid] [the-tid "noname"] [the-parent '()])                
    
    ; receive msg by databus
    (define (receive topic msg) 
      (set! count (+ 1 count))
      (set! received-msg msg))
    (override receive)
    
    ))

; component creation
(define (test-component n)
  (new test-component% [nid n]))

; component creation
(define (databus)
  (new databus%))


;; automata tests
(define databus-test-suite
  (test-suite 
   "test-databus"
   ;
   (test-case "databus tests"
              (let ((c1 (test-component 1001))
                    (c2 (test-component 1002))
                    (dtb (databus)))
                (check-equal? count 0)
                (send dtb subscribe c1 'test-topic-2)
                (send dtb send-msg 'test-topic-1 'hello)
                (check-equal? count 0)
                (check-equal? received-msg '())
                (send dtb send-msg 'test-topic-2 'hello)
                (check-equal? count 1)
                (check-equal? received-msg 'hello)
                (send dtb subscribe c2 'test-topic-2)
                (send dtb send-msg 'test-topic-2 'hello)
                (check-equal? count 3)
                (send dtb unsubscribe c2 'test-topic-2)
                (send dtb send-msg 'test-topic-2 'hello)
                (check-equal? count 4)
                (check-equal? (send dtb has-topic 'test-topic-2) #t)
                (send dtb unsubscribe c1 'test-topic-2)
                (send dtb send-msg 'test-topic-2 'hello)
                (check-equal? count 4)
                (check-equal? (send dtb has-topic 'test-topic-2) #f)
                ))))
