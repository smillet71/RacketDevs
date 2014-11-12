 #lang racket

(require rackunit
         "room.rkt")
;
(provide room-test-suite)

;; room tests
(define room-test-suite
  (test-suite 
   "test-room"
   ;
   (test-case "room tests"
              '())))
