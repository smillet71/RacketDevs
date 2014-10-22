#lang racket/gui

(provide create-gui create-canvas)

; Basic frame creation
(define gui%
  (class frame%
    (super-new [label "test-gui"] 
               ;[style (list 'no-caption 'no-system-menu)] 
               [style (list 'no-resize-border)] 
               [min-width 500] 
               [min-height 500])
    (define/augment (on-close) 
      (let ((choice (get-choices-from-user "Exit application ?" "Do you confirm you want to quit the application ?" (list "Yes" "No"))))
        (when (equal? choice '(0))
          (exit))))))

; frame creation and initial state
(define (create-gui)
  (let ((tw (new gui%)))
    (send tw maximize #t)
    (send tw show #t)
    (send tw fullscreen #t)
    tw))

; Basic canvas creation 
(define my-canvas%   
  (class canvas% 
    
    ; 
    (init mouse-callback char-callback )
    
    ; 
    (define the-mouse-callback mouse-callback)
    (define the-char-callback char-callback)
    
    ; Define overriding method to handle mouse events     
    (define/override (on-event event)       
      (when (not (null? the-mouse-callback))
        (the-mouse-callback event)))
    
    ; Define overriding method to handle keyboard events     
    (define/override (on-char event)       
      (when (not (null? the-char-callback))
        (the-char-callback event)))

    ; Call the superclass init, passing on all init args     
    (super-new)))   

; canvas creation
(define (create-canvas the-parent mousecb charcb paintcb)
  (let ((canvas (new my-canvas% 
                     [mouse-callback mousecb] 
                     [char-callback charcb] 
                     [parent the-parent]
                     [paint-callback paintcb]
                     )))
    canvas))