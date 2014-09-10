#lang racket/gui

; Make a frame by instantiating the frame% class 
(define frame 
  (new frame% 
       [label "Example"]
       [style (list 'no-caption 'no-system-menu)]
       ))   

; Make a static text message in the frame 
(define msg (new message% [parent frame]                           
                 [label "No events so far..."]))   
; Make a button in the frame 
(new button% [parent frame]              
     [label "Click Me"]              
     ; Callback procedure for a button click:              
     [callback (lambda (button event)                          
                 (send msg set-label "Button click"))])   

(define my-canvas%   
  (class canvas% 
    ; The base class is canvas%     
    ; Define overriding method to handle mouse events     
    (define/override (on-event event)       
      (send msg set-label "Canvas mouse"))     
    ; Define overriding method to handle keyboard events     
    (define/override (on-char event)       
      (send msg set-label "Canvas keyboard"))
   
    ; Call the superclass init, passing on all init args     
    (super-new)))   

; Make a canvas that handles events in the frame 
(new my-canvas% 
     [parent frame]
     [paint-callback               
      (lambda (canvas dc)                 
       (send dc set-scale 3 3)                 
       (send dc set-text-foreground "blue")                 
       (send dc draw-text "Don't Panic!" 0 0))]) 

; Show the frame by calling its show method 
(send frame maximize #t)
(send frame fullscreen #t)
(send frame show #t) 

