#lang racket/gui

(provide gui%)

; 
(define gui%
  (class frame%
    ;
    
    ;
    (super-new [label "test-gui"] [style (list 'no-caption 'no-system-menu)])))

(define tw (new gui% ))
(send tw maximize #t)
(send tw show #t)
(send tw fullscreen #t)