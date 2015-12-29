#lang racket
(require math)

(provide world-position%
         entity-bounding-box%
         entity%
         world%)

;; ------------------------------------------
;; position, attitudes et vitesse d'un objet
;; ------------------------------------------
(define world-position%
  (class object%
    ; initialization
    (super-new)
    (init-field [x 0.0] [y 0.0] [z 0.0]
                [vx 0.0] [vy 0.0] [vz 0.0]
                [psi 0.0] [theta 0.0] [phi 0.0]
                [m 1])
    ; accessors
    (define/public (position) (array #[ x y z ] ))
    (define/public (attitudes) (array #[ psi theta phi ]))
    (define/public (mass) m)
    (define/public (speed) (array #[ vx vy vz ] ))
    ; setters
    (define/public (set-position nx ny nz)
      (set! x nx)
      (set! y ny)
      (set! z nz))
    (define/public (set-speed nvx nvy nvz)
      (set! vx nvx)
      (set! vy nvy)
      (set! vz nvz))
    (define/public (set-attitudes npsi ntheta nphi)
      (set! psi npsi)
      (set! theta ntheta)
      (set! phi nphi))
      ))

;; ------------------------------------------
;; bounding box of an entity
;; ------------------------------------------
(define entity-bounding-box%
  (class object%
    ; initialization
    (super-new)
    (init-field [x 1.0] [y 1.0] [z 1.0])
    ; accessors
    (define/public (box) (array #[ x y z ] ))))

;; ------------------------------------------
;; definition of an object living in the simulated world
;; ------------------------------------------
(define entity%
  (class object%
    ; initialization
    (super-new)
    (init-field etype esubtype eposition ebbox the-world)
    (field [ t 0] ; date
           [ eid 0 ] ; entity state id
           [ estate 'init ]) ; entity state, 'init 'run 'end)
    ; accessors
    (define/public (id) eid)
    (define/public (entity-type) etype) ; 'static or 'dynamic
    (define/public (entity-subtype) esubtype)
    (define/public (position) eposition)
    (define/public (bbox) ebbox)
    (define/public (state) estate)
    (define/public (set-id the-eid) (set! eid the-eid))

    ;; add this object to the world 
    (define/public (add-to-world)
      (send the-world add-entity this))

    ;; initialize 
    (define/public (initialize the-date)
      (set! t the-date)
      (set! estate 'run))
      
    ;;
    (define/public (update current-time delta-t) '())
     ))


;; ------------------------------------------
;; definition of the simulated world
;; ------------------------------------------
(define world%
  (class object%
    ; initialization
    (super-new)
    (init-field [wname 'the-world] [wid 0] [wseed 0] [wt 0] [cadence-hz 20])
    (field [ estate 'init ]                 ; world state, 'init 'run 'end)
           [ static-entities (make-hash) ]  ; hashtable of static entities
           [ dynamic-entities (make-hash) ] ; hashtable of dynamic entities
           [ eids 100]
           )
    ; accessors
    (define/public (name) wname)
    (define/public (id) wid)
    (define/public (seed) wseed)
    (define/public (time) wt)
    (define/public (ids) eids)
    ; methods
    (define/public (add-entity e)
      (send e set-id eids)
      (set! eids (+ eids 1))
      (if (equal? (send e entity-type) 'static)
          (hash-set! static-entities (send e id) e)
          (hash-set! dynamic-entities (send e id) e)))
    
    ; initialize all objects added to world
    (define/public (init)
      ;
      (writeln (hash-count static-entities))
      (for ([key (hash-keys static-entities)])
        (let ([entity (hash-ref! static-entities key '())])
          (send entity initialize wt)))
      ;
      (writeln (hash-count dynamic-entities))
      (for ([key (hash-keys dynamic-entities)])
        (let ([entity (hash-ref! dynamic-entities key '())])
          (send entity initialize wt))))
          
    (define/public (update)
      '())
    ))

