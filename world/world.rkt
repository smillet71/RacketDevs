#lang racket
(require math)

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
    (init-field [x 0.0] [y 0.0] [z 0.0])
    ; accessors
    (define/public (box) (array #[ x y z ] ))))

;; ------------------------------------------
;; definition of an object living in the simulated world
;; ------------------------------------------
(define entity%
  (class object%
    ; initialization
    (super-new)
    (init-field eid etype esubtype eposition ebbox the-world)
    (field [ t 0] ; date
           [ estate 'init ]) ; entity state, 'init 'run 'end)
    ; accessors
    (define/public (id) eid)
    (define/public (entity-type) etype) ; 'static or 'dynamic
    (define/public (entity-subtype) esubtype)
    (define/public (position) eposition)
    (define/public (bbox) ebbox)
    (define/public (state) estate)
    ;; initialize 
    (define/public (initialize)
      (send the-world add-entity this)
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
    (field [ estate 'init ] ; world state, 'init 'run 'end)
           [ static-entities (make-hash) ] ; hashtable of static entities
           [ dynamic-entities (make-hash) ] ; hashtable of dynamic entities
           )
    ; accessors
    (define/public (name) wname)
    (define/public (id) wid)
    (define/public (seed) wseed)
    (define/public (time) wt)
    ; methods
    (define/public (add-entity e)
      (if (equal? (send e entity-type) 'static)
          (hash-set! static-entities (send e id) e)
          (hash-set! dynamic-entities (send e id) e)))
    (define/public (update)
      '())
    ))

;; ------------------------------------------
;; some tests
;; ------------------------------------------
(define w (new world%))
(define e1 (new entity% 1 

