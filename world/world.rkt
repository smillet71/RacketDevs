#lang racket
(require math)

(provide world-position%
         entity-bounding-box%
         entity%
         world%)

;; ------------------------------------------
;; distance from pos1 to pos2
(define (pos-dist pos1 pos2)
  (let* ([ v0 (array- pos1 pos2)]
         [ v1 (array-sqr v0)]
         [ d (sqrt (array-all-sum v1)) ] )
    d))
;; ------------------------------------------
;; angle of vector pos2-pos1
(define (pos-azimuth-rad pos1 pos2)
  (let* ([ v (array- pos2 pos1)]
         [ a (atan (array-ref v #(1)) (array-ref v #(0))) ])
    a))
;; ------------------------------------------
(define (rad2deg x)
  (* (/ x pi) 180.0))
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
    (init-field [x 0.01] [y 0.01] [z 0.01])
    ; accessors
    (define/public (box) (array #[ x y z ]))))

;; ------------------------------------------
;; definition of an entity sensor 
;; ------------------------------------------
(define sensor%
  (class object%
    ; initialization
    (super-new)
    (init-field entity)
    ))
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
           [ estate 'init ]) ; entity state, 'init 'run 'end
    
    ;
    (field [has-sensors #f]
           [has-weapons #f]
           [is-detectable #f]
           [can-move #f]
           [sensors '()]
           [weapons '()]
           [signatures '()])

    ;
    (add-to-world)
    
    ; accessors
    (define/public (id) eid)
    (define/public (entity-type) etype) 
    (define/public (entity-subtype) esubtype)
    (define/public (position) eposition)
    (define/public (bbox) ebbox)
    (define/public (state) estate)
    (define/public (set-id the-eid) (set! eid the-eid))
    (define/public (hasSensors) has-sensors)
    (define/public (hasWeapons) has-weapons)
    (define/public (canMove) can-move)
    (define/public (isDetectable) is-detectable)
    (define/public (last-time) t)
    
    ;; add this object to the world 
    (define/public (add-to-world)
      (send the-world add-entity this))
    
    ;; initialize 
    (define/public (initialize the-date)
      (set! t the-date)
      (set! estate 'run))
    
    ;; update the state of this entity
    (define/public (update current-time delta-t)
      ; apply-pending-commands
      ; update sensors
      ; update weapons
      ; update position
      ;
      '()
      )
    
    ;; add a new sensor
    (define/public (add-sensor a-sensor)
      (if (null? sensors) (list a-sensor) (cons a-sensor sensors)))
    
    ;; add a new weapon
    (define/public (add-weapon a-weapon)
      (if (null? weapons) (list a-weapon) (cons a-weapon weapons)))
    
    ;; add a new signature
    (define/public (add-signature a-signature)
      (if (null? signatures) (list a-signature) (cons a-signature signatures)))
    
    ))

;; ------------------------------------------
;; definition of the interaction data 
;; ------------------------------------------
(define entity-interaction%
  (class object%

    ;initialization
    (super-new)

    ;constructor
    (init-field id1 id2)

    ;fields
    (field [last-update -1]
           [distance 0]
           [azimut-12 0]
           [azimut-21 0]
           [azimut-12-rl 0]
           [azimut-21-rl 0])

    ; accessors
    (define/public (last-update-time-s) last-update)
    (define/public (distance-km) distance)
    (define/public (azimut12-deg) azimut-12)
    (define/public (azimut21-deg) azimut-21)
    (define/public (azimut12-rl-deg) azimut-12-rl)
    (define/public (azimut21-rl-deg) azimut-21-rl)

    ; update
    (define/public (update e1 e2 the-time)
      (when (> the-time (last-update-time-s))
        (let* ([wpos1 (send e1 position )]
               [pos1 (send wpos1 position )]
               [att1 (send wpos1 attitudes )]
               [route1 (array-ref att1 #(0))]
               [spd1 (send wpos1 speed )]
               [wpos2 (send e2 position )]
               [pos2 (send wpos2 position )]
               [att2 (send wpos2 attitudes )]
               [route2 (array-ref att2 #(0))]
               [spd2 (send wpos2 speed )])
          (set! distance (pos-dist pos1 pos2))
          (set! azimut-12 (rad2deg (pos-azimuth-rad pos1 pos2)))
          (set! azimut-21 (rad2deg (pos-azimuth-rad pos1 pos2))) ; ou azimut12 - 180°
          (set! azimut-12-rl (- azimut-12 (rad2deg route1)))
          (set! azimut-21-rl (- azimut-21 (rad2deg route2)))
        )))
    ))

;; ------------------------------------------
;; definition of the simulated world
;; ------------------------------------------
(define world%
  (class object%
    
    ; initialization
    (super-new)
    (init-field [wname 'the-world] [wid 0] [wseed 0] [wt 0] [cadence-hz 20])
    (field [ estate 'init ]         ; world state, 'init 'run 'end)
           [ entities (make-hash) ] ; hashtable of entities
           [ eids 100]              ; counter for entity ids
           [ deltaT (/ 1.0 cadence-hz) ]
           [ interaction-matrix '() ]
           )
    
    ; accessors
    (define/public (name) wname)
    (define/public (id) wid)
    (define/public (seed) wseed)
    (define/public (time) wt)
    (define/public (ids) eids)
    (define/public (get-entities) entities)
    
    ; methods
    (define/public (add-entity e)
      (send e set-id eids)
      (set! eids (+ eids 1))
      (hash-set! entities (send e id) e))

    ; create the interaction matrix
    (define/public (create-interaction-matrix)
      (set! interaction-matrix (make-hash)))
    
    ; create the interaction matrix
    ; hashtable of hashtables with keys equals to entity ids
    (define/public (update-interaction-matrix the-time)
      (hash-for-each entities
                     (lambda (key1 value1)
                       (hash-for-each entities
                                      (lambda (key2 value2)
                                        (when (not (equal? key1 key2))
                                          (update-interaction key1 value1 key2 value2 the-time)))))))

    ; update the data describing the interaction parameters between 2 entities
    (define/public (update-interaction id1 e1 id2 e2 the-time)
      (let* ([ interaction12 (hash-ref!
                           (hash-ref! interaction-matrix id1 (lambda () (make-hash)))
                           id2
                           (lambda () (new entity-interaction% [id1 id1] [id2 id2])))]
             [ interaction21 (hash-ref!
                           (hash-ref! interaction-matrix id2 (lambda () (make-hash)))
                           id1
                           (lambda () interaction12 ))])
        (send interaction12 update e1 e2 the-time)  
        (print (list the-time id1 id2 interaction12))))
    
    ; initialize all simulated objects
    (define/public (init)
      ;
      (writeln (hash-count entities))
      ;
      (for ([key (hash-keys entities)])
        (let ([entity (hash-ref! entities key '())])
          (send entity initialize wt)))
      ;
      (create-interaction-matrix)
      (update-interaction-matrix (time))
      )
    
    ; update all simulated objects
    (define/public (update)
      (set! wt (+ wt deltaT))
      (writeln (hash-count entities))
      (update-interaction-matrix (time))
      (for ([key (hash-keys entities)])
        (let ([entity (hash-ref! entities key '())])
          (send entity update wt deltaT))))
    
    ))

