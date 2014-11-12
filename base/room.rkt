#lang racket

; definition of what is a room
(struct room 
  ( id                 ; identifier of the room
    pos-x              ; left up corner
    pos-y              ; left up corner
    level              ; level number (Z coord)
    size-x             ; size along x
    size-y             ; size along y
    wall-type          ; wall type ex: 'raw
    floor-type         ; floor type ex: 'raw 
    room-type          ; room type ex: 'hangar
    list-of-equipment  ; list of equipment contained in this room
    (doors #:mutable) ); list of doors connecting this room to other rooms
  #:transparent )

; definition of what is door
(struct door
  ( pos-x              ; position in X
    pos-y              ; position in Y
    width              ; if door is along X then X>0
    height             ; if door is along Y then Y>0
    door-type          ; type of door, ex: 'basic
    door-state         ; door state, ex: 'open 'closed 'locked
    id1 id2 )          ; ids of connected rooms
  #:transparent )

; list of all rooms indexed by ids
(define hroom (make-hash))

; get list of rooms
(define (rooms)
  hroom)

; utility functions
; room initial creation
(define (create-room id px py l sx sy wt gt rt)
  (let ((r (room id px py l sx sy wt gt rt '() '())))
    (hash-set! hroom id r)
    r))

; add a door to a room
;; creer un cas test avec 2 pièces
(define (add-door-to-room r d)
  (let* ((x1 (room-pos-x r))
         (y1 (room-pos-y r))
         (x2 (+ x1 (room-size-x r)))
         (y2 (+ y1 (room-size-y r)))
         (xd (door-pos-x d))
         (yd (door-pos-y d))
         (dw (door-width d))
         (dh (door-height d))
         (b1 (and (= x1 xd) (>= yd y1) (<= yd (- y2 dh))))
         (b2 (and (= x2 xd) (>= yd y1) (<= yd (- y2 dh))))
         (b3 (and (= y1 yd) (>= xd x1) (<= xd (- x2 dw))))
         (b4 (and (= y2 yd) (>= xd x1) (<= xd (- x2 dw))))
         (b (or b1 b2 b3 b4)))
    (when b
      (set-room-doors! r (cons (room-doors r) d)))
    b))
         
         
             

; adding a door to connect 2 rooms
(define (connect-rooms idr1 idr2 x y w h dt ds)
  (let* ((r1 (hash-ref hroom idr1))
         (r2 (hash-ref hroom idr2))
         (l1 (room-level r1))
         (l2 (room-level r2)))
    (when (= l1 l2)
      (let ( (d  (door x y w h dt ds idr1 idr2)) )
        (add-door-to-room r1 d)
        (add-door-to-room r2 d)))))

; removing a door 

; upgrading a door

; upgrading a room

; addinq equipment

; removing equipment

; upgrading equipment

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Exemple
(define r1 (create-room 1001 50 50 0 10 10 'raw 'raw 'storage))
(define r2 (create-room 1002 61 50 0 10 10 'raw 'raw 'storage))
(define cx (create-room 1003 60 55 0 1  1  'raw 'raw 'passage))
(connect-rooms 1001 1003 60 55 0 1 'basic 'closed)
(connect-rooms 1002 1003 61 55 0 1 'basic 'closed)

