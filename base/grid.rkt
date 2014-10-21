#lang racket

(provide make-grid cell cell-set!)

;
(struct grid 
  ( size [data #:mutable] ) 
  #:transparent)

;
(define (make-grid n)
  (grid n (make-vector (* n n) 0)))

;
(define (cell g i j)
  (let ((n (grid-size g))
        (data (grid-data g)))
  (vector-ref data (+ (* i n) j))))

(define (cell-set! g i j v)
  (let ((n (grid-size g))
        (data (grid-data g)))
  (vector-set! data (+ (* i n) j) v)))

