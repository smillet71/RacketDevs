#lang racket

(require rackunit rackunit/gui
         "./base/test-automata.rkt"
         "./base/test-component.rkt"
         "./base/test-databus.rkt")

;
(test/gui automata-test-suite
          component-test-suite
          databus-test-suite)
