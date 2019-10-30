#lang racket/gui

(require racket/path)
(require data/monad)
(require data/either)

(require "exolve.rkt")
(require "qxw.rkt")

(provide (all-defined-out))

(define (try v e)
  (if v (success v) (failure e)))
