#lang racket

(module submarine1 racket/base
  (define depth 0)
  (define horizontal 0)
  (define-namespace-anchor ans)
  (provide depth horizontal ans)

  (define (forward x)
    (set! horizontal (+ horizontal x)))

  (define (down x)
    (set! depth (+ depth x)))

  (define (up x)
    (set! depth (- depth x))))

(module submarine2 racket/base
  (define aim 0)
  (define depth 0)
  (define horizontal 0)
  (define-namespace-anchor ans)
  (provide depth horizontal ans)

  (define (forward x)
    (set! horizontal (+ horizontal x))
    (set! depth (+ depth (* aim x))))

  (define (down x)
    (set! aim (+ aim x)))

  (define (up x)
    (set! aim (- aim x))))

(require 'submarine2)

(for [(line (file->lines "day-2.txt"))]
  (eval (read (open-input-string (string-append "(" line ")")))
        (namespace-anchor->namespace ans)))

(* depth horizontal)
