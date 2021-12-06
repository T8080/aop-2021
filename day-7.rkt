#lang racket
(require threading)

(define positions (~> (file->string "day-7.txt")
                      (string-split #rx",|\r\n")
                      (map string->number _)))

(define (fuel x)
  (/ (* x (+ x 1)) 2))

(define (cost position)
  (~>> positions
       (map (λ (p) (fuel (abs (- position p)))))
       (apply +)))

(apply min (map cost (inclusive-range (apply min positions)
                                      (apply max positions))))
