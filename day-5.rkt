#lang racket

(define lines (map (λ (s) (map string->number
                               (string-split s #rx",| -> ")))
                   (file->lines "day-5.txt")))

(define size (add1 (apply max (flatten lines))))
(define grid (make-vector (* size size) 0))

(define (fill-line! x1 y1 x2 y2)
  (define i
    (+ x1 (* size y1)))
  (define (step from to)
    (+ from (max -1 (min 1 (- to from)))))

  (vector-set! grid i (add1 (vector-ref grid i)))

  (if (not (and (= x1 x2) (= y1 y2)))
      (fill-line! (step x1 x2) (step y1 y2) x2 y2)
      grid))

(define straights
  (filter (match-lambda [(list x1 y1 x2 y2)
                         (or (= x1 x2) (= y1 y2))])
          lines))

(for ([line lines])
  (apply fill-line! line))

(vector-count (curry < 1) grid)
