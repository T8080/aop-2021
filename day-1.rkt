#lang racket

;; (cdr (foldl (λ (next aggregate)
;;                (let [(previous (car aggregate))
;;                      (count (cdr aggregate))]
;;                  (cons next
;;                        (if (> next previous)
;;                            (add1 count)
;;                            count))))
;;             (cons +inf.0 0)
;;             input))

;; (define source "day-1-test.txt")
(define source "day-1.txt")

(define input (map string->number (file->lines source)))

(define (increase-count x)
  (count <
         (drop-right (cons +inf.0 x) 1)
         x))

(increase-count input)

(increase-count (map +
                     (drop (append input '(0 0)) 2)
                     (drop (append input '(0)) 1)
                     input))
