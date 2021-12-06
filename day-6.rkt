#lang racket

(define lanterns (map string->number
                      (string-split (file->string "day-6-test.txt") #rx",|\n")))

(define (make-cycle-map lanterns)
  (foldl (lambda (n acc) (hash-update acc n add1 0))
         (make-immutable-hash)
         lanterns))

(define (simulate i n cycle)
  (define pos (remainder i 9))
  (define pos-next (remainder (+ pos 7) 9))
  (define count (hash-ref cycle pos 0))
  (if (= i n)
      (apply + (hash-values cycle))
      (simulate (add1 i) n
                (hash-update cycle pos-next (curry + count) 0))))

(simulate 0 256 (make-cycle-map lanterns))
