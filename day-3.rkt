#lang racket

(define input (map (lambda (line)
                     (map (lambda c
                            (if (equal? (car c) #\1) 1 -1))
                          (string->list line)))
                   (file->lines "day-3.txt")))

(define (sum-rowwise input)
  (foldl (curry map +) (car input) (cdr input)))

(define (decimal list)
  (cond [(null? list) 0]
        [else (+ (if (car list)
                     (expt 2 (sub1 (length list)))
                     0)
                 (decimal (cdr list)))]))

(define epsilon (map negative? (sum-rowwise input)))
(define gamma (map not epsilon))

(* (decimal gamma)
   (decimal epsilon))

(define (narrow index input test)
  (cond [(null? (cdr input)) (car input)]
        [else (let* ([sign (list-ref (sum-rowwise input) index)]
                     [target (if (test sign 0) positive? negative?)]
                     (input-next (filter (lambda (line)
                                           (target (list-ref line index)))
                                         input)))
                (narrow (add1 index)
                        input-next
                        test))]))

(* (decimal (map positive? (narrow 0 input <)))
   (decimal (map positive? (narrow 0 input >=))))
