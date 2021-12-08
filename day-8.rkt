#lang rackjure

(define input (map (λ (line)
                     (map (λ (part)
                           (string-split part))
                          (string-split line " | ")))
                   (file->lines "day-8-test.txt")))

(define segments
  (dict-map
   {"abcefg"  0
    "cf"      1
    "acdeg"   2
    "acdfg"   3
    "bcdf"    4
    "abdfg"   5
    "abdefg"  6
    "acf"     7
    "abcdefg" 8
    "abcdfg"  9}
   (λ (k v) (cons (list->set (string->list k)) v))))

(define (decode-signal assignment signal)
  (~>> signal
       string->list
       (map (curry dict-ref assignment))
       list->set
       segments))

(define (valid? assignment signals)
  (andmap (curry decode-signal assignment) signals))

(define (find-assignment assignment variables options signals)
  (or (and (null? variables) (valid? assignment signals) assignment)
      (ormap (λ (option)
               (find-assignment (dict-set assignment (car variables) option)
                                (cdr variables)
                                (remove option options)
                                signals))
             options)))

(define (decode-values assignment values)
  (concat-numbers (map (curry decode-signal assignment) values)))

(define (concat-numbers numbers)
  (if (null? numbers) 0
      (+ (* (car numbers) (expt 10 (sub1 (length numbers))))
         (concat-numbers (cdr numbers)))))

(define vars
  (string->list "abcdefg"))

(for/sum ([line input])
  (match-define (list signals values) line)
  (define assignment
    (find-assignment (make-immutable-hash) vars vars signals))
  (decode-values assignment values))
