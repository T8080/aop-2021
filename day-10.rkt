#lang rackjure

(define input (file->lines "day-10.txt"))

(define pairs
  {#\( #\) #\[ #\] #\{ #\} #\< #\>})

(define error-cost
  {#\) 3 #\] 57 #\} 1197 #\> 25137})

(define unclose-cost
  {#\) 1 #\] 2 #\} 3 #\> 4})

(define (open? token)
  (member token (dict-keys pairs)))

(define (close? token)
  (member token (dict-values pairs)))

(define (find-unmatched tokens unmatched)
  (define token (if (null? tokens) #f (first tokens)))
  (cond [(not token) unmatched]
        [(open? token)
         (find-unmatched (rest tokens)
                         (cons (pairs token)
                               unmatched))]
        [(eq? token (first unmatched))
         (find-unmatched (rest tokens)
                         (rest unmatched))]
        [(close? token) token]))

(define (residu-cost unmatched)
  (foldl (λ (p total) (+ (* total 5) (unclose-cost p))) 0 unmatched))

(define errors
  (map #λ(find-unmatched (string->list %) '()) input))

(~>> errors
     (filter char?)
     (map (curry dict-ref error-cost))
     (apply +))

(define unmatched-error-costs
  (~>> errors
       (filter pair?)
       (map residu-cost)
       (sort _ <)))

(list-ref unmatched-error-costs (quotient (length unmatched-error-costs) 2))
