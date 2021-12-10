#lang racket
(require threading)

(define lines (file->lines "day-9.txt"))
(define height (length lines))
(define width (string-length (first lines)))

(define (char->digit char)
  (- (char->integer char) (char->integer #\0)))

(define board (map (λ (line) (map char->digit (string->list line)))
                   lines))

(define (board-ref coord)
  (list-ref (list-ref board (cdr coord)) (car coord)))

(define (neighbours coord)
  (match-define (cons x y) coord)
  (for/list ([xx (list x (add1 x) x (sub1 x))]
             [yy (list (add1 y) y (sub1 y) y)]
             #:when (and (xx . >= . 0) (xx . < . width)
                         (yy . >= . 0) (yy . < . height)))
    (cons xx yy)))

(define low-points (for*/list ([y (in-range height)]
                               [x (in-range width)]
                               #:when (let* ([coord (cons x y)]
                                             [m (apply min (map board-ref (neighbours coord)))]
                                             [v (board-ref coord)])
                                        (m . > . v)))
                     (cons x y)))

(define (grow-basin frontier seen)
  (cond [(null? frontier) seen]
        [(= 9 (board-ref (first frontier)))]
        [else
         (define coord (first frontier))
         (define value (board-ref coord))
         (define nexts (filter
                        (lambda (n)
                          (and (not (= 9 (board-ref n)))
                               (not (or (member n seen) (member n frontier)))))
                        (neighbours coord)))
         (grow-basin
          (append nexts (cdr frontier))
          (cons coord seen))]))

(~>> low-points
     (map (lambda (coord) (grow-basin (list coord) '())))
     (sort _ > #:key length)
     (take _ 3)
     (map length)
     (apply *))
