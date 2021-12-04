#lang racket

(define input (string-split (file->string "day-4.txt")
                            #:repeat? #t))

(define numbers (map string->number
                     (string-split (car input)
                                   ",")))

(define (chunk rest n)
  (if (null? rest) rest
      (let-values ([(b r) (split-at rest n)])
        (cons b
              (chunk r n)))))

(define boards (chunk (map string->number (cdr input)) 25))

(define board-filled-in? negative?)

(define (board-fill-in board target)
  (map (λ (n) (if (= n target) -1 n))
       board))

(define (board-ref board row col)
  (list-ref board (+ (* 5 row) col)))

(define (board-cols board)
  (for/list ([row 5])
    (for/list ([col 5])
      (board-ref board col row))))

(define (board-lines board)
  (append (chunk board 5)
          (board-cols board)))

(define (board-wins? board)
  (ormap (λ (line) (andmap board-filled-in? line))
         (board-lines board)))

(define (board-win-score board n)
  (* n (apply + (filter (compose not board-filled-in?)
                        board))))

(define (part1 boards numbers)
  (let* ([n (car numbers)]
         [next-boards (map (curryr board-fill-in n) boards)]
         [winner (findf board-wins? next-boards)])
    (if winner
        (board-win-score winner n)
        (part1 next-boards
              (cdr numbers)))))

(define (part2 boards numbers last-score)
  (let* ([n (car numbers)]
         [next-boards (map (curryr board-fill-in n) boards)]
         [winner (findf board-wins? next-boards)])
    (cond [(empty? boards) last-score]
          [winner
           (part2 (filter (compose not board-wins?) next-boards)
                  (cdr numbers)
                  (board-win-score winner n))]
          [else (part2 next-boards
                       (cdr numbers)
                       last-score)])))

(part1 boards numbers)
(part2 boards numbers 0)
