#lang racket

(define ex #<<>>
L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
>>
  )

(define (count-zero-positions in)
  (let loop ([pos 50]
             [zeros-seen 0])
    (let ([line (read-line in)])
      (cond
        [(eof-object? line) zeros-seen]
        [else
         (string-set! line 0 (if (equal? (string-ref line 0) #\L) #\- #\+))
         (let* ([turn (string->number line)]
                [after-turn (modulo (+ pos turn) 100)])
           (loop after-turn (+ zeros-seen (if (= after-turn 0) 1 0))))]))))

(define (count-zero-clicks in)
  (let loop ([pos 50]
             [zeros-seen 0])
    (let ([line (read-line in)])
      (cond
        [(eof-object? line) zeros-seen]
        [else
         (string-set! line 0 (if (equal? (string-ref line 0) #\L) #\- #\+))
         (let* ([turn (string->number line)]
                [after-turn (modulo (+ pos turn) 100)]
                [quo (/ (+ pos turn) 100)]
                [zeros-clicked ((if (and (not (zero? pos)) (<= quo 0))
                                    add1 identity)
                                (abs (truncate quo)))])
           (loop after-turn
                 (+ zeros-seen zeros-clicked)))]))))

(call-with-input-string ex count-zero-positions)
(call-with-input-file "input-1" count-zero-positions)
(call-with-input-string ex count-zero-clicks)
(call-with-input-file "input-1" count-zero-clicks)
