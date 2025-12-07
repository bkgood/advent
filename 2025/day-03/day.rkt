#lang racket/base

(require racket/list
         racket/math
         racket/port)

(define ex #<<>>
987654321111111
811111111111119
234234234234278
818181911112111
>>
  )

(define (best-joltage batts)
  (define best-first (apply max (drop-right batts 1)))
  (let loop ([batts batts]
             [best 0])
    (cond
      [(or (null? batts) (null? (rest batts))) best]
      [else
       (let ([batt (first batts)]
             [batts (rest batts)])
         (loop batts (max best
                          (if (= batt best-first)
                              (+ (* 10 best-first) (apply max batts))
                              0))))])))

(define (parse-bank s)
  (define zeroa (char->integer #\0))
  (for/list ([c (in-string s)])
    (- (char->integer c) zeroa)))

(define (parse-banks in)
  (for/list ([l (in-lines in)])
    (parse-bank l)))

(define (pt1 in)
  (define banks (parse-banks in))
  (for/sum ([b (in-list banks)])
    (let ([bj (best-joltage b)])
      bj)))

(module+ test
  (require rackunit)

  (check-equal? (call-with-input-string ex pt1) 357))

(module+ main
  (call-with-input-file "in" pt1))

(define (num-digits i)
  (add1 (exact-truncate (log i 10))))

(define (prepend-digit i d)
  (+ (* d (expt 10 (num-digits i))) i))

(module+ test
  (check-equal? (prepend-digit 100 9) 9100))

(define (choose-best-heads-with-remaining bank remaining)
  (let loop ([bank (rest bank)]
             [found (list bank)])
    (cond
      [(or (null? bank) (< (cdr (first bank)) remaining))
       (reverse found)]
      [else
       (let ([curr-best (car (first bank))]
             [cand (car (first (first found)))])
         (loop (rest bank)
               (cond
                 [(> curr-best cand) (list bank)]
                 [(= curr-best cand) (cons bank found)]
                 [else found])))])))

(define (with-remaining bank)
  (let ([len (length bank)])
    (for/list ([x (in-list bank)]
               [i (in-inclusive-range (sub1 len) 0 -1)])
      (cons x i))))

(define (best-joltage-len bank of-batts)
  (let loop ([with-remaining (with-remaining bank)]
             [still-need (sub1 of-batts)])
    (let ([cands (choose-best-heads-with-remaining with-remaining still-need)])
      (cond
        [(zero? still-need)
         ; this is our last digit, so we can just give the best one.
         (car (first (first cands)))]
        [else
         ; iterate through all of cands, choose best one
         (for/fold ([best #f])
                   ([cand (in-list cands)])
           (define c (prepend-digit
                      (loop (rest cand) (sub1 still-need))
                      (car (first cand))))
           (if (or (not best) (> c best)) c best))]))))

(define (pt2 in)
  (define banks (parse-banks in))
  (for/sum ([b (in-list banks)])
    (best-joltage-len b 12)))

(module+ test
  (check-equal? (call-with-input-string ex pt2) 3121910778619))

(module+ main
  (call-with-input-file "in" pt2))
