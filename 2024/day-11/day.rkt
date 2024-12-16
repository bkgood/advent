#lang racket

; rules:
; 0 => 1
; even number of digits => two stones, [first half][second half]
; else multiply by 2024

(define (num-digits x) (add1 (exact-truncate (log x 10))))

(define histo-inc (case-lambda
                    [(ht k n) (hash-update ht k (lambda (a) (+ a n)) 0)]
                    [(ht k) (histo-inc ht k 1)]))

(define histo-inc! (case-lambda
                     [(ht k n) (hash-update! ht k (lambda (a) (+ a n)) 0)]
                     [(ht k) (histo-inc! ht k 1)]))

(define (blink row)
  (define histo (make-hash)) ; mutable saves some time.
  (for ([(x n) (in-hash row)])
    (cond
      [(zero? x) (histo-inc! histo 1 n)]
      [else
       (let ([nd (num-digits x)])
         [cond
           [(even? nd)
            (let-values ([(a b) (quotient/remainder x (expt 10 (/ nd 2)))])
              (histo-inc! histo b n)
              (histo-inc! histo a n))]
           [else (histo-inc! histo (* x 2024) n)]])]))
  histo)

(define (parse in)
  (foldl (lambda (x acc) (histo-inc acc x))
         (hash)
         (map string->number (string-split (read-line in) " "))))

(define (n-stones-after row blinks)
  (cond
    [(zero? blinks) (for/sum ([x (in-hash-values row)]) x)]
    [else
     (n-stones-after (blink row) (sub1 blinks))]))

(define ex "125 17")

(n-stones-after (parse (open-input-string ex)) 25)
(n-stones-after (call-with-input-file "in" parse) 25)
(n-stones-after (call-with-input-file "in" parse) 75)
