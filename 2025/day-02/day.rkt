#lang racket/base

(require (only-in racket/string
                  string-split)
         (only-in racket/list
                  second
                  third)
         (only-in racket/port
                  call-with-input-string))

(define ex #<<>>
11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124
>>
  )

(define repeat-matcher
  (let ([lookup (for/vector ([i (in-inclusive-range 1 10)])
                  (pregexp (format "^([0-9]{~a})\\1+$" i)))])
    (lambda (n) (vector-ref lookup (sub1 n)))))

(define (num-digits i)
  (truncate (add1 (log i 10))))

(define (is-duplicated-sequence? i)
  (and (even? (num-digits i))
       (let* ([s (number->string i)]
              [len (truncate (/ (string-length s) 2))])
         (regexp-match (repeat-matcher len) s))))

(define (is-repeated-sequence? i)
  (let ([s (number->string i)])
    (for/or ([i (in-inclusive-range 1 (truncate (/ (string-length s) 2)))])
      (regexp-match (repeat-matcher i) s))))

(define (parse-ranges line)
  (for/list ([s (string-split line ",")])
    (define matches (regexp-match-positions #px"([0-9]{1,})-([0-9]{1,})" s))
    (cons (string->number (substring s (car (second matches)) (cdr (second matches))))
          (string->number (substring s (car (third matches)) (cdr (third matches)))))))

(define (sum-duplicated-sequence-numbers in)
  (for*/sum ([range (in-list (parse-ranges (read-line in)))]
             [i (in-inclusive-range (car range) (cdr range))]
             #:when (is-duplicated-sequence? i))
    i))

(define (sum-repeated-sequence-numbers in)
  (for*/sum ([range (in-list (parse-ranges (read-line in)))]
             [i (in-inclusive-range (car range) (cdr range))]
             #:when (is-repeated-sequence? i))
    i))

(module+ main
  (call-with-input-string ex sum-duplicated-sequence-numbers)
  (call-with-input-file "in" sum-duplicated-sequence-numbers)

  (call-with-input-string ex sum-repeated-sequence-numbers)
  (call-with-input-file "in" sum-repeated-sequence-numbers))
