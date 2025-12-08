#lang racket/base

(require racket/port
         racket/string
         racket/list)

(define ex #<<>>
3-5
10-14
16-20
12-18

1
5
8
11
17
32
>>
  )

(define (parse-range line)
  (let ([splits (string-split line "-")])
    (cons (string->number (first splits)) (string->number (second splits)))))

(define (parse-fresh-ranges in)
  (for/list ([l (in-lines in)]
             #:break (string=? "" l))
    (parse-range l)))

(define (parse-available-ids in)
  (for/list ([l (in-lines in)])
    (string->number l)))

(define (parse-input in)
  (let ([fresh-ranges (parse-fresh-ranges in)]
        [avail-ids (parse-available-ids in)])
    (values fresh-ranges avail-ids)))

(define (in-ranges? ranges id)
  (for/first ([range (in-list ranges)]
              #:when (and (>= id (car range)) (<= id (cdr range))))
    #t))

(define (pt1 in)
  (define-values (fresh-ranges avail-ids) (parse-input in))
  (for/sum ([id (in-list avail-ids)])
    (if (in-ranges? fresh-ranges id) 1 0)))

(define (add-range ordered-ranges range)
  (cond
    [(null? ordered-ranges) (cons range '())]
    [else
     (let ([prev (first ordered-ranges)])
       (cond
         ; next range begins after last range ends, just accept next range.
         [(> (car range) (cdr prev)) (cons range ordered-ranges)]
         ; last range ends after next range, drop next range.
         [(>= (cdr prev) (cdr range)) ordered-ranges]
         [(and (<= (car range) (cdr prev))
               (> (cdr range) (cdr prev)))
          ; next range begins after last range's beginning and ends after its ending.
          (cons (cons (add1 (cdr prev)) (cdr range)) ordered-ranges)]
         [else
          (printf "bad: next: ~a last: ~a~n" range prev)
          (error "oops")]))]))

(define (unique-ranges ranges)
  (let ([sorted (sort ranges (lambda (a b)
                               (cond
                                 [(= (car a) (car b)) (< (cdr a) (cdr b))]
                                 [else (< (car a) (car b))])))])
    (for/fold ([out '()]
               #:result (reverse out))
              ([range (in-list sorted)])
      (add-range out range))))

(define (pt2 in)
  (define-values (fresh-ranges _) (parse-input in))
  (define uniq-ranges (unique-ranges fresh-ranges))
  (for/sum ([range (in-list uniq-ranges)])
    (add1 (- (cdr range) (car range)))))

(module+ test
  (require rackunit)
  (check-equal? (call-with-input-string ex pt1) 3)
  (check-equal? (call-with-input-file "in" pt1) 821)

  (check-equal? (call-with-input-string ex pt2) 14)
  (check-equal? (call-with-input-file "in" pt2) 344771884978261))
