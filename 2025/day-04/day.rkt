#lang racket/base

(require racket/port
         racket/stream)

(define ex #<<>>
..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.
>>
  )

(define (call-with-parsed-grid in f)
  (for/fold ([occupied-cells (hash)]
             [injured-cells (hash)]
             #:result (f occupied-cells injured-cells))
            ([i (in-naturals)]
             [row (in-lines in)]
             #:do []
             [c (in-string row)]
             [j (in-naturals)]
             #:when (char=? c #\@)
             [injured (in-injured-cells (cons i j))])
    (values
     (hash-set occupied-cells (cons i j) #t)
     (hash-update injured-cells injured add1 0))))

(define (cell-reachable? injured-cells occupied)
  (< (hash-ref injured-cells occupied 0) 4))

(define (pt1 in)
  (call-with-parsed-grid
   in
   (lambda (occupied-cells injured-cells)
     (for/sum ([occupied (in-hash-keys occupied-cells)]
               #:when (cell-reachable? injured-cells occupied))
       1))))

(define (find-removable-cell occupied-cells injured-cells)
  (for/first ([cell (in-hash-keys occupied-cells)]
              #:when (cell-reachable? injured-cells cell))
    cell))

(define-syntax-rule (in-injured-cells occupied)
  (let ([occ-i (car occupied)]
        [occ-j (cdr occupied)])
    (for*/stream ([i (in-inclusive-range (sub1 occ-i) (add1 occ-i))]
                  [j (in-inclusive-range (sub1 occ-j) (add1 occ-j))]
                  #:when (not (and (= i occ-i) (= j occ-j))))
      (cons i j))))

(define (deinjure-cell injured-cells occupied)
  (for/fold ([injured-cells injured-cells])
            ([injured (in-injured-cells occupied)])
    (hash-update injured-cells injured sub1)))

(define (pt2 in)
  (call-with-parsed-grid
   in
   (lambda (occupied-cells injured-cells)
     (let loop ([removed 0]
                [occupied-cells occupied-cells]
                [injured-cells injured-cells])
       (define removable (find-removable-cell occupied-cells injured-cells))
       (cond [removable
              (loop (add1 removed)
                    (hash-remove occupied-cells removable)
                    (deinjure-cell injured-cells removable))]
             [else removed])))))

(module+ test
  (require rackunit)

  (check-equal? (call-with-input-string ex pt1) 13)
  (check-equal? (call-with-input-file "in" pt1) 1320)

  (check-equal? (call-with-input-string ex pt2) 43)
  (check-equal? (time (call-with-input-file "in" pt2)) 8354))
