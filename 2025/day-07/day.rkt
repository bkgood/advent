#lang racket/base

(require racket/string
         racket/port)

(define ex #<<>>
.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............
>>
  )

(define (parse-splitters line)
  (for/hash ([c (in-string line)]
             [i (in-naturals)]
             #:when (char=? c #\^))
    (values i #t)))

(define (unique-eqv xs)
  (for/fold ([h (hasheqv)]
             #:result (hash-keys h))
            ([x (in-list xs)])
    (hash-set h x #f)))

(define (pt1 in)
  (for/fold ([beams-split 0]
             [beams-active (hasheqv (string-find (read-line in) "S") #f)]
             #:result beams-split)
            ([l (in-lines in)]
             #:do [(define splitting-positions (parse-splitters l))]
             [beam (in-hash-keys beams-active)])
    (cond
      [(hash-ref splitting-positions beam #f)
       (values (add1 beams-split)
               (hash-set* (hash-remove beams-active beam)
                          (sub1 beam) #f
                          (add1 beam) #f))]
      [else (values beams-split beams-active)])))

(module+ test
  (require rackunit)

  (check-equal? (call-with-input-string ex pt1) 21)
  (check-equal? (call-with-input-file "in" pt1) 1675))
