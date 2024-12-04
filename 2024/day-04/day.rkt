#lang racket

(define ex #<<>>
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
>>
  )

; matrix is (cols . (vector chars))
; upper left is (0, 0).

(define (matrix-cols m) (car m))
(define (matrix-rows m) (/ (vector-length (cdr m)) (matrix-cols m)))

(define (matrix-ref m i j)
  (define cols (matrix-cols m))
  (define rows (matrix-rows m))
  (if (or (< i 0) (< j 0) (>= i rows) (>= j cols))
      #f
      (vector-ref (cdr m) (+ (* i cols) j))))

(define (matrix-ref/point m p)
  (matrix-ref m (car p) (cdr p)))

; flatten a list of vectors into a single vector
(define (flatten-vectors xs)
  (for/fold ([out (make-vector (for/sum ([x (in-list xs)]) (vector-length x)))]
             [pos 0]
             #:result (vector->immutable-vector out))
            ([x (in-list xs)])
    (vector-copy! out pos x)
    (values out (+ pos (vector-length x)))))

(define (parse in)
  (define (string->vector s)
    (for/vector #:length (string-length s) ([c (in-string s)]) c))

  (let* ([line (read-line in)]
         [cols (string-length line)])
    (for/fold ([lines (list (string->vector line))]
               #:result (cons cols (flatten-vectors (reverse lines))))
              ([line (in-lines in)])
      (cons (string->vector line) lines))))

(define (point-adder d)
  (lambda (x) (cons (+ (car x) (car d)) (+ (cdr x) (cdr d)))))

(define (match-from-point m point chars dir)
  (cond
    [(null? chars) #t]
    [else
     (define-values (c cs) (values (first chars) (rest chars)))
     (define x (matrix-ref/point m point))
     (and
      x
      (char-ci=? x c)
      (match-from-point m (dir point) cs dir))]))

(define (count-xmases m)
  (define dirs (for*/list ([i (in-range -1 2)] [j (in-range -1 2)]) (point-adder (cons i j))))
  (for*/sum ([i (in-range (matrix-rows m))]
             [j (in-range (matrix-cols m))])
    (for/sum ([dir (in-list dirs)])
      (if (match-from-point m (cons i j) (string->list "xmas") dir) 1 0))))

(define m (parse (open-input-string ex)))
(count-xmases m)
(count-xmases (call-with-input-file "in" parse #:mode 'text))

(define (x-mas-at-point m p)
  (define up-left    (point-adder (cons -1 -1)))
  (define down-left  (point-adder (cons  1 -1)))
  (define up-right   (point-adder (cons -1  1)))
  (define down-right (point-adder (cons  1  1)))

  (define mas (string->list "mas"))
  (define sam (string->list "sam"))

  (and
   (or
    (match-from-point m (up-left p) mas down-right)
    (match-from-point m (up-left p) sam down-right))
   (or
    (match-from-point m (down-left p) mas up-right)
    (match-from-point m (down-left p) sam up-right))))

(define (count-x-mases m)
  (for*/sum ([i (in-range (matrix-rows m))]
             [j (in-range (matrix-cols m))])
    (if (x-mas-at-point m (cons i j)) 1 0)))

(count-x-mases m)
(count-x-mases (call-with-input-file "in" parse #:mode 'text))
