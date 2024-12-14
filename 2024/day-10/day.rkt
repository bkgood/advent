#lang racket

(define ex #<<>>
89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732
>>
  )

(define ex-small #<<>>
0123
1234
8765
9876
>>
  )

; matrix is (cols . (vector chars))
; upper left is (0, 0).

(struct point (i j) #:prefab)
(struct matrix (cols xs) #:transparent)

(define (matrix-rows m) (/ (vector-length (matrix-xs m)) (matrix-cols m)))

(define (point-translate p x y) (point (+ x (point-i p)) (+ y (point-j p))))

(define matrix-ref
  (case-lambda
    [(m i j)
     (define cols (matrix-cols m))
     (define rows (matrix-rows m))
     (if (or (< i 0) (< j 0) (>= i rows) (>= j cols))
         #f
         (vector-ref (matrix-xs m) (+ (* i cols) j)))]
    [(m point) (matrix-ref m (point-i point) (point-j point))]))

(define (matrix-fold m acc proc)
  (define-values (cols xs) (values (matrix-cols m) (matrix-xs m)))
  (for/fold ([acc acc])
            ([x (in-vector xs)]
             [i (in-naturals)])
    (proc acc
          (call-with-values
           (thunk (quotient/remainder i cols))
           point)
          x)))

(define (find-heads m)
  (define (next acc p x)
    (cond
      [(zero? (matrix-ref m p)) (cons p acc)]
      [else acc]))
  (reverse (matrix-fold m '() next)))

(define (point-translator i j)
  (lambda (x) (point-translate x i j)))

; pro tip do not reuse your old directions
(define dirs (list
              (point-translator -1 0)
              (point-translator 1 0)
              (point-translator 0 1)
              (point-translator 0 -1)))

(struct path (points) #:prefab)

(define (path-summit p) (last (path-points p)))

(define (paths-to-summits m from)
  (unique
   (flatten
    (let loop ([pos from]
               [acc-path (list from)]
               [visited (hash from #t)])
      (define cur-lvl (matrix-ref m pos))
      (for/list ([d (in-list dirs)]
                 #:do [(define next (d pos))]
                 #:when (and next (not (hash-has-key? visited next)))
                 #:do [(define next-lvl (matrix-ref m next))]
                 #:when (equal? (add1 cur-lvl) next-lvl))
        (cond
          [(= 9 next-lvl)
           (path (reverse (cons next acc-path)))]
          [else (loop next (cons next acc-path) (hash-set visited next #t))]))))))

(define (parse in)
  (define (char->int c)
    (- (char->integer c) (char->integer #\0)))
  (for/fold ([acc '()]
             [max-i 0]
             [max-j 0]
             #:result (matrix (add1 max-j) (list->vector (reverse acc))))
            ([l (in-lines in)]
             [i (in-naturals)]
             #:do []
             [c (in-string l)]
             [j (in-naturals)])
    (values (cons (char->int c) acc) i j)))

(define (unique xs)
  (for/fold ([acc '()]
             [seen (hash)]
             #:result (reverse acc))
            ([x (in-list xs)]
             #:when (not (hash-has-key? seen x)))
    (values (cons x acc) (hash-set seen x #t))))

(define (summits paths)
  (map path-summit paths))

(define (pt1 in)
  (let* ([grid (parse in)]
         [heads (find-heads grid)]
         [paths-for-head (for/hash ([h (in-list heads)])
                           (values h (paths-to-summits grid h)))]
         [summits-for-head (for/hash ([h (in-list heads)])
                             (define paths (hash-ref paths-for-head h))
                             (values h (unique (summits paths))))]
         [score-for-head (hash-map/copy
                          summits-for-head
                          (lambda (k v) (values k (length v))))])
    (for/sum ([s (in-hash-values score-for-head)]) s)))

(define (pt2 in)
  (let* ([grid (parse in)]
         [heads (find-heads grid)]
         [paths-for-head (for/hash ([h (in-list heads)])
                           (values h (paths-to-summits grid h)))]
         [n-paths-for-head (for/hash ([h (in-list heads)])
                             (define paths (hash-ref paths-for-head h))
                             (values h (length paths)))])
    (for/sum ([s (in-hash-values n-paths-for-head)]) s)))

(pt1 (open-input-string ex))
(call-with-input-file "in" pt1 #:mode 'text)

(pt2 (open-input-string ex))
(call-with-input-file "in" pt2 #:mode 'text)
