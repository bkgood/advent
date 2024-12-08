#lang racket

(require racket/treelist)

(define ex #<<>>
............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
>>
  )

(define (delta a b) (cons (- (car b) (car a)) (- (cdr b) (cdr a))))
(define (coord-add a b) (cons (+ (car a) (car b)) (+ (cdr a) (cdr b))))
(define (coord-sub a b) (coord-add a (cons (- (car b)) (- (cdr b)))))
(define (coord-=? a b) (and (= (car a) (car b)) (= (cdr a) (cdr b))))
(define (coord-ok? a max)
  (and
   (>= (car a) 0)
   (>= (cdr a) 0)
   (<= (car a) (car max))
   (<= (cdr a) (cdr max))))

(define (antinodes a b max)
  (cond
    [(or (> (car a) (car b)) (and (= (car a) (car b)) (> (cdr a) (cdr b))))
     (antinodes b a max)]
    [else
     (let* ([d (delta a b)]
            [a1 (coord-sub a d)]
            [a2 (coord-add b d)])
       (for/list ([x (in-list (list a1 a2))]
                  #:when (and (coord-ok? x max)
                              (not (or
                                    (coord-=? a x)
                                    (coord-=? b x)))))
         x))]))

(define (extrapolate-from-points a b max)
  (define d (delta a b))
  (define (trace a next)
    (let loop ([out (list a)])
      (define p (next (car out)))
      (cond
        [(coord-ok? p max)
         (loop (cons p out))]
        [else
         out])))
  (let ([d (delta a b)])
    (append (trace a (lambda (x) (coord-add x d)))
            (trace a (lambda (x) (coord-sub x d))))))

(define (parse in)
  (define (add-point h c i j)
    (hash-update h c (lambda (xs) (treelist-add xs (cons i j))) (treelist)))
  (for/fold ([acc (hash)]
             [max #f])
            ([l (in-lines in)]
             [i (in-naturals)]
             #:do [] ; splice in nested loop
             [c (in-string l)]
             [j (in-naturals)])
    (values
     (cond
       [(not (char=? c #\.)) (add-point acc c i j)]
       [else acc])
     (cons i j))))

(define (hash-set-all h keys v)
  (for/fold ([h h])
            ([x (in-list keys)])
    (hash-set h x v)))

(define (pt1 antennae-by-signal max)
  (hash-count
   (for/fold ([antinode-points (hash)])
             ([antennae (in-hash-values antennae-by-signal)])
     (for*/fold ([antinode-points antinode-points])
                ([a (in-treelist antennae)]
                 [b (in-treelist antennae)])
       (hash-set-all antinode-points (antinodes a b max) #t)))))

(define (pt2 antennae-by-signal max)
  (hash-count
   (for/fold ([antinode-points (hash)])
             ([antennae (in-hash-values antennae-by-signal)])
     (for*/fold ([antinode-points antinode-points])
                ([a (in-treelist antennae)]
                 [b (in-treelist antennae)]
                 #:when (not (coord-=? a b)))
       (hash-set-all antinode-points (extrapolate-from-points a b max) #t)))))

(call-with-values (thunk (parse (open-input-string ex))) pt1)
(call-with-values (thunk (call-with-input-file "in" parse #:mode 'text)) pt1)

(call-with-values (thunk (parse (open-input-string ex))) pt2)
(call-with-values (thunk (call-with-input-file "in" parse #:mode 'text)) pt2)
