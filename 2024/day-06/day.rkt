#lang racket

(define ex #<<>>
....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
>>
  )

(require racket/hash)

(define (parse in)
  (define obs (make-hash))
  (define max-x 0)
  (define max-y 0)
  (define guard #f)

  (for ([l (in-lines in)]
        [i (in-naturals)])
    (set! max-y (max max-y i))
    (for      ([c (in-string l)]
               [j (in-naturals)])
      (set! max-x (max max-x j))
      (case c
        [(#\#)
         (hash-set! obs (cons i j) #t)
         #;(printf "found obstacle at (~a, ~a)~n" i j)]
        [(#\^)
         (set! guard (cons i j))
         #;(printf "found guard at (~a, ~a)~n" i j)])))
  #;(printf "obstacles: ~a~n" obs)
  (values obs guard (cons max-x max-y)))

(define (advance pos dir max)
  #;(printf "pos: ~a dir: ~a~n" pos dir)
  (let* ([pos (cons (+ (car pos) (car dir)) (+ (cdr pos) (cdr dir)))]
         [x (car pos)]
         [y (cdr pos)])
    (if (or
         (< x 0) (< y 0)
         (> x (car max)) (> y (cdr max)))
        #f
        pos)))

(define next-dir (hash '(-1 .  0) '( 0 .  1)
                       '( 0 .  1) '( 1 .  0)
                       '( 1 .  0) '( 0 . -1)
                       '( 0 . -1) '(-1 .  0)))

(define (turn-right dir)
  (hash-ref next-dir dir))

(define (walk obs starting-pos max)
  (let loop ([guard starting-pos]
             [dir '(-1 . 0)]
             [visited (hash starting-pos #t)]
             [iters-ok (* 1 (car max) (cdr max))])
    (let ([new-pos (advance guard dir max)])
      #;(printf "~a @ ~a -> ~a~n" dir guard new-pos)
      (cond
        [(zero? iters-ok) 'too-many-iters]
        [new-pos
         (cond
           [(hash-has-key? obs new-pos)
            #;(printf "turning at ~a~n" guard)
            (loop guard (turn-right dir) visited (sub1 iters-ok))]
           [else
            #;(printf "visiting ~a~n" new-pos)
            (loop new-pos dir (hash-set visited new-pos #t) (sub1 iters-ok))])]
        [else visited]))))

(define (pt1 in)
  (hash-count (call-with-values (thunk (parse in)) walk)))

(define (pt2 in)
  (define-values (obs guard max) (parse in))
  (define obs-imm (make-immutable-hash (hash->list obs)))
  (define visited (walk obs guard max))
  (define circular-blocks (for/fold ([h (hash)]
                                     #:result (hash-keys h))
                                    ([v (in-hash-keys visited)])
                            (define h2 (for/hash ([dir (in-hash-keys next-dir)]
                                                  #:do ([define block (advance v dir max)])
                                                  #:when block
                                                  #:when (not (equal? block guard))
                                                  #:when (not (hash-has-key? obs block))
                                                  #:when (eq? 'too-many-iters (walk (hash-set obs-imm block #t) guard max)))
                                         (values block #t)))
                            (hash-union h h2 #:combine (thunk* #t))))
  (length circular-blocks))

#;(parse (open-input-string ex))
(pt1 (open-input-string ex))
(call-with-input-file "in" pt1 #:mode 'text)
(pt2 (open-input-string ex))
(call-with-input-file "in" pt2 #:mode 'text)