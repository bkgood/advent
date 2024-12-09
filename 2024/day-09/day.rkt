#lang racket

(require racket/treelist)

(define ex #<<>>
2333133121414131402
>>
  )

; ([file len][free space len]) ...
; files have IDs from the left, starting with 0.

(define (treelist-mem xs x #:from [from #f] #:to [to #f])
  (cond
    [(not from) (treelist-mem xs x #:from 0 #:to to)]
    [(not to) (treelist-mem xs x #:from from #:to (treelist-length xs))]
    [else
     (for/first ([i (in-range from to)]
                 #:when (equal? (treelist-ref xs i) x))
       i)]))

(define (treelist-set tl i v)
  (treelist-insert (treelist-delete tl i) i v))

(define (compact blocks)
  (for/fold ([blocks blocks]
             [search-from 0]
             #:result blocks)
            ([i (in-inclusive-range (sub1 (treelist-length blocks)) 0 -1)]
             #:do [(define x (treelist-ref blocks i))]
             #:when (not (equal? x #\.))
             #:do [(define free-pos (treelist-mem blocks #\. #:from search-from #:to i))]
             #:break (not free-pos))
    (values (treelist-set (treelist-set blocks free-pos x) i #\.)
            free-pos)))

(define (treelist-first-where tl ok? #:from [from 0] #:to [to #f])
  (if (not to)
      (treelist-first-where tl ok? #:from from #:to (treelist-length tl))
      (for/first ([x (in-treelist tl)]
                  [i (in-range from to)]
                  #:when (ok? x))
        i)))

(define (print-nodes nodes)
  (for ([n (in-treelist nodes)])
    (define char (cond
                   [(equal? 'free (car n)) "."]
                   [else (car n)]))
    (for ([_ (in-range (cdr n))])
      (display char)))
  (newline))

(define (defragmented-compact nodes)
  #;(print-nodes nodes)
  (for/fold ([nodes nodes])
            ([i (in-inclusive-range (sub1 (treelist-length nodes)) 0 -1)]
             #:do [(define x (treelist-ref nodes i))]
             #:when (not (equal? (car x) 'free))
             #:do [(define (ok? y)
                     (and (equal? 'free (car y))
                          (>= (cdr y) (cdr x))))
                   (define free-space (treelist-first-where nodes ok? #:to i))]
             #:when free-space
             #:do [(define free-space-remaining (- (cdr (treelist-ref nodes free-space)) (cdr x)))])
    (let* ([ns (treelist-set (treelist-set nodes i (cons 'free (cdr x))) free-space x)]
           [ns (if (not (zero? free-space-remaining))
                      (treelist-insert ns (add1 free-space) (cons 'free free-space-remaining))
                      ns)])
      #;(print-nodes ns)
      ns)))

(define (explode-to-ids-and-free xs)
  (for/fold ([acc (treelist)]
             [next-id 0]
             #:result acc)
            ([x (in-list xs)]
             [i (in-naturals)])
    (define x-num (- (char->integer x) (char->integer #\0)))
    (cond
      [(even? i)
       (values
        (treelist-append acc (for/treelist ([i (in-range x-num)]) next-id))
        (add1 next-id))]
      [else
       (values
        (treelist-append acc (for/treelist ([i (in-range x-num)]) #\.))
        next-id)])))

; pt2 is easier if i instead make a list like
; [(id_0 size_0) (free size_1) (id_1 (size_2)) ...].
; but that makes checksumming more involved so i just convert this
; list back to the ids-and-free form for that.
(define (explode-to-nodes xs)
  (for/fold ([acc (treelist)]
             [next-id 0]
             #:result acc)
            ([x (in-list xs)]
             [i (in-naturals)])
    (define x-num (- (char->integer x) (char->integer #\0)))
    (cond
      [(even? i)
       (values
        (treelist-add acc (cons next-id x-num))
        (add1 next-id))]
      [else
       (values
        (treelist-add acc (cons 'free x-num))
        next-id)])))

(define (checksum blocks)
  (if (and (not (zero? (treelist-length blocks)))
           (pair? (treelist-ref blocks 0)))
      (checksum (nodes->blocks blocks))
      (for/sum ([b (in-treelist blocks)]
                [i (in-naturals)]
                #:when (not (equal? b #\.)))
        (* b i))))

(define (nodes->blocks nodes)
  (for/fold ([out (treelist)])
            ([n (in-treelist nodes)])
    (treelist-append out
                     (vector->treelist
                      (make-vector(cdr n)
                                   (if (equal? 'free (car n)) #\. (car n)))))))

(define (parse in)
  (explode-to-ids-and-free (string->list (read-line in))))

(define (parse-nodes in)
  (explode-to-nodes (string->list (read-line in))))

(checksum (compact (parse (open-input-string ex))))
(checksum (compact (call-with-input-file "in" parse #:mode 'text)))

(checksum (defragmented-compact (parse-nodes (open-input-string ex))))
(checksum (defragmented-compact (call-with-input-file "in" parse-nodes #:mode 'text)))