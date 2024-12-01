#lang racket

(require racket/treelist)
(require racket/string)

(define ex #<<>>
3   4
4   3
2   5
1   3
3   9
3   3
>>
  )

(define (string-empty? s) (zero? (string-length s)))

(define (parse in)
  (define lines
    (for/list ([line (in-lines in 'any)])
      (for/list ([num (in-list (string-split line))])
        (string->number num))))
  (for/fold ([lists (cons empty-treelist empty-treelist)])
            ([l (in-list lines)])
    (cons
     (treelist-add (car lists) (car l))
     (treelist-add (cdr lists) (cadr l)))))

(define (dists cols)
  (for/sum ([c1 (treelist-sort (car cols) <)]
            [c2 (treelist-sort (cdr cols) <)])
    (abs (- c1 c2))))

(define (pt-1 in)
  (define cols (parse in))
  (dists cols))

(define (pt-2 in)
  (define cols (parse in))
  (define histo
    (for/fold ([m (make-immutable-hash)])
              ([x (in-treelist (cdr cols))])
      (hash-update m x add1 (thunk 0))))
  (for/sum ([x (in-treelist (car cols))])
    (* x (hash-ref histo x 0))))

(pt-1 (open-input-string ex))
(call-with-input-file "in" pt-1 #:mode 'text)

(pt-2 (open-input-string ex))
(call-with-input-file "in" pt-2 #:mode 'text)
