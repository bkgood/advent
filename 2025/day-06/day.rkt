#lang racket/base

(require racket/port
         racket/string
         racket/list)

(define ex #<<>>
123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  
>>
  )

(define (parse-input in)
  (for/fold ([out #f])
            ([line (in-lines in)])
    (let ([split (string-split line)])
      (cond
        [(not out) (list->vector (map list split))]
        [else (for ([x (in-list split)]
                    [i (in-naturals)])
                (vector-set! out i (cons x (vector-ref out i))))
              out]))))

(define (parse-columnar-input in)
  (for/fold ([cols #f])
            ([line (in-lines in)])
    (cond
      [(not cols) (for/vector #:length (string-length line)
                    ([c (in-string line)])
                    (list(string c)))]
      [else (for ([c (in-string line)]
                  [i (in-naturals)])
              (vector-set! cols i (cons (string c) (vector-ref cols i))))
            cols])))

(define (string->op s)
  (case s
    [("+") +]
    [("*") *]
    [else (error (format "unknown op: ~a" s))]))

(define (pt1 in)
  (for/sum ([xs (in-vector (parse-input in))])
    (let ([op (string->op (first xs))])
      (apply op (map string->number (rest xs))))))

(module+ test
  (require rackunit)

  (check-equal? (call-with-input-string ex pt1) 4277556)
  (check-equal? (call-with-input-file "in" pt1) 7098065460541))

(define (col-empty? col)
  (for/and ([s (in-list col)])
    (string=? " " s)))

(define (join-digits-to-integer xs)
  (for/fold ([out 0])
            ([x (in-list (reverse xs))]
             #:when (not (string=? x " ")))
    (+ (* 10 out) (string->number x))))

(define (pt2 in)
  (define (flush pending op sum)
    (values '()
            #f
            (+ sum (apply op pending))))
  (for/fold ([pending '()]
             [op #f]
             [sum 0]
             #:result (let-values ([(_ __ sum) (flush pending op sum)]) sum))
            ([col (in-vector (parse-columnar-input in))])
    (cond
      [(col-empty? col) (flush pending op sum)]
      [(not op)
       (let ([new-op (string->op (first col))]
             [num (join-digits-to-integer (rest col))])
          (values (cons num pending) new-op sum))]
      [else (values (cons (join-digits-to-integer col) pending) op sum)])))

(module+ test
  (check-equal? (call-with-input-string ex pt2) 3263827)
  (check-equal? (call-with-input-file "in" pt2) 13807151830618))
