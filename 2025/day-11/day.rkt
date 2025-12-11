#lang racket/base

(require racket/string
         racket/list
         racket/port
         racket/function)

(define ex #<<>>
aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out
>>
  )

(define ex2 #<<>>
svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out
>>
  )

(define (parse-input in)
  (for/hasheq ([l (in-lines in)])
    (let* ([colon-split (string-split l ": ")]
           [node (string->symbol (first colon-split))]
           [conns (map string->symbol (sort (string-split (second colon-split) " ") string<?))])
      (values node conns))))

(define (pt1 in)
  (define graph (parse-input in))
  (let loop ([nodes (hash-ref graph 'you)]
             [path (list 'you)])
    (cond
      [(eq? (first nodes) 'out) 1]
      [else (for/sum ([x (in-list nodes)])
              (loop (hash-ref graph x) (cons x path)))])))

(module+ test
  (require rackunit)

  (check-equal? (call-with-input-string ex pt1) 5)
  (check-equal? (call-with-input-file "in" pt1) 636))

(define (pt2 in)
  (define graph (parse-input in))
  (define cache (make-hash))

  (let loop ([path (list 'svr)]
             [have-fft #f]
             [have-dac #f])
    (define nodes (hash-ref graph (first path)))
    (cond
      [(eq? (first nodes) 'out)
       (if (and have-dac have-fft) 1 0)]
      [else
       (for/sum ([child (in-list nodes)])
         (hash-ref! cache (vector child have-fft have-dac)
                    (thunk
                     (loop (cons child path)
                           (or have-fft (eq? child 'fft))
                           (or have-dac (eq? child 'dac))))))])))

(module+ test
  (require rackunit)

  (check-equal? (call-with-input-string ex2 pt2) 2)
  (check-equal? (call-with-input-file "in" pt2) 509312913844956))
