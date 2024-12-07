#lang racket

(require racket/treelist)

(define ex #<<>>
190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
>>
  )

; insert + or * between numbers after colon to get number before colon

(define (num-digits x)
  (let loop ([x x]
             [n 1])
    (define q (truncate (/ x 10)))
    (cond
      [(zero? q) n]
      [else (loop q (add1 n))])))

(define (|| x y)
  (+ (* x (expt 10 (num-digits y))) y))

(define (eval xs)
  (define (handle op res x)
    (cond
      [op (values (op res x) #f)]
      [else (values res x)]))
  (cond
    [(treelist? xs)
     (for/fold ([res (treelist-ref xs 0)]
                [op #f]
                #:result res)
               ([x (in-treelist (treelist-drop xs 1))])
       (handle op res x))]
    [else
     (for/fold ([res (first xs)]
                [op #f]
                #:result res)
               ([x (in-list (rest xs))])
       (handle op res x))]))

(define (interleave xs x)
  (rest (flatten (map (lambda (a) (cons x a)) xs))))

(define (indexes-of xs y)
  (for/list ([x (in-treelist xs)]
             [i (in-naturals)]
             #:when (eq? x y))
    i))

(define (treelist-set-many tl is x)
  (cond
    [(null? is) tl]
    [else (treelist-set-many (treelist-set tl (first is) x) (rest is) x)]))

(define (passes? case)
  (let* ([expected (car case)]
         [expr (interleave (cdr case) +)]
         [expr-tl (list->treelist expr)])
    (for/or ([c (in-combinations (indexes-of expr-tl +))])
      (= expected (eval (treelist-set-many expr-tl c *))))))

(define (passes-2? case)
  (let* ([expected (car case)]
         [expr (interleave (cdr case) +)]
         [expr-tl (list->treelist expr)])
    (define indexes (indexes-of expr-tl +))
    (for*/or ([c|| (in-combinations indexes)]
              [c* (in-combinations indexes)])
      (= expected
         (eval
          (treelist-set-many (treelist-set-many expr-tl c|| ||)
                             c* *))))))

(define (sum-ok-tvals ok? cases)
  (for/sum ([case (in-list cases)]
            #:when (ok? case))
    (car case)))

(define (pt1 cases) (sum-ok-tvals passes? cases))
(define (pt2 cases) (sum-ok-tvals passes-2? cases))

(define (parse in)
  (for/list ([l (in-lines in)])
    (let ([nums (map string->number (string-split l #px":?\\s+"))])
      (cons (first nums)
            (rest nums)))))

(pt1 (parse (open-input-string ex)))
(pt1 (call-with-input-file "in" parse #:mode 'text))

(pt2 (parse (open-input-string ex)))
(pt2 (call-with-input-file "in" parse #:mode 'text))
