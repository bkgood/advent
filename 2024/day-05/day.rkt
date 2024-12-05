#lang racket

(define ex #<<>>
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
>>
  )

; a|b -> a must come before b.

(define (parse in)
  (let ([raw-rules (for/list ([l (in-lines in)]
                              #:break (zero? (string-length l)))
                     (cons (substring l 0 2)
                           (substring l 3 5)))]
        [updates (for/list ([l (in-lines in)]) l)])
    (values raw-rules updates)))

(define (rules-to-regex rules)
  (for/fold ([pats '()]
             #:result (pregexp (string-join (reverse pats) "|")))
            ([r (in-list rules)])
    (cons (format "(~a).+(~a)" (cdr r) (car r)) pats)))

(define (check update rules)
  (not (regexp-match rules update)))

(define (middle-page updates)
  (define half (/ (string-length updates) 2))
  (string->number (substring updates (sub1 half) (add1 half))))

(define (ok-updates rules updates #:pred [pred identity])
  (for/list ([u (in-list updates)]
             #:when (pred (check u rules)))
    u))

(define (sum-middle-pages updates)
  (for/sum ([u (in-list updates)])
    (middle-page u)))

(define (pt1 in)
  (sum-middle-pages (call-with-values (thunk
                                       (define-values (raw-rules updates) (parse in))
                                       (values (rules-to-regex raw-rules) updates))
                                      ok-updates)))

(define (explode-update up)
  (list->vector (string-split up ",")))

(define (rejoin-update xs)
  (string-join (vector->list xs) ","))

(define update-member
  (case-lambda
    [(xs x) (update-member xs x 0)]
    [(xs x from)
     (for/or ([i (in-range from (vector-length xs))])
       (and (equal? (vector-ref xs i) x) i))]))

(define (vector-advance! xs #:dest dst #:src src)
  (when (< src dst)
    (error 'advance-src-before-dst))
  (define x (vector-ref xs src))
  (vector-copy! xs (add1 dst) xs dst src)
  (vector-set! xs dst x)
  xs)

(define (enforce-rule! r xs)
  (let* ([first (car r)]
         [second (cdr r)]
         [found-second (update-member xs second)]
         [found-first-after-second (and found-second (update-member xs first found-second))])
    (and found-first-after-second
         (vector-advance! xs #:dest found-second #:src found-first-after-second))))

(define (fix-updates rules not-ok-updates)
  (define (fix-update u)
    (define ul (explode-update u))
    (let loop ([n 0])
      (let ([ok (for/fold ([ok 0])
                          ([r (in-list rules)])
                  (define e (enforce-rule! r ul))
                  (cond
                    [(not e) (add1 ok)]
                    [else ok]))])
        (when (not (= ok (length rules)))
          (when (> n 100)
            (error 'too-many-iterations))
          (loop (add1 n)))))
    (rejoin-update ul))
  (map fix-update not-ok-updates))

(define (pt2 in)
  (define-values (raw-rules updates) (parse in))
  (define combined-rule (rules-to-regex raw-rules))
  (define not-ok-updates (ok-updates combined-rule updates #:pred not))
  (define updates-fixed (fix-updates raw-rules not-ok-updates))
  (sum-middle-pages updates-fixed))

(pt1 (open-input-string ex))
(call-with-input-file "in" pt1 #:mode 'text)

(pt2 (open-input-string ex))
(call-with-input-file "in" pt2 #:mode 'text)