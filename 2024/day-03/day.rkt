#lang racket

(define ex "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")
(define ex-dont "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(define (ascii-bytes->number bs)
  (string->number (bytes->string/utf-8 bs)))

(define (read-match in)
  (define m (regexp-match #px"(?:(mul)[(]([0-9]{,3}),([0-9]{,3})[)]|(don't|do)[(][)])" in))
  (cond
    [(not m) #f]
    [(cadr m) (list * (ascii-bytes->number (caddr m)) (ascii-bytes->number (cadddr m)))]
    [else (let* ([toggle-bytes (list-ref m 4)]
                 [toggle (bytes->string/utf-8 toggle-bytes)])
            (case toggle
              [("don't") '(off)]
              [("do") '(on)]
              [else (error "unknown toggle" toggle)]))]))

(define (filter-toggles instrs)
  (for/list ([i (in-list instrs)]
             #:when (eq? (first i) *))
    i))

(define (apply-toggles instrs)
  (for/fold ([acc '()]
             [dropping #f]
             #:result acc)
            ([i (in-list instrs)])
    (case (first i)
      [(on) (values acc #f)]
      [(off) (values acc #t)]
      [else (values (if dropping
                        acc
                        (cons i acc))
                    dropping)])))

(define (parse in)
  (let loop ([matches '()])
    (cond
      [(eof-object? (peek-char in)) (reverse matches)]
      [else
       (let* ([m (read-match in)]
              [matches (if m (cons m matches) matches)])
         (loop matches))])))

(define (exec instrs)
  (for/sum ([stmt (in-list instrs)])
    (apply (first stmt) (rest stmt))))

(exec (filter-toggles (parse (open-input-string ex))))
(exec (filter-toggles (call-with-input-file "in" parse #:mode 'text)))

(exec (apply-toggles (parse (open-input-string ex-dont))))
(exec (apply-toggles (call-with-input-file "in" parse #:mode 'text)))
