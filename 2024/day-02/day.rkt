#lang racket

(require racket/treelist)

(define ex #<<>>
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
>>
  )

(define (parse-reports in)
  (for/list ([l (in-lines in 'any)])
    (for/treelist ([num (in-list (string-split l))])
      (string->number num))))

(define (report-safe? rpt)
  (define (all-pairwise op)
    (for/and ([x (in-treelist rpt)]
              [y (in-treelist (treelist-drop rpt 1))])
      (op x y)))

  (and (or (all-pairwise <) (all-pairwise >))
       (all-pairwise (lambda (x y)
                       (let ([d (abs (- x y))])
                         (and (>= d 1) (<= d 3)))))))

(define (safe-reports is-safe? rpts)
  (for/sum ([rpt (in-list rpts)])
    (if (is-safe? rpt) 1 0)))

(define (dampened-report-safe? rpt)
  (or
   (report-safe? rpt)
   (for/or ([i (in-range (treelist-length rpt))])
     (report-safe? (treelist-delete rpt i)))))

(safe-reports report-safe? (parse-reports (open-input-string ex)))
(safe-reports report-safe? 
              (call-with-input-file "in" parse-reports #:mode 'text))

(safe-reports dampened-report-safe? (parse-reports (open-input-string ex)))
(safe-reports dampened-report-safe?
              (call-with-input-file "in" parse-reports #:mode 'text))