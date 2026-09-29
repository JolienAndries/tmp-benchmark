#lang racket
(provide do-assign-benchmark)
(require "benchmark-results.rkt"
         (for-syntax racket/base racket/syntax))

;; define benchmark , iter times: output-count times set-field! of a neural var, then invoke a train on the object
(define-syntax (define-assign-benchmark stx)
  (syntax-case stx ()
    [(_ name input-count output-count)
     (let* ([inputs (syntax-e #'input-count)]
            [outputs (syntax-e #'output-count)]
            [fields (if (= outputs 1)
                        (list (format-id stx "neural~a" inputs))
                        (for/list ([index (in-range 1 (add1 outputs))])
                          (format-id stx "neural~a.~a" outputs index))) ]
            [method (if (= outputs 1)
                        (format-id stx "train-neural~a" inputs)
                        (format-id stx "train-neurals~a" outputs))])
       #`(define (name object iter)
           (do ((i 1 (+ i 1)))
             ((> i iter))
             #,@(for/list ([field fields] [value (in-naturals 1)])
                  #`(set-field! #,field object #,value))
             (send object #,method))))]))

(define-syntax (define-all-assign-benchmarks stx)
  (with-syntax ([(input-definition ...)
                 (for/list ([count (in-range 1 100 2)])
                   #`(define-assign-benchmark
                       #,(format-id stx "benchmark-assign-~a-in-1-out" count)
                       #,count 1))]
                [(output-definition ...)
                 (for/list ([count (in-range 3 50 2)])
                   #`(define-assign-benchmark
                       #,(format-id stx "benchmark-assign-1-in-~a-out" count)
                       1 #,count))])
    #'(begin input-definition ... output-definition ...)))

(define-all-assign-benchmarks)

(define input-counts '(1 3 5 7 9 11 13 15 17 19 21 23 25 27 29 31 33 35 37 39 41 43 45 47 49 51 53 55 57 59 61 63 65 67 69 71 73 75 77 79 81 83 85 87 89 91 93 95 97 99))
(define output-counts '(3 5 7 9 11 13 15 17 19 21 23 25 27 29 31 33 35 37 39 41 43 45 47 49))

;; list of benchmark names defined above
(define-syntax (input-benchmark-list stx)
  (with-syntax ([(name ...)
                 (for/list ([count (in-range 1 100 2)])
                   (format-id stx "benchmark-assign-~a-in-1-out" count))])
    #'(list name ...)))

(define-syntax (output-benchmark-list stx)
  (with-syntax ([(name ...)
                 (for/list ([count (in-range 3 50 2)])
                   (format-id stx "benchmark-assign-1-in-~a-out" count))])
    #'(list name ...)))

(define (run-case output benchmark-name benchmark input-count output-count object iter times)
  (for ([iteration (in-range 1 (add1 times))])
    (record-benchmark output benchmark-name input-count output-count iter
                      (lambda () (benchmark object iter)))))

(define (do-assign-benchmark iter times object-1-out object-1-in
                             [output-path "racket-intra-object-assign.csv"])
  (with-benchmark-results output-path
    (lambda (output)
      (for ([input-count input-counts] [benchmark (input-benchmark-list)])
        (run-case output "intra-object-assign" benchmark input-count 1 object-1-out iter times))
      (for ([output-count output-counts] [benchmark (output-benchmark-list)])
        (run-case output "intra-object-assign" benchmark 1 output-count object-1-in iter times)))))
