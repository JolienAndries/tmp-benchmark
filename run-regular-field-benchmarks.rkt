#lang racket
(require racket/class
         "benchmark-results.rkt")
(provide run-regular-field-benchmarks)

(define results-folder (if (getenv "NO_PYTHON") "results-no-python/" "results-python/"))


(define regular-field-class%
  (class object%
    (field [field1 0])
    (super-new)))

(define (benchmark-get-field object iter)
  (do ((i 1 (+ i 1)))
    ((> i iter))
    (get-field field1 object)))

(define (benchmark-set-field! object iter)
  (do ((i 1 (+ i 1)))
    ((> i iter))
    (set-field! field1 object i)))

(define (run-case output benchmark-name benchmark object iter times)
  (do ((iteration 1 (+ iteration 1)))
    ((> iteration times))
    (record-benchmark output benchmark-name 1 1 iter
                      (lambda () (benchmark object iter)))))

(define (run-regular-field-benchmarks iter times
                                      [output-path (string-append results-folder "regular-field-benchmarks.csv")])
  (displayln "run regular field benchmark")
  (with-benchmark-results output-path
    (lambda (output)
      (define object (new regular-field-class%))
      (run-case output "regular-get-field" benchmark-get-field object iter times)
      (run-case output "regular-set-field!" benchmark-set-field! object iter times)))
  (displayln "done running"))



(run-regular-field-benchmarks 1000000 15)
