#lang racket
(provide run-racket-inter-object-assign-benchmarks)
(require racket/class "racket-classes.rkt" "benchmark-results.rkt"
         (for-syntax racket/base racket/syntax))

;; list of functions invoking train input ranging
(define-syntax (input-method-list stx)
  (with-syntax ([(method ...)
                 (for/list ([count (in-range 1 100 2)])
                   (define method (format-id stx "train-neural-~a-in-1-out" count))
                   #`(lambda (object inputs) (send/apply object #,method inputs)))])
    #'(list method ...)))

;; same as above but with output ranging
(define-syntax (output-method-list stx)
  (with-syntax ([(method ...)
                 (for/list ([count (in-range 3 50 2)])
                   (define method (format-id stx "train-neural-1-in-~a-out" count))
                   #`(lambda (object input outputs) (send/apply object #,method input outputs)))])
    #'(list method ...)))

(define (assign-fields objects)
  (for-each (lambda (object) (set-field! external-neural object 42)) objects))

(define (run-racket-inter-object-assign-benchmarks iter times
                                                   [output-path "racket-inter-object-assign.csv"])
  (with-benchmark-results output-path
    (lambda (output)
      (for ([count (in-range 1 100 2)]
            [method (input-method-list)])
        (define object (new inter-object-class%))
        (define inputs (build-list count (lambda (_) (new inter-object-class%))))
        (do ((iteration 1 (+ iteration 1)))
          ((> iteration times))
          (record-benchmark output "inter-object-assign" count 1 iter
                            (lambda ()
                              (do ((i 1 (+ i 1)))
                                ((> i iter))
                                (set-field! external-neural object 42)
                                (method object inputs))))))
      (for ([count (in-range 3 50 2)]
            [method (output-method-list)])
        (define object (new inter-object-class%))
        (define input (new inter-object-class%))
        (define outputs (build-list (sub1 count) (lambda (_) (new inter-object-class%))))
        (define objects (cons object outputs))
        (do ((iteration 1 (+ iteration 1)))
          ((> iteration times))
          (record-benchmark output "inter-object-assign" 1 count iter
                            (lambda ()
                              (do ((i 1 (+ i 1)))
                                ((> i iter))
                                (assign-fields objects)
                                (method object input outputs)))))))))
