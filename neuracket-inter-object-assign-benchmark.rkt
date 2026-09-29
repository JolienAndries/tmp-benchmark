#lang racket
(provide run-neuracket-inter-object-assign-benchmarks)
(require "neuracket-classes.rkt" "neuracket-neural-slices.rkt" "benchmark-results.rkt"
         (for-syntax racket/base racket/syntax))


;; lists of slices.
(define-syntax (input-slice-list stx)
  (with-syntax ([(slice ...)
                 (for/list ([count (in-range 1 100 2)])
                   (format-id stx "~a-in-1-out-slice" count))])
    #'(list slice ...)))

(define-syntax (output-slice-list stx)
  (with-syntax ([(slice ...)
                 (for/list ([count (in-range 3 50 2)])
                   (format-id stx "1-in-~a-out-slice" count))])
    #'(list slice ...)))

;; ( (lambda (obj .. ) (set-fields! (lbl ..) (obj ...) (vals ...))) ... ) -> x objs, lbls, vals and x goes from 3 to 49 steps of 2 (+2 for each lambda)
(define-syntax (output-assigner-list stx)
  (with-syntax ([(assigner ...)
                 (for/list ([count (in-range 3 50 2)])
                   (define objects
                     (for/list ([index (in-range 1 (add1 count))])
                       (format-id stx "object~a" index)))
                   (define labels
                     (for/list ([index (in-range 1 (add1 count))])
                       (format-id stx "lbl~a" index)))
                   (define values (for/list ([index (in-range 1 (add1 count))]) index))
                   #`(lambda (#,@objects)
                       (set-fields! (#,@labels)
                                    (#,@objects)
                                    (#,@values))))])
    #'(list assigner ...)))

(define (run-neuracket-inter-object-assign-benchmarks iter times
                                                      [output-path "neuracket-inter-object-assign.csv"])
  (with-benchmark-results output-path
    (lambda (output)
      (for ([count (in-range 1 100 2)] [slice (input-slice-list)])
        (define inputs (build-list count (lambda (_) (new inter-object-class%))))
        (define output-object (new inter-object-class%))
        (apply new-neural-slice slice (append inputs (list output-object)))
        (do ((iteration 1 (+ iteration 1)))
          ((> iteration times))
          (record-benchmark output "inter-object-assign" count 1 iter
                            (lambda ()
                              (do ((i 1 (+ i 1)))
                                ((> i iter))
                                (set-field! lbl1 output-object 42))))))
      (for ([count (in-range 3 50 2)] [slice (output-slice-list)]
                                      [assigner (output-assigner-list)])
        (define input-object (new inter-object-class%))
        (define output-objects (build-list count (lambda (_) (new inter-object-class%))))
        (apply new-neural-slice slice (cons input-object output-objects))
        (do ((iteration 1 (+ iteration 1)))
          ((> iteration times))
          (record-benchmark output "inter-object-assign" 1 count iteration
                            (lambda ()
                              (do ((i 1 (+ i 1)))
                                ((> i iter))
                                (apply assigner output-objects)))))))))
