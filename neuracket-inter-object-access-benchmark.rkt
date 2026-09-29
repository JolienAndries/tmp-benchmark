#lang racket
(provide run-neuracket-inter-object-access-benchmarks)
(require "neuracket-classes.rkt" "neuracket-neural-slices.rkt" "benchmark-results.rkt"
         (for-syntax racket/base racket/syntax))

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

;; (((lambda (object) (get-field external-neuralx object)) ...) ...) x = 3,5,7,...49 aka a list of lists that get longer (first sublist = 3 ; second = 5 ; ...)
(define-syntax (field-function-lists stx)
  (with-syntax ([(function-list ...)
                 (for/list ([count (in-range 3 50 2)])
                   (with-syntax ([(function ...)
                                  (for/list ([index (in-range 1 (add1 count))])
                                    (define field (if (= index 1)
                                                      (format-id stx "external-neural")
                                                      (format-id stx "external-neural~a" index)))
                                    #`(lambda (object) (get-field #,field object)))])
                     #'(list function ...)))])
    #'(list function-list ...)))

(define (do-access objects functions iter)
  (do ((i 1 (+ i 1)))
    ((> i iter))
    (for-each (lambda (function object) (function object)) functions objects)))

(define (run-neuracket-inter-object-access-benchmarks iter times
                                                      [output-path "neuracket-inter-object-access.csv"])
  (with-benchmark-results output-path
    (lambda (output)
      (for ([count (in-range 1 100 2)] [slice (input-slice-list)])
        (define inputs (build-list count (lambda (_) (new inter-object-class%))))
        (define output-object (new inter-object-class%))
        (apply new-neural-slice slice (append inputs (list output-object)))
        (for ([iteration (in-range 1 (add1 times))])
          (record-benchmark output "inter-object-access" count 1 iteration
                            (lambda () (do-access (list output-object)
                                                  (list (lambda (object)
                                                          (get-field external-neural object)))
                                                  iter)))))
      (for ([count (in-range 3 50 2)] [slice (output-slice-list)])
        (define input-object (new inter-object-class%))
        (define output-objects (build-list count (lambda (_) (new inter-object-class%))))
        (apply new-neural-slice slice (cons input-object output-objects))
        (do ((iteration 1 (+ iteration 1)))
          ((> iteration times))
          (record-benchmark output "inter-object-access" 1 count iter
                            (lambda () (do-access output-objects
                                                  (list-ref (field-function-lists)
                                                            (/ (- count 3) 2)) ;; count starts at 3, and steps +2 -> convert to listref aka 0 1 2 ...
                                                  iter))))))))
