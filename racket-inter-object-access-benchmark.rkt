#lang racket
(provide run-racket-inter-object-access-benchmarks)
(require racket/class "racket-classes.rkt" "benchmark-results.rkt"
         (for-syntax racket/base racket/syntax))

;; ( (lambda (object inputs) (send/apply object infer-neural-x-in-1-out inputs)) ..)
(define-syntax (input-method-list stx)
  (with-syntax ([(proc ...)
                 (for/list ([count (in-range 1 100 2)])
                   (define method (format-id stx "infer-neural-~a-in-1-out" count))
                   #`(lambda (object inputs) (send/apply object #,method inputs)))])
    #'(list proc ...)))

;; same as above but ranging over outputs 
(define-syntax (output-method-list stx)
  (with-syntax ([(proc ...)
                 (for/list ([count (in-range 3 50 2)])
                   (define method (format-id stx "infer-neural-1-in-~a-out!" count))
                   #`(lambda (object input outputs) (send/apply object #,method input outputs)))])
    #'(list proc ...)))

;; (list (list (lambda (object) (get-field  external-neuralx  object)) ...) ...)) list of longer getting lists of lambdas accessing external neural fields 

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

(define (run-racket-inter-object-access-benchmarks iter times
                                                   [output-path "racket-inter-object-access.csv"])
  (with-benchmark-results output-path
    (lambda (output)
      (for ([count (in-range 1 100 2)]
            [method (input-method-list)])
        (define object (new inter-object-class%))
        (define inputs (build-list count (lambda (_) (new inter-object-class%))))
        (for ([iteration (in-range 1 (add1 times))])
          (record-benchmark output "inter-object-access" count 1 iter
                            (lambda ()
                              (method object inputs)
                              (do-access (list object)
                                         (list (lambda (value)
                                                 (get-field external-neural value)))
                                         iter)))))
      (for ([count (in-range 3 50 2)]
            [method (output-method-list)])
        (define object (new inter-object-class%))
        (define input (new inter-object-class%))
        (define outputs (build-list (sub1 count) (lambda (_) (new inter-object-class%))))
        (define objects (cons object outputs))
        (for ([iteration (in-range 1 (add1 times))])
          (let ((function (list-ref (field-function-lists) (/ (- count 3) 2))))
          (record-benchmark output "inter-object-access" 1 count iter
                            (lambda ()
                              (method object input outputs)
                              (do-access objects
                                         function
                                         iter)))))))))

