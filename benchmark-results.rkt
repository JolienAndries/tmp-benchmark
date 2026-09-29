#lang racket
(provide with-benchmark-results record-benchmark)

(define (with-benchmark-results path thunk)
  (call-with-output-file path
    (lambda (output)
      (fprintf output "benchmark,input_count,output_count,iteration,cpu_ms,real_ms,gc_ms\n")
      (thunk output))
    #:exists 'replace))

(define (record-benchmark output benchmark input-count output-count iteration thunk)
   
  (define-values (_ cpu-ms real-ms gc-ms) (time-apply thunk '()))
  (fprintf output "~a,~a,~a,~a,~a,~a,~a\n"
           benchmark input-count output-count iteration cpu-ms real-ms gc-ms))
