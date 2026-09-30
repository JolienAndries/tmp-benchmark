#lang racket

(require "neuracket-intra-object-access-benchmarks.rkt" "neuracket-classes.rkt" "neuracket-intra-object-assign-benchmarks.rkt"
         "neuracket-inter-object-access-benchmark.rkt" "neuracket-inter-object-assign-benchmark.rkt")

(define results-folder (if (getenv "NO_PYTHON") "results-no-python/" "results-python/"))


(define (run-benchmarks iter times)
  (displayln "start evaluating neuracket-intra-object-access ...")
  (run-access-benchmark iter times intra-object-1-out intra-object-1-in
                        (string-append results-folder "neuracket-intra-object-access.csv"))
  (displayln "...done evaluating") 
  
  (displayln "start evaluating neuracket-intra-object-assign ...")
  (do-assign-benchmark iter times intra-object-1-out intra-object-1-in
                       (string-append results-folder  "neuracket-intra-object-assign.csv"))
  (displayln "...done evaluating") 
  
  (displayln "start evaluating neuracket-inter-object-access ...")
  (run-neuracket-inter-object-access-benchmarks iter times
                                                (string-append results-folder "neuracket-inter-object-access.csv"))
  (displayln "...done evaluating") 
  
  (displayln "start evaluating neuracket-inter-object-assign ...")
  (run-neuracket-inter-object-assign-benchmarks iter times
                                                (string-append results-folder "neuracket-inter-object-assign.csv"))
  (displayln "...done evaluating"))
  


(define (test-benchmarks) (run-benchmarks 2 1))
(define (real-benchmarks) (run-benchmarks 10000 15))

;(test-benchmarks)
(real-benchmarks)
