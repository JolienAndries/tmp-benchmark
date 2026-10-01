#lang racket

(require "neuracket-intra-object-access-benchmarks-cached.rkt" "neuracket-intra-object-access-benchmarks-no-cache.rkt" "neuracket-classes.rkt" "neuracket-intra-object-assign-benchmarks.rkt"
         "neuracket-inter-object-access-benchmark-cached.rkt" "neuracket-inter-object-access-benchmark-no-cache.rkt" "neuracket-inter-object-assign-benchmark.rkt")

(define results-folder (if (getenv "NO_PYTHON") "results-no-python/" "results-python/"))


(define (run-benchmarks iter times)
  (displayln "start evaluating neuracket-intra-object-access-cached ...")
  (run-access-benchmark-cached iter times intra-object-1-out intra-object-1-in
                               (string-append results-folder "neuracket-intra-object-access-cached.csv"))
  (displayln "...done evaluating")
  (displayln "start evaluating neuracket-intra-object-access-no-cache ...")
  (run-access-benchmark-no-cache iter times intra-object-1-out intra-object-1-in
                                 (string-append results-folder "neuracket-intra-object-access-no-cache.csv"))
  (displayln "...done evaluating") 
  
  (displayln "start evaluating neuracket-intra-object-assign ...")
  (do-assign-benchmark iter times intra-object-1-out intra-object-1-in
                       (string-append results-folder  "neuracket-intra-object-assign.csv"))
  (displayln "...done evaluating") 
  
  (displayln "start evaluating neuracket-inter-object-access-cached ...")
  (run-neuracket-inter-object-access-benchmarks-cached iter times
                                                       (string-append results-folder "neuracket-inter-object-access-cached.csv"))
  (displayln "...done evaluating")
  (displayln "start evaluating neuracket-inter-object-access-no-cache ...")
  (run-neuracket-inter-object-access-benchmarks-no-cache iter times
                                                         (string-append results-folder "neuracket-inter-object-access-no-cache.csv"))
  (displayln "...done evaluating") 
  
  (displayln "start evaluating neuracket-inter-object-assign ...")
  (run-neuracket-inter-object-assign-benchmarks iter times
                                                (string-append results-folder "neuracket-inter-object-assign.csv"))
  (displayln "...done evaluating"))


(define (run-cached-access-benchmarks iter times)
  (displayln "start evaluating neuracket-intra-object-access-cached ...")
  (run-access-benchmark-cached iter times intra-object-1-out intra-object-1-in
                               (string-append results-folder "neuracket-intra-object-access-cached.csv"))
  (displayln "...done evaluating")
    
  (displayln "start evaluating neuracket-inter-object-access-cached ...")
  (run-neuracket-inter-object-access-benchmarks-cached iter times
                                                       (string-append results-folder "neuracket-inter-object-access-cached.csv"))
  (displayln "...done evaluating"))

(define (run-no-cache-access-benchmarks iter times)

  (displayln "start evaluating neuracket-intra-object-access-no-cache ...")
  (run-access-benchmark-no-cache iter times intra-object-1-out intra-object-1-in
                                 (string-append results-folder "neuracket-intra-object-access-no-cache.csv"))
  (displayln "...done evaluating") 
  
  (displayln "start evaluating neuracket-inter-object-access-no-cache ...")
  (run-neuracket-inter-object-access-benchmarks-no-cache iter times
                                                         (string-append results-folder "neuracket-inter-object-access-no-cache.csv"))
  (displayln "...done evaluating"))
  


(define (test-benchmarks) (run-benchmarks 5 2))
(define (real-benchmarks)
  (run-cached-access-benchmarks 100000 15)
  (run-no-cache-access-benchmarks 10000 15))

;(test-benchmarks)
(real-benchmarks)
