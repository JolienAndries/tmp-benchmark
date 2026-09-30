#lang racket
(require "racket-intra-object-access-benchmarks-cached.rkt" "racket-intra-object-access-benchmarks-no-cache.rkt" "racket-classes.rkt" "racket-intra-object-assign-benchmarks.rkt"
         "racket-inter-object-access-benchmark-cached.rkt" "racket-inter-object-access-benchmark-no-cache.rkt" "racket-inter-object-assign-benchmark.rkt")

(define results-folder (if (getenv "NO_PYTHON") "results-no-python/" "results-python/"))

(define (run-benchmarks iter times)
  (displayln "Start racket-intra-object-access-cached...")
  (run-access-benchmark-cached iter times intra-object-1-out intra-object-1-in
                        (string-append results-folder "racket-intra-object-access-cached.csv"))
  (displayln "...done")
  (displayln "Start racket-intra-object-access-no-cache...")
  (run-access-benchmark-no-cache iter times intra-object-1-out intra-object-1-in
                        (string-append results-folder "racket-intra-object-access-no-cache.csv"))
  (displayln "...done")
  (displayln "Start racket-intra-object-assign...")
  (do-assign-benchmark iter times intra-object-1-out intra-object-1-in
                       (string-append results-folder "racket-intra-object-assign.csv"))
  (displayln "...done") 
  (displayln "Start racket-inter-object-access-cached...")
  (run-racket-inter-object-access-benchmarks-cached iter times
                                             (string-append results-folder "racket-inter-object-access-cached.csv"))
  (displayln "...done")
    (displayln "Start racket-inter-object-access-no-cache...")
  (run-racket-inter-object-access-benchmarks-no-cache iter times
                                             (string-append results-folder "racket-inter-object-access-no-cache.csv"))
  (displayln "...done")
  (displayln "Start racket-inter-object-assign ...")
 (run-racket-inter-object-assign-benchmarks iter times
                                             (string-append results-folder "racket-inter-object-assign.csv"))
  (displayln "...done"))

(define (test-benchmarks) (run-benchmarks 5 2))
(define (real-benchmarks) (run-benchmarks 100000 15))

;(test-benchmarks)
(real-benchmarks)