#lang racket
(require "intra-object-access-benchmarks.rkt" "racket-classes.rkt" "racket-intra-object-assign-benchmarks.rkt"
         "racket-inter-object-access-benchmark.rkt" "racket-inter-object-assign-benchmark.rkt")

(define results-folder (if (getenv "NO_PYTHON") "results-no-python/" "results-python/"))

(define (run-benchmarks iter times)
  (displayln "Start racket-intra-object-access...")
  (run-access-benchmark iter times intra-object-1-out intra-object-1-in
                        (string-append results-folder "racket-intra-object-access.csv"))
  (displayln "...done")
  (displayln "Start racket-intra-object-assign...")
  (do-assign-benchmark iter times intra-object-1-out intra-object-1-in
                       (string-append results-folder "racket-intra-object-assign.csv"))
  (displayln "...done")
  (displayln "Start racket-inter-object-access...")
  (run-racket-inter-object-access-benchmarks iter times
                                             (string-append results-folder "racket-inter-object-access.csv"))
  (displayln "...done")
  (displayln "Start racket-inter-object-assign ...")
  (run-racket-inter-object-assign-benchmarks iter times
                                             (string-append results-folder "racket-inter-object-assign.csv"))
  (displayln "...done"))

(define (test-benchmarks) (run-benchmarks 1 1))
(define (real-benchmarks) (run-benchmarks 10000 15))

;;(test-benchmarks)
(real-benchmarks)