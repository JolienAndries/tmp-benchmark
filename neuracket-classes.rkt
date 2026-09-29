#lang racket
(provide intra-object-1-out intra-object-1-in inter-object-class%)
(require "neuracket-ML-models.rkt"
         (for-syntax racket/base racket/syntax))

(define-syntax (define-label-fields stx)
  (syntax-case stx ()
    [(_ counts)
     (with-syntax ([(field ...)
                    (for/list ([count (in-list (syntax->datum #'counts))])
                      (format-id stx "lbl~a" count))])
       #'(label-field [field 5] ...))]))

(define-syntax (define-input-neural-fields stx)
  (syntax-case stx ()
    [(_ counts)
     (with-syntax ([(entry ...)
                    (for/list ([count (in-list (syntax->datum #'counts))])
                      (define field (format-id stx "neural~a" count))
                      (define model (format-id stx "model-~a-in-1-out" count))
                      (define inputs
                        (for/list ([index (in-range count)])
                          (list-ref (list (format-id stx "a")
                                          (format-id stx "b")
                                          (format-id stx "c"))
                                    (modulo index 3))))
                      (define label (format-id stx "lbl~a" count))
                      #`[(#,field) #,model (#,@inputs) (#,label)])])
       #'(neural-field entry ...))]))

(define-syntax (define-output-label-fields stx)
  (syntax-case stx ()
    [(_ counts)
     (with-syntax ([(field ...)
                    (apply append
                           (for/list ([count (in-list (syntax->datum #'counts))])
                             (for/list ([index (in-range 1 (add1 count))])
                               (format-id stx "lbl~a.~a" count index))))])
       #'(label-field [field 5] ...))]))

(define-syntax (define-output-neural-fields stx)
  (syntax-case stx ()
    [(_ counts)
     (with-syntax ([(entry ...)
                    (for/list ([count (in-list (syntax->datum #'counts))])
                      (define fields
                        (for/list ([index (in-range 1 (add1 count))])
                          (format-id stx "neural~a.~a" count index)))
                      (define model (format-id stx "model-1-in-~a-out" count))
                      (define labels
                        (for/list ([index (in-range 1 (add1 count))])
                          (format-id stx "lbl~a.~a" count index)))
                      #`[(#,@fields) #,model (#,(format-id stx "in")) (#,@labels)])])
       #'(neural-field entry ...))]))

(define-syntax (define-external-fields stx)
  (syntax-case stx ()
    [(_ count)
     (with-syntax ([(field ...)
                    (for/list ([index (in-range 1 (add1 (syntax-e #'count)))])
                        (if (= index 1)
                          (format-id stx "external-neural")
                          (format-id stx "external-neural~a" index)))])
       #'(external-neural-field field ...))]))

(define odd-input-counts '(1 3 5 7 9 11 13 15 17 19 21 23 25 27 29 31 33 35 37 39 41 43 45 47 49 51 53 55 57 59 61 63 65 67 69 71 73 75 77 79 81 83 85 87 89 91 93 95 97 99))
(define odd-output-counts '(3 5 7 9 11 13 15 17 19 21 23 25 27 29 31 33 35 37 39 41 43 45 47 49))

(define intra-object-1-out%
  (class object%
    (field [a 1] [b 2] [c 3])
    (define-label-fields (1 3 5 7 9 11 13 15 17 19 21 23 25 27 29 31 33 35 37 39 41 43 45 47 49 51 53 55 57 59 61 63 65 67 69 71 73 75 77 79 81 83 85 87 89 91 93 95 97 99))
    (define-input-neural-fields (1 3 5 7 9 11 13 15 17 19 21 23 25 27 29 31 33 35 37 39 41 43 45 47 49 51 53 55 57 59 61 63 65 67 69 71 73 75 77 79 81 83 85 87 89 91 93 95 97 99))
    (super-new)))

(define intra-object-1-in%
  (class object%
    (field [in 1])
    (define-output-label-fields (3 5 7 9 11 13 15 17 19 21 23 25 27 29 31 33 35 37 39 41 43 45 47 49))
    (define-output-neural-fields (3 5 7 9 11 13 15 17 19 21 23 25 27 29 31 33 35 37 39 41 43 45 47 49))
    (super-new)))

(define inter-object-class%
  (class object%
    (field [a 1])
    (define-label-fields (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49))
    (define-external-fields 49)
    (super-new)))

(define intra-object-1-out (new intra-object-1-out%))
(define intra-object-1-in (new intra-object-1-in%))
