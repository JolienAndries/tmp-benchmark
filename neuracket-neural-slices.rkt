#lang racket
(provide (all-defined-out))
(require "neuracket-ML-models.rkt"
         (for-syntax racket/base racket/syntax))

(define-syntax (define-input-slice stx)
  (syntax-case stx ()
    [(_ name count)
     (let* ([n (syntax-e #'count)]
            [parameters (append (for/list ([index (in-range 1 (add1 n))])
                                  (format-id stx "obj-in~a" index))
                                (list (format-id stx "obj-out")))]
            [inputs (for/list ([index (in-range 1 (add1 n))])
                      #`[#,(format-id stx "obj-in~a" index) #,(format-id stx "a")])]
            [output-object (format-id stx "obj-out")]
            [model (format-id stx "model-~a-in-1-out" n)])
      #`(defneuralslice (name #,@parameters)
           [input-fields #,@inputs]
             [label-fields [#,output-object lbl1]]
             [target-fields [#,output-object external-neural]]
           [MLObject #,model]))]))

(define-syntax (define-output-slice stx)
  (syntax-case stx ()
    [(_ name count)
     (let* ([n (syntax-e #'count)]
            [parameters (append (list (format-id stx "obj-in"))
                                (for/list ([index (in-range 1 (add1 n))])
                                  (format-id stx "obj-out~a" index)))]
            [input-object (format-id stx "obj-in")]
            [labels (for/list ([index (in-range 1 (add1 n))])
                       #`[#,(format-id stx "obj-out~a" index)
                     #,(format-id stx "lbl~a" index)])]
            [targets (for/list ([index (in-range 1 (add1 n))])
                       (define field (if (= index 1)
                                         (format-id stx "external-neural")
                                         (format-id stx "external-neural~a" index)))
                       #`[#,(format-id stx "obj-out~a" index) #,field])]
            [model (format-id stx "model-1-in-~a-out" n)])
      #`(defneuralslice (name #,@parameters)
             [input-fields [#,input-object a]]
           [label-fields #,@labels]
           [target-fields #,@targets]
           [MLObject #,model]))]))

(define-syntax (define-all-slices stx)
  (with-syntax ([(input-slice ...)
                 (for/list ([count (in-range 1 100 2)])
                   #`(define-input-slice
                       #,(format-id stx "~a-in-1-out-slice" count)
                       #,count))]
                [(output-slice ...)
                 (for/list ([count (in-range 3 50 2)])
                   #`(define-output-slice
                       #,(format-id stx "1-in-~a-out-slice" count)
                       #,count))]
                [(input-name ...)
                 (for/list ([count (in-range 1 100 2)])
                   (format-id stx "~a-in-1-out-slice" count))]
                [(output-name ...)
                 (for/list ([count (in-range 3 50 2)])
                   (format-id stx "1-in-~a-out-slice" count))])
    #'(begin
        input-slice ...
        output-slice ...
        (provide input-name ... output-name ...))))

(define-all-slices)
