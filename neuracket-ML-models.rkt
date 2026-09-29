#lang racket
(require racket/class
         mlobject
         (for-syntax racket/base racket/syntax))
(provide (all-defined-out))

(define path-to-python-file "./train-and-infer-functions.py")

(define no-python-model%
  (class object%
    (init-field output-count)
    (super-new)
    (define/public (train . args) 'ok)
    (define/public (infer . args)
      (apply values (make-list output-count 42)))))

(define-syntax (define-input-model stx)
  (syntax-case stx ()
    [(_ name input-count)
     (let* ([count (syntax-e #'input-count)]
           [python-name (format-id stx "python-model-~a-in-1-out" count)]
           [output-argument (format-id stx "output-count")]
            [infer (format "infer_~a_in_1_out" count)]
            [train (format "train_~a_in_1_out" count)]
            [inputs (for/list ([index (in-range 1 (add1 count))])
                      (format-id stx "in~a" index))])
      (if (getenv "NO_PYTHON")
          #`(define name (new no-python-model% [#,output-argument 1]))
          #`(begin
              (display "*yes python*")
              (defMLObject #,python-name
                [file path-to-python-file]
                [infer #,infer]
                [train #,train]
                [input #,@inputs]
                [label out1])
              (define name #,python-name))))]))

(define-syntax (define-output-model stx)
  (syntax-case stx ()
    [(_ name output-count)
     (let* ([count (syntax-e #'output-count)]
           [python-name (format-id stx "python-model-1-in-~a-out" count)]
           [output-argument (format-id stx "output-count")]
            [infer (format "infer_1_in_~a_out" count)]
            [train (format "train_1_in_~a_out" count)]
            [outputs (for/list ([index (in-range 1 (add1 count))])
                       (format-id stx "out~a" index))])
      (if (getenv "NO_PYTHON")
          #`(define name (new no-python-model% [#,output-argument #,count]))
          #`(begin
              (defMLObject #,python-name
                [file path-to-python-file]
                [infer #,infer]
                [train #,train]
                [input in1]
                [label #,@outputs])
              (define name #,python-name))))]))

(define-syntax (define-all-models stx)
  (with-syntax ([(input-model ...)
                 (for/list ([count (in-range 1 100 2)])
                   #`(define-input-model
                       #,(format-id stx "model-~a-in-1-out" count)
                       #,count))]
                [(output-model ...)
                 (for/list ([count (in-range 3 50 2)])
                   #`(define-output-model
                       #,(format-id stx "model-1-in-~a-out" count)
                       #,count))]
                [(input-name ...)
                 (for/list ([count (in-range 1 100 2)])
                   (format-id stx "model-~a-in-1-out" count))]
                [(output-name ...)
                 (for/list ([count (in-range 3 50 2)])
                   (format-id stx "model-1-in-~a-out" count))])
    #'(begin
        input-model ...
      output-model ...
      (provide input-name ... output-name ...))))

(define-all-models)
