#lang racket/base
(require racket/class pyffi "racket-ML-models.rkt"
         (for-syntax racket/base racket/syntax))
(provide intra-object-1-out intra-object-1-in inter-object-class%)

(define-for-syntax (class-id stx template . arguments)
  (syntax-local-introduce (apply format-id stx template arguments)))

;; (field (external-neural #f) (external-neuralx #f) ...) x = 2 - 49 
(define-syntax (define-external-fields stx)
  (with-syntax ([field-form
                 #`(field
                    #,@(for/list ([index (in-range 1 50)])
                         (define field (if (= index 1)
                                           (format-id stx "external-neural")
                                           (format-id stx "external-neural~a" index)))
                         #`[#,field #f]))])
    #'field-form))

;; (begin (field [neuralx #f] ...)  (define/public (train-neuralx) (send/apply racket-ml-models train-x-in-1-out ((get-field a/b/c this) ..x times.. (get-field neural this))) ...)
(define-syntax (define-input-structure stx)
  (with-syntax ([field-form
                 #`(field
                    #,@(for/list ([count (in-range 1 100 2)])
                         #`[#,(class-id stx "neural~a" count) #f]))]
                [(method ...)
                 (for/list ([count (in-range 1 100 2)])
                   (define field (class-id stx "neural~a" count))
                   (define inputs
                     (for/list ([index (in-range count)])
                       (define input-field
                         (list-ref (list (class-id stx "a")
                                         (class-id stx "b")
                                         (class-id stx "c"))
                                   (modulo index 3)))
                       #`(get-field #,input-field this)))
                   (define method-name (format-id stx "train-neural~a" count))
                   (define train-function (format-id stx "train-~a-in-1-out" count))
                   #`(define/public (#,method-name)
                         (send/apply racket-ml-models #,train-function (list #,@inputs (get-field #,field this)))))])
    #'(begin
        field-form
        method ...)))

;; same as above but now with ranging outputs instead of inputs
(define-syntax (define-output-structure stx)
  (with-syntax ([field-form
                 #`(field
                    #,@(apply append
                              (for/list ([count (in-range 3 50 2)])
                                (for/list ([index (in-range 1 (add1 count))])
                                  #`[#,(class-id stx "neural~a.~a" count index) #f]))))]
                [(method ...)
                 (for/list ([count (in-range 3 50 2)])
                   (define fields
                     (for/list ([index (in-range 1 (add1 count))])
                       (class-id stx "neural~a.~a" count index)))
                   (define field-values
                     (for/list ([field fields])
                       #`(get-field #,field this)))
                   (define method-name (format-id stx "train-neurals~a" count))
                   (define train-function (format-id stx "train-1-in-~a-out" count))
                   #`(define/public (#,method-name)
                         (send/apply racket-ml-models #,train-function (list (get-field in this)  #,@field-values))))])
    #'(begin
        field-form
        method ...)))


;; (begin (begin (define/public (infer-neural-x-in-1-out obj ..x objs..)
;;     (set-field! external-neural this (Send/apply racket-ml-models infer-x-in-1-out (list (get-field a obj) ..x times..))
;;     (define/public (train-neural-x-in-1-out obj ...)(Send/apply racket-ml-models train-x-in-1-out (list (get-field a in-obj) ..x times.. (get-field external-neural this))
(define-syntax (define-inter-input-methods stx)
  (with-syntax ([(method ...)
                 (for/list ([count (in-range 1 100 2)])
                   (define objects
                     (for/list ([index (in-range 1 (add1 count))])
                       (format-id stx "in-obj-~a" index)))
                   (define values
                     (for/list ([object objects])
                       #`(get-field a #,object)))
                   (define infer-name (format-id stx "infer-neural-~a-in-1-out" count))
                   (define train-name (format-id stx "train-neural-~a-in-1-out" count))
                   (define infer-function (format-id stx "infer-~a-in-1-out" count))
                   (define train-function (format-id stx "train-~a-in-1-out" count))
                   #`(begin
                       (define/public (#,infer-name #,@objects)
                         (set-field! external-neural this
                                      (send/apply racket-ml-models #,infer-function
                                                  (list #,@values))))
                       (define/public (#,train-name #,@objects)
                         (send/apply racket-ml-models #,train-function (list #,@values (get-field external-neural this))))))])
    #'(begin method ...)))

(define-syntax (define-inter-output-methods stx)
  (with-syntax ([(method ...)
                 (for/list ([count (in-range 3 50 2)])
                   (define objects
                     (for/list ([index (in-range 2 (add1 count))])
                       (format-id stx "out-obj-~a" index)))
                   (define values
                     (cons #'(get-field external-neural this)
                           (for/list ([index (in-range 2 (add1 count))])
                             (define field (format-id stx "external-neural~a" index))
                             #`(get-field #,field #,(list-ref objects (- index 2))))))
                   (define infer-name (format-id stx "infer-neural-1-in-~a-out!" count))
                   (define train-name (format-id stx "train-neural-1-in-~a-out" count))
                   (define infer-function (format-id stx "infer-1-in-~a-out" count))
                   (define train-function (format-id stx "train-1-in-~a-out" count))
                   (define first-result-field (format-id stx "external-neural"))
                   (define rest-result-fields (for/list ([index (in-range 2 (add1 count))])
                                           (format-id stx "external-neural~a" index)))
                   #`(begin
                       (define/public (#,infer-name input-object #,@objects)
                         (define result (send racket-ml-models #,infer-function
                                              (get-field a input-object)))
                         (set-field! #,first-result-field this (list-ref result 0))
                         #,@(for/list ([field rest-result-fields] [index (in-naturals 1)])
                              #`(set-field! #,field #,(list-ref objects (- index 1)) (list-ref result #,index))))
                       (define/public (#,train-name input-object #,@objects)
                         (send/apply racket-ml-models #,train-function (list (get-field a input-object) #,@values)))))])
    #'(begin method ...)))

(define intra-object-1-out%
  (class object%
    (field [a 1] [b 2] [c 3])
    (define-input-structure)
    (super-new)))

(define intra-object-1-in%
  (class object%
    (field [in 1])
    (define-output-structure)
    (super-new)))

(define inter-object-class%
  (class object%
    (field [a 1])
    (define-external-fields)
    (define-inter-input-methods)
    (define-inter-output-methods)
    (super-new)))

(define intra-object-1-out (new intra-object-1-out%))
(define intra-object-1-in (new intra-object-1-in%))
