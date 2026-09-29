#lang racket/base
(require racket/class
         pyffi
         racket/list
         (for-syntax racket/base racket/syntax))
(initialize)
(post-initialize)
(define no-python? (getenv "NO_PYTHON"))
(provide no-python?)
(unless no-python?
  (run* "with open('./train-and-infer-functions.py') as file: exec(file.read())"))

(define (no-python-infer output-count)
  (lambda args
    (if (= output-count 1)
        42
        (make-list output-count 42))))

(define (no-python-train . args) 'ok)

;; (begin (begin (define inferinout (run pythoninfer)) (define traininout (run pythontrain))) ...) // (begin (begin (define inferinout (nopythoninfer 1)) (define traininout nopythoninfer)) ...)
(define-syntax (define-input-functions stx)
  (with-syntax ([(definition ...)
                 (for/list ([count (in-range 1 100 2)])
                   (define infer-name (format-id stx "infer-~a-in-1-out-function" count))
                   (define train-name (format-id stx "train-~a-in-1-out-function" count))
                   (define infer-python (format "infer_~a_in_1_out" count))
                   (define train-python (format "train_~a_in_1_out" count))
                   (if (getenv "NO_PYTHON")
                       #`(begin
                           (display "-no python-")
                           (define #,infer-name (no-python-infer 1))
                           (define #,train-name no-python-train))
                       #`(begin
                           (display "*yes python*")
                           (define #,infer-name (run #,infer-python))
                           (define #,train-name (run #,train-python)))))])
    #'(begin definition ...)))

;; same as above but with ranging outputs ipv ranging inputs
(define-syntax (define-output-functions stx)
  (with-syntax ([(definition ...)
                 (for/list ([count (in-range 3 50 2)])
                   (define infer-name (format-id stx "infer-1-in-~a-out-function" count))
                   (define train-name (format-id stx "train-1-in-~a-out-function" count))
                   (define infer-python (format "infer_1_in_~a_out" count))
                   (define train-python (format "train_1_in_~a_out" count))
                   (if (getenv "NO_PYTHON")
                       #`(begin
                           (define #,infer-name (no-python-infer #,count))
                           (define #,train-name no-python-train))
                       #`(begin
                           (define #,infer-name (run #,infer-python))
                           (define #,train-name (run #,train-python)))))])
    #'(begin definition ...)))

(define-input-functions)
(define-output-functions)

;; methods for x input 1 output ; calls functions above
(define-syntax (define-input-methods stx)
  (with-syntax ([(method ...)
                 (for/list ([count (in-range 1 100 2)])
                   (define infer-name (format-id stx "infer-~a-in-1-out" count))
                   (define train-name (format-id stx "train-~a-in-1-out" count))
                   (define infer-function (format-id stx "infer-~a-in-1-out-function" count))
                   (define train-function (format-id stx "train-~a-in-1-out-function" count))
                   #`(begin
                       (define/public (#,infer-name . args)
                         (apply #,infer-function args))
                       (define/public (#,train-name . args)
                         (apply #,train-function args))))])
    #'(begin method ...)))

;; methods for 1 input x output calls functions above
(define-syntax (define-output-methods stx)
  (with-syntax ([(method ...)
                 (for/list ([count (in-range 3 50 2)])
                   (define infer-name (format-id stx "infer-1-in-~a-out" count))
                   (define train-name (format-id stx "train-1-in-~a-out" count))
                   (define infer-function (format-id stx "infer-1-in-~a-out-function" count))
                   (define train-function (format-id stx "train-1-in-~a-out-function" count))
                   #`(begin
                       (define/public (#,infer-name . args)
                         (let ((res (apply #,infer-function args)))
                           (if (pylist? res) (pylist->list res) res)))
                       (define/public (#,train-name . args)
                         (apply #,train-function args))))])
    #'(begin method ...)))

(define racket-ml-model%
  (class object%
    (define-input-methods)
    (define-output-methods)
    (super-new)))

(define racket-ml-models (new racket-ml-model%))

(provide no-python? racket-ml-models)
