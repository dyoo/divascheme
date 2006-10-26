;; =============================================================================
;;
;; datatype.ss - by Dave Herman
;; version 4, 2005-2-18
;;
;; An implementation of algebraic datatypes that work with the contract system,
;; somewhat similar to the define-datatype construct from EoPL.
;;
;; =============================================================================

(module datatype mzscheme
  (require (lib "contract.ss"))
  (require-for-syntax (lib "boundmap.ss" "syntax"))

  ;; The global table of defined datatypes.
  (define-for-syntax datatypes (make-module-identifier-mapping))

  (define-syntax (define-datatype stx)
    (syntax-case stx ()
      [(_ type [variant (field ...)] ...)
       (let ([datatype-info
              (list #'type (syntax->list #'([variant (field ...)] ...)))])
         (module-identifier-mapping-put! datatypes #'type datatype-info)
         #'(begin
             (define-struct type ())
             (define-struct (variant type) (field ...))
             ...))]))

  (define-syntax (provide-datatype stx)
    (syntax-case stx ()
      [(_ type)
       (let ([static-info (module-identifier-mapping-get datatypes #'type)]
             [predicate-id (list-ref (syntax-local-value #'type) 2)])
         (with-syntax ([([variant (arg ...)] ...) (cadr static-info)])
           #`(begin
               (provide #,predicate-id)
               (provide (struct variant (arg ...)))
               ...)))]))

  (define-syntax (provide-datatype/contract stx)
    (syntax-case stx ()
      [(_ type [variant (contract ...)] ...)
       (let ([static-info (module-identifier-mapping-get datatypes #'type)]
             [predicate-id (list-ref (syntax-local-value #'type) 2)])
         (with-syntax ([([variant (arg ...)] ...) (cadr static-info)])
           #`(begin
               (provide #,predicate-id)
               (provide/contract (struct (variant type) ([arg contract] ...)))
               ...)))]))

  (provide define-datatype provide-datatype provide-datatype/contract))