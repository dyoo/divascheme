;; =============================================================================
;;
;; datatype.ss - by Dave Herman
;; version 5, 2005-7-14
;;
;; An implementation of algebraic datatypes that work with the contract system,
;; somewhat similar to the define-datatype construct from EoPL.
;;
;; =============================================================================

(module datatype mzscheme
  (require (lib "contract.ss"))
  (require (lib "struct.ss"))
  (require-for-syntax "private/struct-info-compat.ss")
  (require-for-syntax "private/datatype-registry.ss")
  (require-for-syntax "private/datatype-registry-data.ss")

  (define-syntax (define-datatype stx)
    (syntax-case stx ()
      [(_ (type (prop ...)) [variant (field ...) extra ...] ...)
       (with-syntax ([build-static-info (parse-datatype #'(type [variant (field ...)] ...))])
         #'(begin
             ;; Register the datatype every time the defining module is visited.
             (begin-for-syntax (register-datatype! #'type build-static-info))
             (define-struct/properties type () (prop ...))
             (define-variant type variant (field ...) extra ...)
             ...))]
      [(_ type [variant (field ...) extra ...] ...)
       (with-syntax ([build-static-info (parse-datatype #'(type [variant (field ...)] ...))])
         #'(begin
             ;; Register the datatype every time the defining module is visited.
             (begin-for-syntax (register-datatype! #'type build-static-info))
             (define-struct type ())
             (define-variant type variant (field ...) extra ...)
             ...))]))

  (define-syntax define-variant
    (syntax-rules ()
      [(_ type variant (field ...))
       (define-variant type variant (field ...) ())]
      [(_ type variant (field ...) ([prop prop-val] ...))
       (define-struct/properties (variant type) (field ...) ([prop prop-val] ...))]))

  (define-syntax (provide-datatype stx)
    (syntax-case stx ()
      [(_ type)
       (let ([static-info (lookup-datatype #'type)])
         (with-syntax ([(type [variant (arg ...)] ...) (unparse-datatype static-info)]
                       [predicate (get-struct-predicate #'type)])
           #'(begin
               (provide predicate)
               (provide (struct variant (arg ...)))
               ...)))]))

  (define-syntax (provide-datatype/contract stx)
    (syntax-case stx ()
      [(_ type [variant (contract ...)] ...)
       (let ([static-info (lookup-datatype #'type)])
         (with-syntax ([(type [variant (arg ...)] ...) (unparse-datatype static-info)]
                       [predicate (get-struct-predicate #'type)])
           #'(begin
               (provide predicate)
               (provide/contract (struct (variant type) ([arg contract] ...)))
               ...)))]))

  (provide define-datatype provide-datatype provide-datatype/contract))