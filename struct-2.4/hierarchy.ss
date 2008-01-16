(module hierarchy mzscheme
  (require-for-syntax (lib "boundmap.ss" "syntax"))
  (require (lib "contract.ss"))

  ;; The global table of defined hierarchies.
  (define-for-syntax hierarchies (make-module-identifier-mapping))

  ;; Useful for making things run faster during debugging:
  (define-for-syntax disable-hierarchy-contracts? (make-parameter #f))

  (define-syntax (without-hierarchy-contracts stx)
    (syntax-case stx ()
      [(_ e)
       (parameterize ([disable-hierarchy-contracts? #t])
         (local-expand #'e
                       (syntax-local-context)
                       (list #'define-values
                             #'define-syntaxes
                             #'define-struct
                             #'provide
                             #'require)))]))

  (define-syntax (define-hierarchy/1 stx)
    (syntax-case stx ()
      [(_ (name parent) (field ...) child-name ...)
       (with-syntax ([(parent-field ...) (list-ref (module-identifier-mapping-get hierarchies #'parent) 2)])
         (module-identifier-mapping-put! hierarchies #'name
           (list #'parent
                 #'(field ...)
                 #'(parent-field ... field ...)
                 #'(child-name ...)))
         #'(define-struct (name parent) (field ...)))]
      [(_ name (field ...) child-name ...)
       (begin
         (module-identifier-mapping-put! hierarchies #'name
           (list #f
                 #'(field ...)
                 #'(field ...)
                 #'(child-name ...)))
         #'(define-struct name (field ...)))]))

  (define-syntax (provide-hierarchy/contract/1 stx)
    (syntax-case stx ()
      [(_ name (contract ...))
       (let ([static-info (module-identifier-mapping-get hierarchies #'name)])
         (with-syntax ([(local-field ...) (list-ref static-info 1)]
                       [(every-field ...) (list-ref static-info 2)])
           (cond
             [(disable-hierarchy-contracts?)
              #'(provide (struct name (local-field ...)))]
             [(car static-info)
              #`(provide/contract (struct (name #,(car static-info)) ([every-field contract] ...)))]
             [else
              #'(provide/contract (struct name ([every-field contract] ...)))])))]))

  (define-syntax define-hierarchy
    (syntax-rules ()
      [(_ (name (field ...) (child-name etc ...) ...))
       (begin
         (define-hierarchy/1 name (field ...) child-name ...)
         (define-hierarchy/child name child-name etc ...)
         ...)]))

  (define-syntax define-hierarchy/child
    (syntax-rules ()
      [(_ parent name (field ...) (child-name etc ...) ...)
       (begin
         (define-hierarchy/1 (name parent) (field ...) child-name ...)
         (define-hierarchy/child name child-name etc ...)
         ...)]))

  (define-syntax provide-hierarchy/contract
    (syntax-rules ()
      [(_ (name (contract ...)
            (child (child-contract ...) etc ...)
            ...))
       (begin
         (provide-hierarchy/contract/1 name (contract ...))
         (provide-hierarchy/contract (child (contract ... child-contract ...) etc ...))
         ...)]))

  (define-syntax (provide-hierarchy stx)
    (syntax-case stx ()
      [(_ name)
       (let ([static-info (module-identifier-mapping-get hierarchies #'name)])
         (with-syntax ([(field ...) (list-ref static-info 1)]
                       [(child ...) (list-ref static-info 3)])
         #'(begin
             (provide (struct name (field ...)))
             (provide-hierarchy child)
             ...)))]))

  (define-syntax define-hierarchy/provide/contract
    (syntax-rules ()
      [(_ (name ([field contract] ...)
            (child ([child-field child-contract] ...) etc ...)
            ...))
       (begin
         (both->define (name ([field contract] ...)
                         (child ([child-field child-contract] ...) etc ...)
                         ...))
         (both->provide (name (contract ...)
                          (child ([child-field child-contract] ...) etc ...)
                          ...)))]))

  (define-syntax both->define
    (syntax-rules ()
      [(_ (name ([field contract] ...)
            (child ([child-field child-contract] ...) etc ...)
            ...))
       (begin
         (define-hierarchy/1 name (field ...) child ...)
         (both->define/child ((child name) ([child-field child-contract] ...) etc ...))
         ...)]))

  (define-syntax both->define/child
    (syntax-rules ()
      [(_ ((name parent) ([field contract] ...)
             (child ([child-field child-contract] ...) etc ...)
             ...))
       (begin
         (define-hierarchy/1 (name parent) (field ...) child ...)
         (both->define/child ((child name) ([child-field child-contract] ...) etc ...))
         ...)]))

  (define-syntax both->provide
    (syntax-rules ()
      [(_ (name (contract ...)
            (child ([child-field child-contract] ...) etc ...)
            ...))
       (begin
         (provide-hierarchy/contract/1 name (contract ...))
         (both->provide (child (contract ... child-contract ...) etc ...))
         ...)]))

  (provide define-hierarchy provide-hierarchy
           provide-hierarchy/contract define-hierarchy/provide/contract
           without-hierarchy-contracts))
