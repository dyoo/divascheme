(module struct mzscheme
  (require-for-syntax (lib "struct.ss" "syntax"))
  (require (lib "etc.ss"))

  ;; TODO: provide-struct/immutable
  ;; TODO: provide-struct/contract that's compatible with define-struct/opt

  (define-for-syntax (extract-field-ids stx)
    (let loop ([result null] [fields stx])
      (syntax-case fields ()
        [() (reverse result)]
        [([field-id default] field ...)
         (loop (cons #'field-id result)
               #'(field ...))]
        [(field-id field ...)
         (loop (cons #'field-id result)
               #'(field ...))])))

  (define-syntax (opt-constructor! stx)
    (syntax-case stx ()
      [(_ name (field ...) (field-id ...))
       (with-syntax ([(struct-type-name ctor-name pred-name proc-name ...)
                      (build-struct-names #'name (syntax-e #'(field-id ...)) #f #f stx)])
         #'(let ([old-ctor ctor-name])
             (set! ctor-name (opt-lambda (field ...)
                               (old-ctor field-id ...)))))]))

  (define-syntax (define-struct/opt stx)
    (syntax-case stx ()
      [(_ (name parent) (field ...) inspector-exp)
       (with-syntax ([(field-id ...) (extract-field-ids #'(field ...))])
         #'(begin
             (define-struct (name parent) (field-id ...) inspector-exp)
             (opt-constructor! name (field ...) (field-id ...))))]
      [(_ name (field ...) inspector-exp)
       (with-syntax ([(field-id ...) (extract-field-ids #'(field ...))])
         #'(begin
             (define-struct name (field-id ...) inspector-exp)
             (opt-constructor! name (field ...) (field-id ...))))]
      [(_ (name parent) (field ...))
       #'(define-struct/opt (name parent) (field ...) (current-inspector))]
      [(_ name (field ...))
       #'(define-struct/opt name (field ...) (current-inspector))]))

  (provide define-struct/opt))
