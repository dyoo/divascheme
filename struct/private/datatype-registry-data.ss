(module datatype-registry-data mzscheme

  (define-struct datatype (name variants))
  (define-struct variant (name fields))

  ;; unparse-datatype : datatype -> sexp
  ;; generates an s-expression of syntax objects suitable for with-syntax
  (define (unparse-datatype dt)
    (unless (datatype? dt)
      (error 'unparse-datatype "expected datatype"))
    (cons (datatype-name dt)
          (map (lambda (v)
                 (list (variant-name v)
                       (variant-fields v)))
               (datatype-variants dt))))

  ;; parse-datatype : syntax-object -> syntax-object
  ;; generates code that will evaluate to a `datatype' instance
  (define (parse-datatype stx)
    (syntax-case stx ()
      [(type [variant (field ...)] ...)
       #'(let ([certify (syntax-local-certifier)])
           (make-datatype (certify #'type)
                          (list (make-variant (certify #'variant)
                                              (list (certify #'field) ...))
                                ...)))]))

  (provide (all-defined)))
