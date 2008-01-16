(module class mzscheme
  (require (lib "class.ss"))
  (require (lib "match.ss"))

  (define-syntax (init-private/h stx)
    (syntax-case stx ()
      [(_ decl)
       #'(begin)]
      [(_ decl (name default-value) clause ...)
       (with-syntax ([internal-name (syntax-local-introduce (datum->syntax-object #f (gensym)))])
         #'(begin
             (decl ((internal-name name) default-value))
             (define name internal-name)
             (init-private/h decl clause ...)))]
      [(_ decl name clause ...)
       (with-syntax ([internal-name (syntax-local-introduce (datum->syntax-object #f (gensym)))])
         #'(begin
             (decl ((internal-name name)))
             (define name internal-name)
             (init-private/h decl clause ...)))]))

  (define-syntax init-private
    (syntax-rules ()
      [(_ args ...)
       (init-private/h init args ...)]))

  (define-syntax init-private-field
    (syntax-rules ()
      [(_ args ...)
       (init-private/h init-field args ...)]))

  (define-syntax method
    (syntax-rules ()
      [(_ object-exp method-name)
       (lambda args (send/apply object-exp method-name args))]))

  (define-syntax (define-method stx)
    (syntax-case stx (private)
      ;; unfortunately, send/apply is incompatible with private methods
      [(_ private (name . args) body0 body1 ...)
       (raise-syntax-error 'define-method "cannot be used for private methods" stx)]
      [(_ access (name . args) body0 body1 ...)
       ;; create an internal name whose string representation is identical to the external name
       (with-syntax ([internal-name (datum->syntax-object #'name
                                                          (string->uninterned-symbol
                                                           (symbol->string
                                                            (syntax-object->datum #'name))))])
         #'(begin
             (define (internal-name . args)
               body0 body1 ...)
             (access [internal-name name])
             (define-syntax name
               (syntax-id-rules ()
                 [(name macro-args (... ...))
                  (internal-name macro-args (... ...))]
                 [name
                  (method this name)]))))]))

  (define-match-expander %
    (lambda (stx)
      (syntax-case stx ()
        [(% class-or-interface access ...)
         (with-syntax ([(app-exp ...)
                        (map (lambda (acc)
                               (syntax-case acc ()
                                 [(x (method args ...))
                                  #'(app (lambda (y) (send y method args ...)) x)]
                                 [(x id)
                                  (identifier? #'id)
                                  #'(app (lambda (y) (get-field id y)) x)]
                                 [id
                                  (identifier? #'id)
                                  #'(app (lambda (y) (get-field id y)) id)]))
                             (syntax->list #'(access ...)))])
           #'(and (? (lambda (v) (is-a? v class-or-interface)))
                  app-exp ...))]))
    (lambda (stx)
      (syntax-case stx ()
        [(% class-or-interface access ...)
         (with-syntax ([(app-exp ...)
                        (map (lambda (acc)
                               (syntax-case acc ()
                                 [(x (method args ...))
                                  #'(= (lambda (y) (send y method args ...)) x)]
                                 [(x id)
                                  (identifier? #'id)
                                  #'(= (lambda (y) (get-field id y)) x)]
                                 [id
                                  (identifier? #'id)
                                  #'(= (lambda (y) (get-field id y)) id)]))
                             (syntax->list #'(access ...)))])
           #'(and (? (lambda (v) (is-a? v class-or-interface)))
                  app-exp ...))]))
    (lambda (stx)
      (raise-syntax-error '% "used outside of match or plt-match context" stx)))

  (provide init-private init-private-field method %))
