(module datatype-registry mzscheme
  (require (lib "boundmap.ss" "syntax"))

  ;; The global table of defined datatypes.
  (define datatypes (make-module-identifier-mapping))

  (define (register-datatype! name dt)
    (module-identifier-mapping-put! datatypes name dt))

  (define (lookup-datatype id)
    (module-identifier-mapping-get datatypes id))

  (provide register-datatype! lookup-datatype))
