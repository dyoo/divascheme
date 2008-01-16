(module struct-info-compat mzscheme
  ;; Compatibility library for mzscheme 360 and 3.99.
  ;; The syntax-local-value of a structure type changed
  ;; in 3.99, so this code is meant to smooth the transition.
  
  (require (planet "version-case.ss" ("dyoo" "version-case.plt" 1 4)))
  
  
  (provide get-struct-predicate)
  
  (version-case
   [(version< (version) "3.99")
    (define (get-struct-predicate type-stx)
      (list-ref (syntax-local-value type-stx) 2))]
   
   [else
    (require scheme/struct-info)
    (define (get-struct-predicate type-stx)
      (list-ref (extract-struct-info
                 (syntax-local-value type-stx))
                2))]))