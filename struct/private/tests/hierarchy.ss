(module hierarchy mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  (require (lib "string.ss" "srfi" "13"))
  (require (lib "list.ss"))
  (require "../../hierarchy.ss")

  (define truck-bath-flap
    '(root (thing1 thing2)
           (boy-child (hair age))
           (girl-child (nose ducks))
           (truck (wheels)
                  (bath (water ring-around-rosy?))
                  (flap (wtf?)))))

  (define truck-bath-flap/contracts
    '(root (integer? procedure?)
           (boy-child (string? number?))
           (girl-child (string? number?))
           (truck (number?)
                  (bath (string? boolean?))
                  (flap (boolean?)))))

  (define truck-bath-flap/both
    '(root ([thing1 integer?] [thing2 procedure?])
           (boy-child ([hair string?] [age number?]))
           (girl-child ([nose string?] [ducks number?]))
           (truck ([wheels number?])
                  (bath ([water string?] [ring-around-rosy? boolean?]))
                  (flap ([wtf? boolean?])))))

  (require (lib "pretty.ss"))
  (define-syntax quasilet*-modules
    (syntax-rules ()
      [(quasilet*-modules ([m1 lang1 body1 ...]
                           [m2 lang2 body2 ...]
                           ...)
         body ...)
       (let ([ns (make-namespace)])
         (parameterize ([current-namespace ns])
           (eval `(module m1 lang1 body1 ...))
           (eval `(module m2 lang2 body2 ...))
           ...
           (eval `body)
           ...))]))

  (define-syntax context
    (syntax-rules ()
      [(_ def prov use-expression)
       (quasilet*-modules ([definitions mzscheme
                             (require "../../hierarchy.ss")
                             ,def ,prov]
                           [client mzscheme
                             (require definitions)
                             (define result use-expression)
                             (provide result)])
         (require client)
         result)]))

  (define-syntax context:define-hierarchy+provide-hierarchy
    (syntax-rules ()
      [(_ (hierarchy-sexp) use-expression)
       (context `(define-hierarchy ,hierarchy-sexp)
                '(provide-hierarchy root)
                use-expression)]))

  (define-syntax context:define-hierarchy+provide-hierarchy/contract
    (syntax-rules ()
      [(_ (define-sexp provide-sexp) use-expression)
       (context `(define-hierarchy ,define-sexp)
                `(provide-hierarchy/contract ,provide-sexp)
                use-expression)]))

  (define-syntax context:define-hierarchy+provide-hierarchy/contract+disabled
    (syntax-rules ()
      [(_ (define-sexp provide-sexp) use-expression)
       (context `(define-hierarchy ,define-sexp)
                `(without-hierarchy-contracts (provide-hierarchy/contract ,provide-sexp))
                use-expression)]))

  (define-syntax context:define-hierarchy/provide/contract
    (syntax-rules ()
      [(_ (hierarchy-sexp) use-expression)
       (context `(define-hierarchy/provide/contract ,hierarchy-sexp) '() use-expression)]))

  (define-syntax context:define-hierarchy/provide/contract+disabled
    (syntax-rules ()
      [(_ (hierarchy-sexp) use-expression)
       (context `(without-hierarchy-contracts (define-hierarchy/provide/contract ,hierarchy-sexp))
                '()
                use-expression)]))

  (define-syntax assert-arities
    (syntax-rules ()
      [(_ context (data ...) (name ...) (arity ...))
       (with-handlers ([(lambda (exn) #t)
                        (lambda (exn)
                          (print-struct #t)
                          (fprintf (current-error-port) "~v~n" exn)
                          (raise exn))])
       (check-true
        (andmap (lambda (thunk k)
                  (procedure-arity-includes? (thunk) k))
                (list (lambda () (context (data ...) name)) ...)
                (list arity ...))))]))

  ;; since exn:fail:contract is not in the initial namespace, we can't quite
  ;; look at all the information in a contract exception we get from within a
  ;; test case, but we can look at its exn-message field since exn is the same

  ;; TODO: just attach the (lib "contract.ss") instance to the namespace?
  (define (looks-like-contract-exn? exn)
    (and (exn? exn)
         (string-prefix? "client broke the contract" (exn-message exn))))

  (define test-contracts
    (test-suite
     "test contracts"
     (test-case "disable: define-hierarchy with provide-hierarchy/contract"
                     (check-not-exn (lambda ()
                                       (context:define-hierarchy+provide-hierarchy/contract+disabled
                                        (truck-bath-flap truck-bath-flap/contracts)
                                        (make-boy-child 1 1 1 1)))))
     (test-case "disable: define-hierarchy/provide/contract"
                     (check-not-exn (lambda ()
                                       (context:define-hierarchy/provide/contract+disabled
                                        (truck-bath-flap/both)
                                        (make-boy-child 1 1 1 1)))))
     (test-case "enable: define-hierarchy with provide-hierarchy/contract"
                     (check-exn looks-like-contract-exn?
                                 (lambda ()
                                   (context:define-hierarchy+provide-hierarchy/contract
                                    (truck-bath-flap truck-bath-flap/contracts)
                                    (make-boy-child 1 1 1 1)))))
     (test-case "enable: define-hierarchy/provide/contract"
                     (check-exn looks-like-contract-exn?
                                 (lambda ()
                                   (context:define-hierarchy/provide/contract
                                    (truck-bath-flap/both)
                                    (make-boy-child 1 1 1 1)))))
     ))

  (define test-arities
    (test-suite
     "basic arity checks"
     (test-case "provide-hierarchy"
                     (assert-arities context:define-hierarchy+provide-hierarchy (truck-bath-flap)
                                     (make-root make-boy-child make-girl-child make-truck make-bath make-flap)
                                     (2 4 4 3 5 4)))
     (test-case "define-hierarchy with provide-hierarchy/contract"
                     (assert-arities context:define-hierarchy+provide-hierarchy/contract+disabled
                                     (truck-bath-flap truck-bath-flap/contracts)
                                     (make-root make-boy-child make-girl-child make-truck make-bath make-flap)
                                     (2 4 4 3 5 4)))
     (test-case "define-hierarchy/provide/contract"
                     (assert-arities context:define-hierarchy/provide/contract+disabled
                                     (truck-bath-flap/both)
                                     (make-root make-boy-child make-girl-child make-truck make-bath make-flap)
                                     (2 4 4 3 5 4)))
     ))

  (define test-hierarchy
    (test-suite
     "all hierarchy.ss tests"
     test-arities
     test-contracts
     ))

  (provide test-hierarchy))
