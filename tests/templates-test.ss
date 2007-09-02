(module templates-test mzscheme
  (require "test-harness.ss"
           (lib "etc.ss")
           "utilities.ss"
           "templates.ss")

  (define (test-lookup-template symbol number macro? wrap? expected)
    (let-values ([(t n) (lookup-template symbol number macro? wrap?)])
      (test t expected)))


  (define (tests)
    (print-tests true)
    ; lookup-template : (union symbol false) non-negative-integer boolean boolean -> sexp-string
    (test-lookup-template 'let 0 true  false "(let ([$name$ $binding$] ***) \n $body$ +++)")
    (test-lookup-template 'bar 0 true  false "(bar $expr$ ---)")
    (test-lookup-template 'foo 0 true  false "(foo $expr$ ---)")
    (test-lookup-template 'id  0 true  false "(id $expr$)")
    (test-lookup-template 'let 0 false false "(let ([$name$ $binding$] ***) \n $body$ +++)")
    (test-lookup-template 'bar 0 false false "bar")
    (test-lookup-template 'foo 0 false false "foo")
    (test-lookup-template 'id  0 false false "id")

    ; placeholder/stx? : syntax -> boolean
    (test (placeholder/stx? #'||) false)
    (test (placeholder/stx? #'|    |) false)
    (test (placeholder/stx? #'|a b c |) false)
    (test (placeholder/stx? #'|$$|) true)
    (test (placeholder/stx? #'|$|) false)
    (test (placeholder/stx? #'|$ $ $ $ $ |) false)
    (test (placeholder/stx? #'|$ $ $|) true)
     
    ; placeholder/string? : string -> boolean    
    (test (placeholder/string? "") false)
    (test (placeholder/string? "   ") false)
    (test (placeholder/string? "a") false)
    (test (placeholder/string? "foo") false)
    (test (placeholder/string? "bar") false)
    (test (placeholder/string? "$$") true)
    (test (placeholder/string? "$     $") true)
    (test (placeholder/string? " $ $ $") false)
    (test (placeholder/string? "$$$") true)
    (test (placeholder/string? "$") false)
    
    ; holder/ellipsis? : syntax -> boolen
    (test (holder/ellipsis? #'||) false)
    (test (holder/ellipsis? #'|-|) false)
    (test (holder/ellipsis? #'|--|) false)
    (test (holder/ellipsis? #'|---|) true)
    (test (holder/ellipsis? #'|----|) false)
    (test (holder/ellipsis? #'|-----|) false)
    (test (holder/ellipsis? #'| |) false)
    (test (holder/ellipsis? #'|      |) false)
    (test (holder/ellipsis? #'|a|) false)
    (test (holder/ellipsis? #'|$|) false)
    (test (holder/ellipsis? #'|$$|) true)
    (test (holder/ellipsis? #'| $expr$ |) false)
    (test (holder/ellipsis? #'|$expr$|) true)
    
    ; holder/ellipsis-tree? : syntax -> boolean
    (test false (holder/ellipsis-tree? #'()))
    (test false (holder/ellipsis-tree? #'(a)))
    (test false (holder/ellipsis-tree? #'a))
    (test true  (holder/ellipsis-tree? #'---))
    (test false (holder/ellipsis-tree? #'(a ---)))
    (test true  (holder/ellipsis-tree? #'($expr$ ---)))
    (test false (holder/ellipsis-tree? #'((((((()))))))))
    (test true  (holder/ellipsis-tree? #'((((((($placeholder$)))))))))
    (test true  (holder/ellipsis-tree? #'[$name$ $binding$]))
    
    )

  (parameterize ([current-templates (lambda () mzscheme-templates)])
    (tests)))