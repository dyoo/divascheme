(module test-cleanup-whitespace mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2 8))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2 8))
           "../rope.ss"
           "../cleanup-whitespace.ss")
  
  ;; cw: string -> string
  ;; Show how cleanup-whitespace handles strings.
  (define (cw s)
    (let-values ([(a-rope _)
                  (cleanup-whitespace (string->rope s) 0 '())])
      (rope->string a-rope)))
  
  
  ;; cw/m: string (listof natural) -> (listof natural)
  ;; Just focus on how markers are preserved after cleanup.
  (define (cw/m s m)
    (let-values ([(_ markers)
                  (cleanup-whitespace (string->rope s) 0 m)])
      markers))
  
  
  (define cleanup-whitespace-tests
    (test-suite
     "cleanup-whitespace.ss"
     (test-case
      "empty case"
      (check-equal? (cw "") ""))
     
     (test-case
      "simple case"
      (check-equal? (cw "hello") "hello"))
     
     (test-case
      "another simple case"
      (check-equal? (cw "hello  world") "hello world"))
     
     (test-case
      "spaces around an atom"
      (check-equal? (cw " hello ") "hello"))
     
     (test-case
      "spaces in sexp"
      (check-equal? (cw "(hello world this is a test )")
                    "(hello world this is a test)"))
     
     (test-case
      "newlines"
      (check-equal? (cw "(hello world  \n\n    this is a test )")
                    "(hello world\n\nthis is a test)"))
     
     
     (test-case
      "simple markers 1"
      (check-equal? (cw/m "hello world" (list 0 5))
                    (list 0 5)))
     
     (test-case
      "simple markers 2"
      (check-equal? (cw/m "  hello world" (list 2 7))
                    (list 0 5)))))
  
  (test/text-ui cleanup-whitespace-tests))