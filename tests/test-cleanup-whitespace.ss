(module test-cleanup-whitespace mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2 8))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2 8))
           "../rope.ss"
           "../cleanup-whitespace.ss")
  
  (define (cw s)
    (rope->string (cleanup-whitespace (string->rope s))))
  
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
      (check-equal? (cw " hello ") "hello"))))
  
  (test/text-ui cleanup-whitespace-tests))