(module test-in-something mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2 8))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2 8))
           "../in-something.ss")
  
  (define in-something-tests
    (test-suite
     "in-something.ss"
     (test-case
      "empty case"
      (check-false (in-something? "")))
     (test-case
      "strings"
      (check-equal? (in-something? "\"hello") "\""))
     (test-case
      "strings with a hanging escape"
      (check-equal? (in-something? "\"hello\\") "\\\""))))
  
  (test/text-ui in-something-tests))