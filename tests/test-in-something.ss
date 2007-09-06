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
      (check-equal? (in-something? "\"hello\\") "\\\""))
     
     (test-case
      "here strings"
      (check-equal? (in-something? "#<<EOF\nblah")
                    "\nEOF"))
     (test-case
      "here strings 2"
      (check-equal? (in-something? "#<<EOF\nblah\nEOF")
                    #f))
     
     (test-case
      "here strings 3"
      (check-equal? (in-something? "#<<helloworld")
                    "\nhelloworld"))
     
     (test-case
      "here strings 4"
      (check-equal? (in-something? "#<<hello\nhelloblah")
                    "\nhello"))))
  
  (test/text-ui in-something-tests))