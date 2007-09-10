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
      "strings close"
      (check-false (in-something? "\"hello\"")))
     
     (test-case
      "piped symbols close"
      (check-equal? (in-something? "|#<<EOF")
                    "|"))
     
     (test-case
      "comments don't get into strings"
      (check-equal? (in-something? "\" ;; hello")
                    "\""))
     
     (test-case
      "here strings don't get into strings"
      (check-equal? (in-something? "\" #<<EOF\n hello")
                    "\""))
     
     (test-case
      "strings with a hanging escape"
      (check-equal? (in-something? "\"hello\\") "\\\""))
     
     (test-case
      "nested comments being closed"
      (check-false (in-something? "#||#")))
     
     (test-case
      "nested comments"
      (check-equal? (in-something? "#|")
                    "|#"))
     
     (test-case
      "nested comments 2"
      (check-equal? (in-something? "#| hello #| world")
                    "|#|#"))
     
     (test-case
      "here strings"
      (check-equal? (in-something? "#<<EOF\nblah")
                    "\nEOF\n"))
     (test-case
      "here strings 2"
      (check-equal? (in-something? "#<<EOF\nblah\nEOF")
                    #f))
     
     (test-case
      "here strings 3"
      (check-equal? (in-something? "#<<helloworld")
                    "\nhelloworld\n"))
     
     (test-case
      "here strings 4"
      (check-equal? (in-something? "#<<hello\nhelloblah")
                    "\nhello\n"))
     
     (test-case
      "mixup"
      (check-equal? (in-something? "#\\#|")
                    "|"))))
  
  (test/text-ui in-something-tests))