(module test-semi-read-syntax mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2 8))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2 8))
           (lib "list.ss")
           "semi-read-syntax.ss")
  
  ;; parse: string -> sexpr
  ;; Quick and dirty function for seeing if we get expected results from semi-read-syntax
  (define (parse text)
    (syntax-object->datum
     (first (semi-read-syntax-list #f (open-input-string text)))))
  
  
  
  (define semi-read/syntax-tests
    (test-suite
     "semi-read-syntax.ss"
     (test-case
      "simple test case"
      (check-equal?
       (parse "hello")
       'hello))
     
     (test-case
      "dots are treated as symbols"
      (check-equal?
       (parse ".")
       '|.|))
     
     (test-case
      "multiple dots"
      (check-equal?
       (parse "(hello . world . testing!)")
       '(hello |.| world |.| testing!)))
     
     (test-case
      "quotes"
      (check-equal?
       (parse "'foo")
       ''foo))
     
     (test-case
      "comments"
      (check-equal?
       (parse "(hello\n;;blah\nworld)")
       '(hello |;;blah| world)))
     
     (test-case
      "comments 2"
      (check-equal?
       (parse ";; test: of something -> (that looks contracty)")
       '|;; test: of something -> (that looks contracty)|))
     
     
     (test-case
      "hash comments exist"
      (check-equal?
       (parse "#;foo")
       '(foo)))
     
     (test-case
      "hash comments on nested structures"
      (check-equal? (parse "#;(foobar)")
                    '((foobar))))))
  
  
  (test/text-ui semi-read/syntax-tests))