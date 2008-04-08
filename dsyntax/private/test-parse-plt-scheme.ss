(module test-parse-plt-scheme mzscheme
  (require "parse-plt-scheme.ss"
           "dsyntax.ss"
           "text-support.ss"
           (lib "list.ss")
           (lib "etc.ss")
           (lib "class.ss")
           (lib "framework.ss" "framework")
           (lib "mred.ss" "mred")
           (planet "test.ss" ("schematics" "schemeunit.plt" 2 8))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2 8)))
  
  ;; parse: string -> dstx
  ;; convenience function for testing.
  (define (parse s)
    (first (parse-port (open-input-string s))))
  
  
  
  (define (make-graphical-snip children)
    (new scheme:sexp-snip%
         [left-bracket #\(]
         [right-bracket #\)]
         [saved-snips children]))
  
  

  ;; Produces a port with a graphical snip and calls f on it.
  (define (call-with-graphical-snip-port ip-consumer)
    (local ((define text (new scheme:text%))
            
            (define snip (make-graphical-snip
                          `(,(make-object string-snip% "(")
                            ,(make-object string-snip% "hello")
                            ,(make-object string-snip% ")")))))
      (send text insert "(")
      (send text insert snip)
      (send text insert ")")
      
      (call-with-text-input-port text ip-consumer)))
  
  
  (define parse-plt-scheme-tests
    (test-suite
     "parse-plt-scheme.ss"
     (test-case
      "simple test of atom"
      (check-equal?
       (parse "hello")
       (make-atom "hello")))
     
     (test-case
      "pound-percent"
      (check-equal?
       (parse "#%datum")
       (make-atom "#%datum")))
     
     (test-case
      "parse vector"
      (check-equal?
       (parse "#(1)")
       (make-fusion "#(" (list (make-atom "1")) ")")))
     
     (test-case
      "true and false"
      (check-equal? (parse "#t") (make-atom "#t"))
      (check-equal? (parse "#f") (make-atom "#f"))
      (check-equal? (parse "#T") (make-atom "#T"))
      (check-equal? (parse "#F") (make-atom "#F")))
     
     (test-case
      "parse simple s-expression"
      (check-equal? (parse "(hello world)")
                    (make-fusion "(" (list (make-atom "hello")
                                           (make-space " ")
                                           (make-atom "world"))
                                 ")")))
     
     (test-case
      "boxed value"
      (check-equal? (parse "#&5") (make-fusion "#&" (list (make-atom "5")) ""))
      (check-equal? (parse "#&()")
                    (make-fusion "#&"
                                 (list (make-fusion "(" '() ")"))
                                 "")))
     
     (test-case
      "vector with size"
      (check-equal? (parse "#5(1)")
                    (make-fusion "#5(" (list (make-atom "1")) ")")))
     
     (test-case
      "quoted"
      (check-equal?
       (parse "'42")
       (make-fusion "'" (list (make-atom "42")) ""))
      (check-equal?
       (parse "'(foo!)")
       (make-fusion "'" (list (make-fusion "(" (list (make-atom "foo!")) ")")) ""))
      (check-equal?
       (parse "``dyoo")
       (make-fusion "`" (list (make-fusion "`" (list (make-atom "dyoo")) "")) ""))
      (check-equal?
       (parse "#``dyoo")
       (make-fusion "#`" (list (make-fusion "`" (list (make-atom "dyoo")) "")) ""))
      (check-equal?
       (parse ",@ugh")
       (make-fusion ",@" (list (make-atom "ugh")) ""))
      (check-equal?
       (parse "#,@ugh")
       (make-fusion "#,@" (list (make-atom "ugh")) "")))
     
     (test-case
      "pipe literal"
      (check-equal? (parse "#\\|")
                    (make-atom "#\\|"))
      (check-equal? (parse "(#\\| #\\|)")
                    (make-fusion "("
                                 (list (make-atom "#\\|")
                                       (make-space " ")
                                       (make-atom "#\\|"))
                                 ")"))
      
      (check-equal? (parse "(#\\| #rx#\"|\")")
                    (make-fusion "("
                                 (list (make-atom "#\\|")
                                       (make-space " ")
                                       (make-atom "#rx#\"|\""))
                                 ")")))
     
     (test-case
      "character constants (plus a nonsense one)"
      (for-each (lambda (x) (check-equal? (parse x) (make-atom x)))
                '("#\\nul"
                  "#\\null"
                  "#\\backspace"
                  "#\\tab"
                  "#\\newline"
                  "#\\linefeed"
                  "#\\vtab"
                  "#\\vtab"
                  "#\\page"
                  "#\\return"
                  "#\\space"
                  "#\\rubout"
                  "#\\blah"
                  "#\\42"
                  "#\\u42"
                  "#\\U42")))
     
     (test-case
      "strings"
      (check-equal? (parse "\"hello world\"")
                    (make-atom "\"hello world\""))
      (check-equal? (parse "(\"hello\" \"world\")")
                    (make-fusion "("
                                 (list (make-atom "\"hello\"")
                                       (make-space " ")
                                       (make-atom "\"world\""))
                                 ")"))
      (check-equal? (parse "#\"hello\"")
                    (make-atom "#\"hello\"")))
     
     (test-case
      "strings 2"
      (check-equal? (parse "#rx\"foo\"")
                    (make-atom "#rx\"foo\""))
      (check-equal? (parse "#rx#\"^([^|]*)\\\\|\"")
                    (make-atom "#rx#\"^([^|]*)\\\\|\"")))
     
     (test-case
      "weird symbols"
      (check-equal? (parse "a\\(b")
                    (make-atom "a\\(b"))
      (check-equal? (parse "a| |b")
                    (make-atom "a| |b"))
      (check-equal? (parse "|a||b|")
                    (make-atom "|a||b|"))
      (check-equal? (parse "(|a| |b|)")
                    (make-fusion "("
                                 (list (make-atom "|a|")
                                       (make-space " ")
                                       (make-atom "|b|"))
                                 ")")))
     
     (test-case
      "keywords"
      (check-equal? (parse "#:some-keyword")
                    (make-atom "#:some-keyword"))
      (check-equal? (parse "#:#:x")
                    (make-atom "#:#:x")))
     
     (test-case
      "numeric constants"
      (check-equal? (parse "#x3F")
                    (make-atom "#x3F")))
     
     (test-case
      "hash constants"
      (check-equal?
       (parse "#hash((3 4))")
       (make-fusion "#hash("
                    (list (make-fusion
                           "("
                           (list (make-atom "3") (make-space " ") (make-atom "4"))
                           ")"))
                    ")")))
     
     (test-case
      "nested comments"
      (check-equal? (parse "#||#") (make-atom "#||#")))
     (test-case
      "nested comments 2"
      (check-equal? (parse "#|#||#|#") (make-atom "#|#||#|#")))
     (test-case
      "nested comments 3"
      (check-equal? (parse "#|\n#||#\n|#") (make-atom "#|\n#||#\n|#")))
     
     
     (test-case
      "simple graphical symbol test"
      (call-with-graphical-snip-port parse-port))))
  
  
  (test/text-ui parse-plt-scheme-tests))