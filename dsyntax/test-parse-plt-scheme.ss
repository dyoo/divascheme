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
  
  
  (provide test-parse-plt-scheme)
  
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
  
  
  (define test-parse-plt-scheme
    (test-suite
     "test-parse-plt-scheme.ss"
     
     (test-case
      "empty input should give us empty list."
      (check-equal? (parse-port (open-input-string "")) '()))
     
     (test-case
      "open-input-text with lparen."
      (let ([text (new scheme:text%)])
        (send text insert "(")
        (let ([ip (open-input-text text 0 1)])
          (check-equal? #\( (read-char ip))
          (check-true (eof-object? (read-char ip))))))
     
     (test-case
      "open-input-text with rparen."
      (let ([text (new scheme:text%)])
        (send text insert ")")
        (let ([ip (open-input-text text 0 1)])
          (check-equal? #\) (read-char ip))
          (check-true (eof-object? (read-char ip))))))
     
     (test-case
      "open-input-text with rparen: parse should fail"
      (let ([text (new scheme:text%)])
        (send text insert ")")
        (let ([ip (open-input-text text 0 1)])
          (check-exn exn:fail? (lambda () (parse-port ip))))))
     
     (test-case
      "simple test of atom"
      (check-equal?
       (parse "hello")
       (new-atom "hello")))
     
     (test-case
      "pound-percent"
      (check-equal?
       (parse "#%datum")
       (new-atom "#%datum")))
     
     (test-case
      "parsing #lang"
      (check-equal?
       (parse "#lang")
       (new-atom "#lang")))
     
     (test-case
      "parse vector"
      (check-equal?
       (parse "#(1)")
       (new-fusion "#(" (list (new-atom "1")) ")")))
     
     (test-case
      "true and false"
      (check-equal? (parse "#t") (new-atom "#t"))
      (check-equal? (parse "#f") (new-atom "#f"))
      (check-equal? (parse "#T") (new-atom "#T"))
      (check-equal? (parse "#F") (new-atom "#F")))
     
     (test-case
      "parse simple s-expression"
      (check-equal? (parse "(hello world)")
                    (new-fusion "(" (list (new-atom "hello")
                                          (new-space " ")
                                          (new-atom "world"))
                                 ")")))
     
     (test-case
      "boxed value"
      (check-equal? (parse "#&5") (new-fusion "#&" (list (new-atom "5")) ""))
      (check-equal? (parse "#&()")
                    (new-fusion "#&"
                                 (list (new-fusion "(" '() ")"))
                                 "")))
     
     (test-case
      "vector with size"
      (check-equal? (parse "#5(1)")
                    (new-fusion "#5(" (list (new-atom "1")) ")")))
     
     (test-case
      "quoted"
      (check-equal?
       (parse "'42")
       (new-fusion "'" (list (new-atom "42")) ""))
      (check-equal?
       (parse "'(foo!)")
       (new-fusion "'" (list (new-fusion "(" (list (new-atom "foo!")) ")")) ""))
      (check-equal?
       (parse "``dyoo")
       (new-fusion "`" (list (new-fusion "`" (list (new-atom "dyoo")) "")) ""))
      (check-equal?
       (parse "#``dyoo")
       (new-fusion "#`" (list (new-fusion "`" (list (new-atom "dyoo")) "")) ""))
      (check-equal?
       (parse ",@ugh")
       (new-fusion ",@" (list (new-atom "ugh")) ""))
      (check-equal?
       (parse "#,@ugh")
       (new-fusion "#,@" (list (new-atom "ugh")) "")))
     
     
     (test-case
      "space after quote should be swallowed in the fusion"
      (check-equal?
       (parse "' hello")
       (new-fusion "'" (list (new-space " ") (new-atom "hello")) "")))
     
     
     (test-case
      "pipe literal"
      (check-equal? (parse "#\\|")
                    (new-atom "#\\|"))
      (check-equal? (parse "(#\\| #\\|)")
                    (new-fusion "("
                                (list (new-atom "#\\|")
                                      (new-space " ")
                                      (new-atom "#\\|"))
                                ")"))
      
      (check-equal? (parse "(#\\| #rx#\"|\")")
                    (new-fusion "("
                                (list (new-atom "#\\|")
                                       (new-space " ")
                                       (new-atom "#rx#\"|\""))
                                 ")")))
     
     (test-case
      "character constants (plus a nonsense one)"
      (for-each (lambda (x) (check-equal? (parse x) (new-atom x)))
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
                    (new-atom "\"hello world\""))
      (check-equal? (parse "(\"hello\" \"world\")")
                    (new-fusion "("
                                 (list (new-atom "\"hello\"")
                                       (new-space " ")
                                       (new-atom "\"world\""))
                                 ")"))
      (check-equal? (parse "#\"hello\"")
                    (new-atom "#\"hello\"")))
     
     (test-case
      "strings 2"
      (check-equal? (parse "#rx\"foo\"")
                    (new-atom "#rx\"foo\""))
      (check-equal? (parse "#rx#\"^([^|]*)\\\\|\"")
                    (new-atom "#rx#\"^([^|]*)\\\\|\"")))
     
     (test-case
      "weird symbols"
      (check-equal? (parse "a\\(b")
                    (new-atom "a\\(b"))
      (check-equal? (parse "a| |b")
                    (new-atom "a| |b"))
      (check-equal? (parse "|a||b|")
                    (new-atom "|a||b|"))
      (check-equal? (parse "(|a| |b|)")
                    (new-fusion "("
                                 (list (new-atom "|a|")
                                       (new-space " ")
                                       (new-atom "|b|"))
                                 ")")))
     
     (test-case
      "keywords"
      (check-equal? (parse "#:some-keyword")
                    (new-atom "#:some-keyword"))
      (check-equal? (parse "#:#:x")
                    (new-atom "#:#:x")))
     
     (test-case
      "numeric constants"
      (check-equal? (parse "#x3F")
                    (new-atom "#x3F")))
     
     (test-case
      "hash constants"
      (check-equal?
       (parse "#hash((3 4))")
       (new-fusion "#hash("
                    (list (new-fusion
                           "("
                           (list (new-atom "3") (new-space " ") (new-atom "4"))
                           ")"))
                    ")")))
     
     (test-case
      "nested comments"
      (check-equal? (parse "#||#") (new-atom "#||#")))
     (test-case
      "nested comments 2"
      (check-equal? (parse "#|#||#|#") (new-atom "#|#||#|#")))
     (test-case
      "nested comments 3"
      (check-equal? (parse "#|\n#||#\n|#") (new-atom "#|\n#||#\n|#")))
     
     (test-case
      "empty keyword"
      (check-equal? (parse "(#:)") (new-fusion "(" (list (new-atom "#:")) ")")))
     
     
     (test-case
      "just close paren should raise exception"
      (check-exn exn:fail? (lambda () (parse ")"))))
     
     
     (test-case
      "just open paren should raise exception"
      (check-exn exn:fail? (lambda () (parse "("))))
     
     
     (test-case
      "simple graphical symbol test"
      (call-with-graphical-snip-port parse-port))))
  
  (define (test)
    (test/text-ui test-parse-plt-scheme)))