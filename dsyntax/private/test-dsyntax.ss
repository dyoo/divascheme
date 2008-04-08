(module test-dsyntax mzscheme
  (require "dsyntax.ss"
           (only "move.ss" current-line-break-mode)
           "parse-plt-scheme.ss"
           (lib "etc.ss")
           (planet "test.ss" ("schematics" "schemeunit.plt" 2 8))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2 8)))
  
  
  (provide dsyntax-tests)
  
  ;; As an ugly test case, I will be using the sq function.
  (define sq-function-dstx
    (make-fusion "(" `(,(make-atom "define")
                       ,(make-fusion "("
                                     `(,(make-atom "sq")
                                       ,(make-atom "x"))
                                     ")")
                       ,(make-fusion "("
                                     `(,(make-atom "*")
                                       ,(make-atom "x")
                                       ,(make-atom "x"))
                                     ")"))
                 ")"))
  
  
  (define dsyntax-tests
    (test-suite
     "dsyntax.ss"
     
     (test-case
      "initial focus"
      (local ((define cur
                (make-toplevel-cursor
                 (list (make-atom "hello") (make-atom "world")))))
        (check-equal? (cursor-line cur) 1)
        (check-equal? (cursor-col cur) 0)))
     
     (test-case
      "loc-after an atom"
      (check-equal? (loc-after (make-loc 1 0 0)
                               (make-atom "x"))
                    (make-loc 1 1 1))
      (check-equal? (loc-after (make-loc 1 0 0)
                               (make-atom "hello"))
                    (make-loc 1 5 5)))
     
     (test-case
      "loc-after a space"
      (check-equal? (loc-after (make-loc 1 0 0)
                               (make-space "    "))
                    (make-loc 1 4 4)))
     
     (test-case
      "loc-after a tab"
      (check-equal? (loc-after (make-loc 1 0 0)
                               (make-space "     \t     "))
                    (make-loc 1 13 11)))
     
     (test-case
      "loc-after an atom with whitespace and newlines"
      (check-equal? (loc-after (make-loc 1 0 0)
                               (make-atom "|hello world|"))
                    (make-loc 1 13 13))
      (check-equal? (loc-after (make-loc 1 0 0)
                               (make-atom "|hello\nworld|"))
                    (make-loc 2 6 13))
      (check-equal? (loc-after (make-loc 1 0 0)
                               (make-atom "|hello\tworld|"))
                    (make-loc 1 14 13)))
     
     (test-case
      "Slightly evil example with returns as well.
       Line breaks are parameterized." 
      (parameterize ([current-line-break-mode 'linefeed])
        (check-equal? (loc-after (make-loc 1 0 0)
                                 (make-atom "|\r\nfoo\rbar|"))
                      (make-loc 2 8 10)))
      (parameterize ([current-line-break-mode 'any-one])
        (check-equal? (loc-after (make-loc 1 0 0)
                                 (make-atom "|\r\nfoo\rbar|"))
                      (make-loc 4 4 11))))
     
     (test-case
      "loc-after a half empty fusion"
      (check-equal? (loc-after (make-loc 1 0 0)
                               (make-fusion "(" '() ""))
                    (make-loc 1 1 1)))
     
     (test-case
      "loc-after another half empty fusion"
      (check-equal? (loc-after (make-loc 1 0 0)
                               (make-fusion "" '() "}"))
                    (make-loc 1 1 1)))
     
     (test-case
      "loc-after yet another half empty fusion"
      (check-equal? (loc-after (make-loc 1 0 0)
                               (make-fusion "" '() ""))
                    (make-loc 1 0 0)))
     
     (test-case
      "loc-after an empty fusion"
      (check-equal? (loc-after (make-loc 1 0 0)
                               (make-fusion "(" '() ")"))
                    (make-loc 1 2 2)))
     
     (test-case
      "loc-after an empty fusion 2"
      (check-equal? (loc-after (make-loc 1 0 0)
                               (make-fusion "[[" '() ")"))
                    (make-loc 1 3 3)))
     
     (test-case
      "loc-after an empty fusion 3"
      (check-equal? (loc-after (make-loc 1 0 0)
                               (make-fusion "{" '() "}}"))
                    (make-loc 1 3 3)))
     
     (test-case
      "loc-after (hello\n world)"
      (check-equal? (loc-after (make-loc 1 0 0)
                               (make-fusion "("
                                            (list (make-atom "hello")
                                                  (make-space "\n ")
                                                  (make-atom "world"))
                                            ")"))
                    (make-loc 2 7 14)))
     
     (test-case
      "cursor-endpos"
      (local ((define cur
                (make-toplevel-cursor
                 (list (make-atom "hello") (make-atom "world"))))
              (define next-cur (focus-successor cur)))
        (check-equal? (cursor-pos cur) 0)
        (check-equal? (cursor-endpos cur) 5)
        (check-equal? (cursor-pos next-cur) 5)
        (check-equal? (cursor-endpos next-cur) 10)))
     
     
     (test-case
      "loc-after composite (hello\n (world\n))"
      (check-equal?
       (loc-after
        (make-loc 1 0 0)
        (make-fusion "("
                     (list (make-atom "hello")
                           (make-space "\n ")
                           (make-fusion "("
                                        (list (make-atom "world")
                                              (make-space "\n"))
                                        ")"))
                     ")"))
       (make-loc 3 2 17)))
     
     (test-case
      "make-toplevel-cursor"
      (check-equal? (make-toplevel-cursor (list (make-atom "someatom")))
                    (make-cursor (make-atom "someatom")
                                 (make-loc 1 0 0)
                                 #f
                                 '()
                                 '()
                                 '()))
      
      (check-equal? (make-toplevel-cursor (list (make-atom "someatom")
                                                (make-atom "elder")))
                    (make-cursor (make-atom "someatom")
                                 (make-loc 1 0 0)
                                 #f
                                 '()
                                 '()
                                 (list (make-atom "elder")))))
     
     (test-case
      "cursor-line and cursor-col"
      (check-equal?
       (cursor-line (make-toplevel-cursor (list (make-atom "x"))))
       1)
      (check-equal?
       (cursor-col (make-toplevel-cursor (list (make-atom "x"))))
       0 1)
      (check-equal?
       (cursor-line (make-cursor (make-atom "arg")
                                 (make-loc 2 4 5)
                                 #f
                                 (list (make-atom "blah")
                                       (make-space "\n"))
                                 `(,(make-loc 2 0 1)
                                   ,(make-loc 1 0 0))
                                 '()))
       2)
      (check-equal?
       (cursor-col (make-cursor (make-atom "arg")
                                (make-loc 2 4 5)
                                #f
                                (list (make-atom "blah")
                                      (make-space "\n"))
                                `(,(make-loc 2 0 1)
                                  ,(make-loc 1 0 0))
                                '()))
       4))
     
     (test-case
      "cursor-dstx"
      (check-equal?
       (cursor-dstx (make-toplevel-cursor (list (make-atom "focus!"))))
       (make-atom "focus!")))
     
     (test-case
      "focus-in"
      (check-false
       (focus-in (make-toplevel-cursor
                  (list (make-atom "should-fail")))))
      (check-false
       (focus-in (make-toplevel-cursor
                  (list (make-space "\n\n")))))
      (check-false
       (focus-in (make-toplevel-cursor
                  (list (make-fusion "#(" '() ")")))))
      (local
          ((define original-cursor
             (make-toplevel-cursor
              (list (make-fusion "[" (list (make-atom "x")) "]")))))
        (check-equal?
         (focus-in original-cursor)
         (make-cursor
          (make-atom "x") (make-loc 1 1 1) original-cursor '() '() '())))
      (local
          ((define original-cursor
             (make-toplevel-cursor
              (list (make-fusion "#(" (list (make-atom "42")) ")")))))
        (check-equal?
         (focus-in original-cursor)
         (make-cursor (make-atom "42")
                      (make-loc 1 2 2)
                      original-cursor '() '() '()))))
     
     (test-case
      "focus-in should skip space"
      (local
          ((define original-cursor
             (make-toplevel-cursor
              (list (make-fusion "#("
                                 (list (make-space "    ")
                                       (make-atom "42"))
                                 ")")))))
        (check-equal?
         (focus-in original-cursor)
         (make-cursor
          (make-atom "42") (make-loc 1 6 6) original-cursor
          `(,(make-space "    ")) `(,(make-loc 1 2 2)) '()))))
     
     
     (test-case
      "focus-out and focus-in-out"
      (check-false
       (focus-out (make-toplevel-cursor (list (make-atom "no go")))))
      (local ((define my-dstx (make-fusion "<<<"
                                           `(,(make-atom "ok"))
                                           ">>>"))
              (define toplevel-cursor
                (make-toplevel-cursor (list my-dstx))))
        (check-equal?
         (make-cursor
          (make-atom "ok") (make-loc 1 3 3) toplevel-cursor '() '() '())
         (focus-in toplevel-cursor))
        (check-equal?
         (make-cursor my-dstx (make-loc 1 0 0) #f '() '() '())
         (focus-out (focus-in toplevel-cursor)))))
     
     (test-case
      "focus-older"
      (local
          ((define simple-dstx (make-fusion "("
                                            `(,(make-atom "hello")
                                              ,(make-atom "world"))
                                            ")"))
           (define top (make-toplevel-cursor (list simple-dstx))))
        (check-equal? (cursor-dstx (focus-older (focus-in top)))
                      (make-atom "world"))
        (check-equal? (cursor-loc (focus-older (focus-in top)))
                      (make-loc 1 6 6))))
     
     
     (test-case
      "focus-older with space"
      (local
          ((define simple-dstx (make-fusion "("
                                            `(,(make-atom "hello")
                                              ,(make-space " ")
                                              ,(make-atom "world"))
                                            ")"))
           (define top (make-toplevel-cursor (list simple-dstx))))
        (check-equal? (cursor-dstx (focus-older (focus-in top)))
                      (make-atom "world"))
        (check-equal? (cursor-loc (focus-older (focus-in top)))
                      (make-loc 1 7 7))))
     
     
     
     (test-case
      "focus-younger"
      (check-false
       (focus-younger (make-toplevel-cursor (list (make-atom "huh?")))))
      (check-false
       (focus-younger (make-toplevel-cursor (list (make-space "")))))
      (check-false
       (focus-younger (make-toplevel-cursor
                       (list (make-fusion "[" '() "]")))))
      (local
          ((define simple-dstx (make-fusion "("
                                            `(,(make-atom "hello")
                                              ,(make-atom "world"))
                                            ")"))
           (define top (make-toplevel-cursor (list simple-dstx)))
           (define new-cursor
             (focus-younger (focus-older (focus-in top)))))
        (check-equal?
         (cursor-dstx (focus-younger (focus-older (focus-in top))))
         (make-atom "hello"))))
     
     (test-case
      "focus-younger with space"
      (local
          ((define simple-dstx (make-fusion "("
                                            `(,(make-atom "hello")
                                              ,(make-space " ")
                                              ,(make-space "\n ")
                                              ,(make-atom "world"))
                                            ")"))
           (define top (make-toplevel-cursor (list simple-dstx)))
           (define new-cursor
             (focus-younger (focus-older (focus-in top)))))
        (check-equal?
         (cursor-dstx (focus-younger (focus-older (focus-in top))))
         (make-atom "hello"))
        (check-equal?
         (cursor-loc (focus-younger (focus-older (focus-in top))))
         (make-loc 1 1 1))))
     
     
     (test-case
      "focus-successor"
      (local
          ((define top (make-toplevel-cursor (list sq-function-dstx)))
           (define s focus-successor))
        (check-equal? (cursor-dstx (s top))
                      (make-atom "define"))
        (check-equal? (cursor-dstx (s (s top)))
                      (make-fusion "("
                                   `(,(make-atom "sq")
                                     ,(make-atom "x"))
                                   ")"))
        (check-equal? (cursor-dstx (s (s (s top))))
                   (make-atom "sq"))
        (check-equal? (cursor-dstx (s (s (s (s top)))))
                   (make-atom "x"))
        (check-equal? (cursor-dstx (s (s (s (s (s top))))))
                   (make-fusion "("
                                `(,(make-atom "*")
                                  ,(make-atom "x")
                                  ,(make-atom "x"))
                                ")"))
        (check-equal? (cursor-dstx (s (s (s (s (s (s top)))))))
                   (make-atom "*"))
        (check-equal? (cursor-dstx (s (s (s (s (s (s (s top))))))))
                   (make-atom "x"))
        (check-equal? (cursor-dstx (s (s (s (s (s (s (s (s top)))))))))
                   (make-atom "x"))
        (check-false (s (s (s (s (s (s (s (s (s top))))))))))))
     
     (test-case
      "focus-predecessor"
      (local ((define top (make-toplevel-cursor (list sq-function-dstx)))
              (define s focus-successor)
              (define p focus-predecessor)
              (define last (let loop ([dstx top])
                             (cond
                               [(s dstx) => loop]
                               [else dstx]))))
        (check-equal? (cursor-dstx last) (make-atom "x"))
        (check-equal? (cursor-dstx (p last)) (make-atom "x"))
        (check-equal? (cursor-dstx (p (p last))) (make-atom "*"))
        (check-equal?
         (cursor-dstx (p (p (p last))))
         (make-fusion "("
                      `(,(make-atom "*")
                        ,(make-atom "x")
                        ,(make-atom "x"))
                      ")"))
        (check-equal?
         (cursor-dstx (p (p (p (p last))))) (make-atom "x"))
        (check-equal?
         (cursor-dstx (p (p (p (p (p last)))))) (make-atom "sq"))
        (check-equal?
         (cursor-dstx (p (p (p (p (p (p last)))))))
         (make-fusion "("
                      `(,(make-atom "sq")
                        ,(make-atom "x"))
                      ")"))
        (check-equal? (cursor-dstx (p (p (p (p (p (p (p last))))))))
                      (make-atom "define"))
        (check-equal? (p (p (p (p (p (p (p (p last))))))))
                      top)))
     
     
     (test-case
      "back and forth"
      (local ((define c
                (make-toplevel-cursor
                 (parse-port
                  (open-input-string "(hello (world) this is a test)")))))
        (check-equal? (cursor-dstx
                       (focus-successor c))
                      (make-atom "hello"))
        (check-equal? (cursor-dstx
                       (focus-successor
                        (focus-successor c)))
                      (make-fusion "(" (list (make-atom "world")) ")"))
        (check-equal? (cursor-dstx
                       (focus-predecessor
                        (focus-successor
                         (focus-successor c))))
                      (make-atom "hello"))
        (check-equal? (cursor-dstx
                       (focus-successor
                        (focus-successor
                         (focus-predecessor
                          (focus-successor
                           (focus-successor c))))))
                      (make-atom "world"))))))
  
  
  
  
  
  (test/text-ui
   (test-suite
    "all tests"
    dsyntax-tests)))