(module test-dsyntax mzscheme
  (require "dsyntax.ss"
           #;(only "move.ss" current-line-break-mode)
           "parse-plt-scheme.ss"
           (lib "etc.ss")
           (planet "test.ss" ("schematics" "schemeunit.plt" 2 8))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2 8)))
  
  
  (provide test-dsyntax)
  
  ;; As an ugly test case, I will be using the sq function.
  (define sq-function-dstx
    (new-fusion "(" `(,(new-atom "define")
                       ,(new-fusion "("
                                     `(,(new-atom "sq")
                                       ,(new-atom "x"))
                                     ")")
                       ,(new-fusion "("
                                     `(,(new-atom "*")
                                       ,(new-atom "x")
                                       ,(new-atom "x"))
                                     ")"))
                 ")"))
  
  
  (define test-dsyntax
    (test-suite
     "test-dsyntax.ss"
     
     (test-case
      "initial focus"
      (local ((define cur
                (make-toplevel-cursor
                 (list (new-atom "hello") (new-atom "world")))))
        (check-equal? (cursor-line cur) 1)
        (check-equal? (cursor-col cur) 0)))
     
     (test-case
      "loc-after an atom"
      (check-equal? (loc-after (make-loc 1 0 0)
                               (new-atom "x"))
                    (make-loc 1 1 1))
      (check-equal? (loc-after (make-loc 1 0 0)
                               (new-atom "hello"))
                    (make-loc 1 5 5)))
     
     (test-case
      "loc-after a space"
      (check-equal? (loc-after (make-loc 1 0 0)
                               (new-space "    "))
                    (make-loc 1 4 4)))
     
     (test-case
      "loc-after a tab"
      (check-equal? (loc-after (make-loc 1 0 0)
                               (new-space "     \t     "))
                    (make-loc 1 13 11)))
     
     (test-case
      "loc-after an atom with whitespace and newlines"
      (check-equal? (loc-after (make-loc 1 0 0)
                               (new-atom "|hello world|"))
                    (make-loc 1 13 13))
      (check-equal? (loc-after (make-loc 1 0 0)
                               (new-atom "|hello\nworld|"))
                    (make-loc 2 6 13))
      (check-equal? (loc-after (make-loc 1 0 0)
                               (new-atom "|hello\tworld|"))
                    (make-loc 1 14 13)))
     
     #;(test-case
        "Slightly evil example with returns as well.
       Line breaks are parameterized." 
      (parameterize ([current-line-break-mode 'linefeed])
        (check-equal? (loc-after (make-loc 1 0 0)
                                 (new-atom "|\r\nfoo\rbar|"))
                      (make-loc 2 8 10)))
      (parameterize ([current-line-break-mode 'any-one])
        (check-equal? (loc-after (make-loc 1 0 0)
                                 (new-atom "|\r\nfoo\rbar|"))
                      (make-loc 4 4 11))))
     
     (test-case
      "loc-after a half empty fusion"
      (check-equal? (loc-after (make-loc 1 0 0)
                               (new-fusion "(" '() ""))
                    (make-loc 1 1 1)))
     
     (test-case
      "loc-after another half empty fusion"
      (check-equal? (loc-after (make-loc 1 0 0)
                               (new-fusion "" '() "}"))
                    (make-loc 1 1 1)))
     
     (test-case
      "loc-after yet another half empty fusion"
      (check-equal? (loc-after (make-loc 1 0 0)
                               (new-fusion "" '() ""))
                    (make-loc 1 0 0)))
     
     (test-case
      "loc-after an empty fusion"
      (check-equal? (loc-after (make-loc 1 0 0)
                               (new-fusion "(" '() ")"))
                    (make-loc 1 2 2)))
     
     (test-case
      "loc-after an empty fusion 2"
      (check-equal? (loc-after (make-loc 1 0 0)
                               (new-fusion "[[" '() ")"))
                    (make-loc 1 3 3)))
     
     (test-case
      "loc-after an empty fusion 3"
      (check-equal? (loc-after (make-loc 1 0 0)
                               (new-fusion "{" '() "}}"))
                    (make-loc 1 3 3)))
     
     (test-case
      "loc-after (hello\n world)"
      (check-equal? (loc-after (make-loc 1 0 0)
                               (new-fusion "("
                                            (list (new-atom "hello")
                                                  (new-space "\n ")
                                                  (new-atom "world"))
                                            ")"))
                    (make-loc 2 7 14)))
     
     (test-case
      "cursor-endpos"
      (local ((define cur
                (focus-successor/no-snap
                 (make-toplevel-cursor
                  (list (new-atom "hello") (new-atom "world")))))
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
        (new-fusion "("
                    (list (new-atom "hello")
                          (new-space "\n ")
                          (new-fusion "("
                                        (list (new-atom "world")
                                              (new-space "\n"))
                                        ")"))
                     ")"))
       (make-loc 3 2 17)))
     
     (test-case
      "make-toplevel-cursor"
      (let ([result (make-toplevel-cursor (list (new-atom "someatom")))])
        (check-equal? (cursor-dstx result)
                      a-sentinel-space)
        (check-equal? (cursor-loc result)
                      (make-loc 1 0 0))
        (check-equal? (cursor-parent result) #f)
        (check-equal? (cursor-youngers-rev result) '())
        (check-equal? (cursor-youngers-loc-rev result) '())
        (check-equal? (cursor-olders result) (list (new-atom "someatom")))))
     
     
     (test-case
      "make-toplevel-cursor 2"
      (let ([result (make-toplevel-cursor (list (new-atom "someatom")
                                                (new-atom "elder")))])
        (check-equal? result
                      (make-cursor a-sentinel-space
                                   (make-loc 1 0 0)
                                   #f
                                   '()
                                   '()
                                   (list (new-atom "someatom")
                                         (new-atom "elder"))))))
     
     (test-case
      "cursor-line and cursor-col"
      (check-equal?
       (cursor-line (make-toplevel-cursor (list (new-atom "x"))))
       1)
      (check-equal?
       (cursor-col (make-toplevel-cursor (list (new-atom "x"))))
       0 1)
      (check-equal?
       (cursor-line (make-cursor (new-atom "arg")
                                 (make-loc 2 4 5)
                                 #f
                                 (list (new-atom "blah")
                                       (new-space "\n"))
                                 `(,(make-loc 2 0 1)
                                   ,(make-loc 1 0 0))
                                 '()))
       2)
      (check-equal?
       (cursor-col (make-cursor (new-atom "arg")
                                (make-loc 2 4 5)
                                #f
                                (list (new-atom "blah")
                                      (new-space "\n"))
                                `(,(make-loc 2 0 1)
                                  ,(make-loc 1 0 0))
                                '()))
       4))
     
     (test-case
      "cursor-dstx"
      (check-equal?
       (cursor-dstx (make-toplevel-cursor (list (new-atom "focus!"))))
       (new-atom "focus!")))
     
     (test-case
      "focus-in"
      (check-false
       (focus-in (make-toplevel-cursor
                  (list (new-atom "should-fail")))))
      (check-false
       (focus-in (make-toplevel-cursor
                  (list (new-space "\n\n")))))
      (check-false
       (focus-in (make-toplevel-cursor
                  (list (new-fusion "#(" '() ")")))))
      (local
          ((define original-cursor
             (make-toplevel-cursor
              (list (new-fusion "[" (list (new-atom "x")) "]")))))
        (check-equal?
         (focus-in original-cursor)
         (make-cursor
          (new-atom "x")
          (make-loc 1 1 1)
          original-cursor
          (list (new-space ""))
          (list (make-loc 1 1 1))
          '())))
      (local
        ((define original-cursor
           (make-toplevel-cursor
            (list (new-fusion "#(" (list (new-atom "42")) ")")))))
        (check-equal?
         (focus-in original-cursor)
         (make-cursor (new-atom "42")
                      (make-loc 1 2 2)
                      original-cursor
                      (list (new-space ""))
                      (list (make-loc 1 2 2))
                      '()))))
     
     (test-case
      "focus-in should skip space"
      (local
        ((define original-cursor
           (make-toplevel-cursor
            (list (new-fusion "#("
                              (list (new-space "    ")
                                    (new-atom "42"))
                              ")")))))
        (check-equal?
         (focus-in original-cursor)
         (make-cursor (new-atom "42")
                      (make-loc 1 6 6)
                      original-cursor
                      (list (new-space "    ") (new-space ""))
                      (list (make-loc 1 2 2) (make-loc 1 2 2))
                      '()))))
     
     
     (test-case
      "focus-out and focus-in-out"
      (check-false
       (focus-out (make-toplevel-cursor (list (new-atom "no go")))))
      (local ((define my-dstx (new-fusion "<<<"
                                          `(,(new-atom "ok"))
                                          ">>>"))
              (define toplevel-cursor
                (make-toplevel-cursor (list my-dstx))))
        (check-equal? (focus-in toplevel-cursor)
                      (make-cursor (new-atom "ok")
                                   (make-loc 1 3 3)
                                   toplevel-cursor
                                   (list (new-space ""))
                                   (list (make-loc 1 3 3))
                                   '()))
        (check-equal? (focus-out (focus-in toplevel-cursor))
                      (make-cursor my-dstx
                                   (make-loc 1 0 0)
                                   #f
                                   '()
                                   '()
                                   '()))))
     
     (test-case
      "focus-older"
      (local
        ((define simple-dstx (new-fusion "("
                                         `(,(new-atom "hello")
                                           ,(new-atom "world"))
                                         ")"))
         (define top (make-toplevel-cursor (list simple-dstx))))
        (check-equal? (cursor-dstx (focus-older (focus-in top)))
                      (new-atom "world"))
        (check-equal? (cursor-loc (focus-older (focus-in top)))
                      (make-loc 1 6 6))))
     
     
     (test-case
      "focus-older with space"
      (local
          ((define simple-dstx (new-fusion "("
                                            `(,(new-atom "hello")
                                              ,(new-space " ")
                                              ,(new-atom "world"))
                                            ")"))
           (define top (make-toplevel-cursor (list simple-dstx))))
        (check-equal? (cursor-dstx (focus-older (focus-in top)))
                      (new-atom "world"))
        (check-equal? (cursor-loc (focus-older (focus-in top)))
                      (make-loc 1 7 7))))
     
     
     
     (test-case
      "focus-younger"
      (check-false
       (focus-younger (make-toplevel-cursor (list (new-atom "huh?")))))
      (check-false
       (focus-younger (make-toplevel-cursor (list (new-space "")))))
      (check-false
       (focus-younger (make-toplevel-cursor
                       (list (new-fusion "[" '() "]")))))
      (local
          ((define simple-dstx (new-fusion "("
                                            `(,(new-atom "hello")
                                              ,(new-atom "world"))
                                            ")"))
           (define top (make-toplevel-cursor (list simple-dstx)))
           (define new-cursor
             (focus-younger (focus-older (focus-in top)))))
        (check-equal?
         (cursor-dstx (focus-younger (focus-older (focus-in top))))
         (new-atom "hello"))))
     
     (test-case
      "focus-younger with space"
      (local
          ((define simple-dstx (new-fusion "("
                                            `(,(new-atom "hello")
                                              ,(new-space " ")
                                              ,(new-space "\n ")
                                              ,(new-atom "world"))
                                            ")"))
           (define top (make-toplevel-cursor (list simple-dstx)))
           (define new-cursor
             (focus-younger (focus-older (focus-in top)))))
        (check-equal?
         (cursor-dstx (focus-younger (focus-older (focus-in top))))
         (new-atom "hello"))
        (check-equal?
         (cursor-loc (focus-younger (focus-older (focus-in top))))
         (make-loc 1 1 1))))
     
     
     (test-case
      "focus-successor"
      (local
          ((define top (make-toplevel-cursor (list sq-function-dstx)))
           (define s focus-successor))
        (check-equal? (cursor-dstx (s top))
                      (new-atom "define"))
        (check-equal? (cursor-dstx (s (s top)))
                      (new-fusion "("
                                   `(,(new-atom "sq")
                                     ,(new-atom "x"))
                                   ")"))
        (check-equal? (cursor-dstx (s (s (s top))))
                   (new-atom "sq"))
        (check-equal? (cursor-dstx (s (s (s (s top)))))
                   (new-atom "x"))
        (check-equal? (cursor-dstx (s (s (s (s (s top))))))
                   (new-fusion "("
                                `(,(new-atom "*")
                                  ,(new-atom "x")
                                  ,(new-atom "x"))
                                ")"))
        (check-equal? (cursor-dstx (s (s (s (s (s (s top)))))))
                   (new-atom "*"))
        (check-equal? (cursor-dstx (s (s (s (s (s (s (s top))))))))
                   (new-atom "x"))
        (check-equal? (cursor-dstx (s (s (s (s (s (s (s (s top)))))))))
                   (new-atom "x"))
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
        (check-equal? (cursor-dstx last) (new-atom "x"))
        (check-equal? (cursor-dstx (p last)) (new-atom "x"))
        (check-equal? (cursor-dstx (p (p last))) (new-atom "*"))
        (check-equal?
         (cursor-dstx (p (p (p last))))
         (new-fusion "("
                      `(,(new-atom "*")
                        ,(new-atom "x")
                        ,(new-atom "x"))
                      ")"))
        (check-equal?
         (cursor-dstx (p (p (p (p last))))) (new-atom "x"))
        (check-equal?
         (cursor-dstx (p (p (p (p (p last)))))) (new-atom "sq"))
        (check-equal?
         (cursor-dstx (p (p (p (p (p (p last)))))))
         (new-fusion "("
                      `(,(new-atom "sq")
                        ,(new-atom "x"))
                      ")"))
        (check-equal? (cursor-dstx (p (p (p (p (p (p (p last))))))))
                      (new-atom "define"))
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
                      (new-atom "hello"))
        (check-equal? (cursor-dstx
                       (focus-successor
                        (focus-successor c)))
                      (new-fusion "(" (list (new-atom "world")) ")"))
        (check-equal? (cursor-dstx
                       (focus-predecessor
                        (focus-successor
                         (focus-successor c))))
                      (new-atom "hello"))
        (check-equal? (cursor-dstx
                       (focus-successor
                        (focus-successor
                         (focus-predecessor
                          (focus-successor
                           (focus-successor c))))))
                      (new-atom "world"))))
     
     (test-case
      "property removal"
      (let* ([a-dstx (new-atom "something")]
             [a-dstx (dstx-property-set a-dstx 'local-id 42)]
             [a-dstx (dstx-property-set a-dstx 'woot-id 17)]
             [a-dstx (dstx-property-remove a-dstx 'local-id)])
        (check-equal? (dstx-property-names a-dstx)
                      (list 'woot-id))))
     
     
     (test-case
      "property removal 2"
      (let* ([a-dstx (new-atom "something")]
             [a-dstx (dstx-property-set a-dstx 'local-id 42)]
             [a-dstx (dstx-property-set a-dstx 'woot-id 17)]
             [a-dstx (dstx-deepmap (lambda (a-dstx)
                                     (dstx-property-remove a-dstx 'local-id))
                                   a-dstx)])
        (check-equal? (dstx-property-names a-dstx)
                      (list 'woot-id))))))
  
  
  
  
  (define (test)
    (test/text-ui test-dsyntax)))