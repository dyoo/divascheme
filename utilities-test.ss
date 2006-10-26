(module utilities-test mzscheme
  (require "test-harness.ss"
           (lib "etc.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           "structures.ss"
           "utilities.ss")

  (define (tests)
    ; test/stx-list : (syntax list) expected -> void
    (define (test/stx-list stx-list expected)
      (test (map (lambda (stx) (list (syntax-column stx) (syntax-span stx))) stx-list) expected))
    ; string->syntax : string -> syntax
    ; test/str->stx string expected -> void
    (define (test/str->stx str expected)
      (let ([result
             (with-handlers ([(lambda args true) (lambda (exn) exn)])
               (let ([stx (string->syntax str)])
                 (list (syntax-column stx) (syntax-span stx))))])
        (test result expected)))
    ; stx->lst : syntax -> (syntax list)
    ; test/stx->lst : symbol expected -> void
    (define (test/stx->lst symbol expected)
      (test true ((list-equal? equal-syntax?) (stx->lst (syntax<-symbol symbol)) (map syntax<-symbol expected))))
    ; get-clipboard-content : void -> (union string false)
    ; test/get-clipboard-content : string -> void
    (define (test/get-clipboard-content text)
      (send the-clipboard set-clipboard-string text 0)
      (test (get-clipboard-content) text))
    ; set-clipbord-content : (union string false) -> void
    ; test/set-clipboard-content : string -> void
    (define (test/set-clipboard-content text/false)
      (set-clipboard-content text/false)
      (test (send the-clipboard get-clipboard-string 0) (or text/false
                                                            (send the-clipboard get-clipboard-string 0))))
    
    ; test/indent : string index expected -> void
    #;(define (test/indent text index . expected)
      (let-values ([(text index diff) (indent-text/index text index)])
        (test (list text index diff) expected)))
    
    ; test/new-selection : (index non-negative-integer index non-negative-integer -> (index non-negative-integer values)) index non-negative-integer index non-negative-integer expected -> void
    (define (test/new-selection fun current-index current-length operator-index operator-length . expected)
      (let-values ([(index length) (fun current-index current-length operator-index operator-length)])
        (test (list index length) expected)))
    
    ; compute-new-selection/replace : index non-negative-integer index non-negative-integer non-negative-integer -> (index non-negative-integer values)
    ; test/new-selection/replace : index non-negative-integer index non-negative-integer non-negative-integer expected -> void
    (define (test/new-selection/replace current-index current-length replace-index deletion-length insertion-length  . expected)
      (let-values ([(index length) (compute-new-selection/replace current-index current-length replace-index deletion-length insertion-length)])
        (test (list index length) expected)))
    
    ; list-ref/safe : ('a list) int -> 'a
    ; test/list-ref/safe : list non-negative-integer expected -> void
    (define (test/list-ref/safe lst n expected)
      (let ([result (with-handlers ([voice-exn? (lambda args "Out of bounds.")])
                      (list-ref/safe lst n))])
        (test result expected)))
    
    (print-tests true)
    
    ; make-voice-exn : string -> voice-exn
    (test (make-voice-exn "Alacazam!") (list 'voice-exn "Alacazam!"))
    (test (make-voice-exn "Banzai") (list 'voice-exn "Banzai"))
    (test (make-voice-exn "Hi! How are you doing today?") (list 'voice-exn "Hi! How are you doing today?"))
    (test (make-voice-exn "set! powa ;-)") (list 'voice-exn "set! powa ;-)"))
    (test (make-voice-exn "open define foo x y z close open close close") (list 'voice-exn "open define foo x y z close open close close"))
    (test (make-voice-exn "(define (foo x y z) ())") (list 'voice-exn "(define (foo x y z) ())"))

    ; voice-exn? : any -> boolean
    (test (voice-exn? (make-voice-exn "stub")) true)
    (test (voice-exn? 'something) false)
    (test (voice-exn? (list)) false)
    (test (voice-exn? (list 'voice-exn)) false)
    (test (voice-exn? (list 'voice-exn 'not-text)) false)
    (test (voice-exn? (list 'voice-exn "text1" "text2")) false)

    ; voice-exn-message : voice-exn -> string
    (test (voice-exn-message (make-voice-exn "Buffy")) "Buffy")
    (test (voice-exn-message (make-voice-exn "Error: There are only 0 matches.")) "Error: There are only 0 matches.")

    ; insert-text : string index string -> string
    (test (insert-text "string" 0 "start---- ") "start---- string")
    (test (insert-text "(define (bar a b c) (* a b c))" 20 "(bananasplit quote)") "(define (bar a b c) (bananasplit quote)(* a b c))")
    (test (insert-text "wheel" 5 " of time") "wheel of time")
    (test (insert-text "" 0 "TEXT") "TEXT")
    (test (insert-text "TEXT" 3 "") "TEXT")

    ; delete-text : string index non-negative-integer -> string
    (test (delete-text "MetaSuperHyper" 4 5) "MetaHyper")
    (test (delete-text "" 0 0) "")
    (test (delete-text "SomeThing" 0 4) "Thing")
    (test (delete-text "SomeThing" 4 5) "Some")
    (test (delete-text "object" 0 6) "")

    ; replace-text : string index string non-negative-integer -> string
    (test (replace-text "dynamic linking is great" 19 "so so bad!!!!" 5) "dynamic linking is so so bad!!!!")
    (test (replace-text "cowboy" 3 "girl" 3) "cowgirl")
    (test (replace-text "It's raining men! Alleluia!" 13 "wo" 0) "It's raining women! Alleluia!")
    (test (replace-text "GNU is not Not Unix" 0 "The cows are very friendly." 19) "The cows are very friendly.")
    (test (replace-text "" 0 "fanfreluche" 0) "fanfreluche")
    (test (replace-text "The shoes of the shoe maker." 3 " dumped" 0) "The dumped shoes of the shoe maker.")
    
    ; get-subtext/pos+len : string pos non-negative-integer -> string
    (test (get-subtext/pos+len "" (index->syntax-pos 0) 0) "")
    (test (get-subtext/pos+len "(define (foo bar) (bar foo))" (index->syntax-pos 0) 28) "(define (foo bar) (bar foo))")
    (test (get-subtext/pos+len "(define (foo bar) (bar foo))" (index->syntax-pos 1) 6) "define")
    (test (get-subtext/pos+len "(define (foo bar) (bar foo))" (index->syntax-pos 8) 9) "(foo bar)")
    (test (get-subtext/pos+len "(define (foo bar) (bar foo))" (index->syntax-pos 4) 7) "ine (fo")
    (test (get-subtext/pos+len "(define (foo bar) (bar foo))" (index->syntax-pos 11) -7) "ine (fo")
    (test (get-subtext/pos+len "(define (foo bar) (bar foo))" (index->syntax-pos 10) 0) "")
    
    ; syntax-pos->index : pos -> index
    (test (syntax-pos->index -32) -33)
    (test (syntax-pos->index 0) -1)
    (test (syntax-pos->index 1) 0)
    (test (syntax-pos->index 32) 31)
    
    ; index->syntax-pos : index -> pos
    (test (index->syntax-pos -32) -31)
    (test (index->syntax-pos 0) 1)
    (test (index->syntax-pos 1) 2)
    (test (index->syntax-pos 32) 33)
    
    ; id : 'a -> 'a
    (test (id 32) 32)
    (test (id true) true)
    (test (id id) id)
    (test (id 'BANG) 'BANG)
    (test (id (list id id id)) (list id id id))
    
    ; string->syntax-list : string -> (syntax list)
    (test/stx-list (string->syntax-list "") ())
    (test/stx-list (string->syntax-list "()") (list (list 0 2)))
    (test/stx-list (string->syntax-list "    (        )") (list (list 4 10)))
    (test/stx-list (string->syntax-list "()()()()()()()") (list (list 0 2) (list 2 2) (list 4 2) (list 6 2) (list 8 2) (list 10 2) (list 12 2)))
    (test/stx-list (string->syntax-list "(define (foo x y z) (+ x y z)) (define (bar a b c) (* a b c))") (list (list 0 30) (list 31 30)))
    (test/stx-list (string->syntax-list "(1 2 3 4 . 5)") (list (list 0 13)))
    (test/stx-list (string->syntax-list "(1 2 . 3 . 4 5 6)") (list (list 0 17)))
    (test/stx-list (string->syntax-list "(1 2 #rx\"^,\" 4 5 6)") (list (list 0 19)))
    
    ; test/str->stx string expected -> void
    (test/str->stx "" '(voice-exn "string->syntax: empty text"))
    (test/str->stx "     (      )     " (list 5 8))
    (test/str->stx "alpha beta gamma zeta delta omega" (list 0 5))
    
    ; shape-paren : (union false 'Round 'Square 'Curly) string -> string
    (test (shape-paren false "") "")
    (test (shape-paren false "  a    d d d d") "  a    d d d d")
    (test (shape-paren false "()") "()")
    (test (shape-paren false "{}") "{}")
    (test (shape-paren false "[]") "[]")
    (test (shape-paren 'Square "[]") "[]")
    (test (shape-paren 'Square "{}") "[]")
    (test (shape-paren 'Square "()") "[]")
    (test (shape-paren 'Square "( a b c d e ( er t f ) °}}}") "[ a b c d e ( er t f ) °}}]")
    (test (shape-paren 'Square "(}") "[]")
    (test (shape-paren 'Square "     ") "[   ]")
    (test (shape-paren 'Curly "ab") "{}")
    (test (shape-paren 'Curly "[ h ]") "{ h }")
    (test (shape-paren 'Curly "{ bou bou}") "{ bou bou}")
    (test (shape-paren 'Curly "(arg arg)") "{arg arg}")
    (test (shape-paren 'Round "  ") "()")
    (test (shape-paren 'Round "[define (foo bar) (bar foo)]") "(define (foo bar) (bar foo))")
    (test (shape-paren 'Round "{f}") "(f)")
    (test (shape-paren 'Round "(   )") "(   )")
    
    ; list-equal? : ('a 'a -> boolean) -> ('a list) ('a list) -> boolean
    (test ((list-equal? =) (list 1 2 3) (list 1 2 3)) true)
    (test ((list-equal? =) (list) (list)) true)
    (test ((list-equal? =) (list 1 2 3) (list 1 2 3 4)) false)
    (test ((list-equal? =) (list 1 2 3) (list 1 3 2)) false)
    (test ((list-equal? equal?) (list "e" "v" "w") (list "e" "v" "w")) true)
    (test ((list-equal? eq?) (list 'e 'r 't) (list 'e 'r 't)) true)
    (test ((list-equal? equal?) (list (list (list))) (list (list (list)))) true)
    (test ((list-equal? eq?) (list (list (list))) (list (list (list)))) false)
    
    ; syntax<-symbol : symbol -> syntax
    (test (syntax-object->datum (syntax<-symbol '||)) '||)
    (test (syntax-object->datum (syntax<-symbol '(a b c d))) '(a b c d))
    (test (syntax-object->datum (syntax<-symbol '|$ $ $ $|)) '|$ $ $ $|)
    
    ; equal-syntax? : syntax syntax -> boolean
    (test (equal-syntax? #'e #'e) true)
    (test (equal-syntax? #'f #'e) false)
    (test (equal-syntax? #'(   a b    c ( k ji )) #'(a b c (k ji))) true)
    (test (equal-syntax? #'[] #'{}) true)
    (test (equal-syntax? #'0.0 #'0) false)
    
    ; andgmap : (syntax -> 'a) syntax -> 'a
    (test (andgmap id #'()) true)
    (test (equal-syntax? (andgmap id #'(a)) #'a) true)
    (test (equal-syntax? (andgmap id #'(a b c d e f)) #'f) true)
    
    ; orgmap : (syntax -> 'a) syntax -> 'a
    (test (orgmap id #'()) false)
    (test (equal-syntax? (orgmap id #'(a)) #'a) true)
    (test (equal-syntax? (orgmap id #'(a b c d e f)) #'a) true)
    
    ; gmap : (syntax -> 'a) syntax -> ('a list)
    (test (gmap syntax-object->datum #'()) ())
    (test (gmap syntax-object->datum #'(a b c)) '(a b c))
    
    ; stx->lst : syntax -> (syntax list)
    (test/stx->lst 'foo ())
    (test/stx->lst '()  ())
    (test/stx->lst '(foo bar) (list 'foo 'bar))
    (test/stx->lst '(foo (( bar x y) z)) (list 'foo '((bar x y)z)))
    
    ; atomic? : any -> boolean
    (test (atomic? (list)) false)
    (test (atomic? 'symbol) true)
    (test (atomic? 1.23+4i) true)
    (test (atomic? "string") true)
    (test (atomic? #(1 2 3)) false)
    (test (atomic? (make-After)) true)
    (test (atomic? true) true)
    (test (atomic? false) true)
    (test (atomic? #\newline) true)
    (test (atomic? #\a) true)
    (test (atomic? #rx"^,") true)

    ; or* : ('a list) -> 'a
    (test (or* (list 1 2 3 4 5)) 1)
    (test (or* (list false false false (list false) 4)) (list false))
    
    ; test/get-clipboard-content : string -> void
    (test/get-clipboard-content "WAZA")
    (test/get-clipboard-content "")
    (test/get-clipboard-content "(define (foo bar) (bar foo))")
    
    ; test/set-clipboard-content : string -> void
    (test/set-clipboard-content false)
    (test/set-clipboard-content "What's up?")
    (test/set-clipboard-content "")
    (test/set-clipboard-content "(define (foo bar) (bar foo))")
    
    ; identifier-match? : symbol symbol -> boolean
    (test true  (identifier-match? 'x 'x))
    (test true  (identifier-match? 'foo 'foobar))
    (test true  (identifier-match? 'foo 'foo-bar))
    (test true  (identifier-match? 'define 'define/bind))
    (test true  (identifier-match? 'string 'string?))
    (test true  (identifier-match? 'set 'set!))
    (test true  (identifier-match? 'class 'class*))
    (test true  (identifier-match? 'object 'object%))
    (test true  (identifier-match? 'unit 'unit@))
    (test true  (identifier-match? 'fw 'fw:thing))
    (test true  (identifier-match? 'signature 'signature^))
    (test true  (identifier-match? 'interface 'interface<%>))
    (test true  (identifier-match? 'top '#%top))
    (test true  (identifier-match? 'app '#%app))
    
    ; syntax-is-symbol? : symbol -> syntax -> boolean
    (test true  ((syntax-is-symbol? 'foobar)  (syntax foobar)))
    (test false ((syntax-is-symbol? 'foobar)  (syntax foo)))
    (test true  ((syntax-is-symbol? 'foo)     (syntax foo-bar)))
    (test false ((syntax-is-symbol? 'foo-bar) (syntax foo)))
    (test true  ((syntax-is-symbol? '|4|)     (syntax 4)))
    (test false ((syntax-is-symbol? '|5|)     (syntax 4)))
    (test true  ((syntax-is-symbol? '|"Bonjour !"|) (syntax "Bonjour !")))
    (test true  ((syntax-is-symbol? '|#f|)       (syntax #f)))
    (test true  ((syntax-is-symbol? '|#t|)       (syntax #t)))
    (test false ((syntax-is-symbol? '|#t|)       (syntax #f)))
    (test false ((syntax-is-symbol? '|"Different."|) (syntax "not equal")))
    (test true  ((syntax-is-symbol? '|#\z|) (syntax #\z)))
    (test true  ((syntax-is-symbol? 'symbol)        (syntax symbol)))
    (test false ((syntax-is-symbol? 'symbol-symbol) (syntax symbol)))
    (test false ((syntax-is-symbol? '|(symbol)|)    (syntax symbol)))
    (test false ((syntax-is-symbol? '|(symbol)|)    (syntax (symbol))))
    (test false ((syntax-is-symbol? '|(symbol)|)    (syntax |symbol|)))
    (test true  ((syntax-is-symbol? '|(symbol)|)    (syntax |(symbol)|)))
    (test false ((syntax-is-symbol? 'symbol)        (syntax (symbol))))
    
    
    ; syntax-begins-with? : symbol -> syntax -> boolean
    (test true  ((syntax-begins-with? 'foo) (syntax (foo))))
    (test false ((syntax-begins-with? 'foo) (syntax foo)))
    (test false ((syntax-begins-with? 'foo) (syntax ((foo)))))
    (test true  ((syntax-begins-with? 'foo) (syntax (foo2))))
    (test true  ((syntax-begins-with? 'foo) (syntax (foobar))))
    (test true  ((syntax-begins-with? 'foo) (syntax (foo-bar))))
    (test true  ((syntax-begins-with? '|1|) (syntax [ 1 2 3 4 5 6 7 ])))
    (test false ((syntax-begins-with? '|2|) (syntax [ 1 2 3 4 5 6 7 ])))
    (test false ((syntax-begins-with? '|3|) (syntax [ 1 2 3 4 5 6 7 ])))
    (test false ((syntax-begins-with? '|4|) (syntax [ 1 2 3 4 5 6 7 ])))
    (test false ((syntax-begins-with? '|5|) (syntax [ 1 2 3 4 5 6 7 ])))
    (test false ((syntax-begins-with? '|6|) (syntax [ 1 2 3 4 5 6 7 ])))
    (test false ((syntax-begins-with? '|7|) (syntax [ 1 2 3 4 5 6 7 ])))
    (test false ((syntax-begins-with? '|8|) (syntax [ 1 2 3 4 5 6 7 ])))
    (test true  ((syntax-begins-with? '|#f|)  (syntax (#f foo bar))))
    (test true  ((syntax-begins-with? '|#\a|) (syntax (#\a #\A #\à #\À #\â #\Â #\ä #\Ä #\ã #\Ã #\å #\Å))))
    (test true  ((syntax-begins-with? 'foo)   (syntax #(foo bar))))
    (test true  ((syntax-begins-with? 'fact)  (syntax (fact 10))))
    (test false ((syntax-begins-with? 'bar)   (syntax (foo bar))))
    (test false ((syntax-begins-with? 'foo)   (syntax foo)))
    (test false ((syntax-begins-with? '|(foo)|) (syntax ((foo) bar))))
    (test true  ((syntax-begins-with? '|(foo)|) (syntax (|(foo)| bar))))
    (test false ((syntax-begins-with? '|(foo)|) (syntax ('(foo) bar))))
    (test false ((syntax-begins-with? 'quote) (syntax ('(foo) bar))))
    (test true  ((syntax-begins-with? 'quote) (syntax '(foo))))

    ; tokenize-identifier : string -> (string list)
    (test (tokenize-identifier "AZERTY")  (list "AZERTY"))
    (test (tokenize-identifier "$%£AZERTY£****!&#@?/ezezez+++===ezezeez") (list "AZERTY" "ezezez" "ezezeez"))
    (test (tokenize-identifier "#%top") (list "top"))
    (test (tokenize-identifier "define-struct") (list "define" "struct"))
    (test (tokenize-identifier "") (list))
    (test (tokenize-identifier "éééàààçççç") (list "éééàààçççç"))

    ; compute-new-start-index/insert : index index non-negative-integer -> index
    (test (compute-new-start-index/insert 2343 90 79843) 82186)
    (test (compute-new-start-index/insert 32 321 43092) 32)
    (test (compute-new-start-index/insert 456 456 0) 456)
    (test (compute-new-start-index/insert 324 324 123) 447)
    
    ; compute-new-end-index/insert : index index non-negative-integer -> index
    (test (compute-new-end-index/insert 2343 90 79843) 82186)
    (test (compute-new-end-index/insert 32 321 43092) 32)
    (test (compute-new-end-index/insert 456 456 0) 456)
    (test (compute-new-end-index/insert 324 324 123) 324)
                 
    ; test/new-selection : (index non-negative-integer index non-negative-integer -> (index non-negative-integer values)) index non-negative-integer index non-negative-integer expected -> void
    ; compute-new-selection/insert : index non-negative-integer index non-negative-integer -> (index non-negative-integer values)
    (test/new-selection compute-new-selection/insert 3214 789 43219 32312 3214 789)
    (test/new-selection compute-new-selection/insert 98437 983 654 8923 107360 983)
    (test/new-selection compute-new-selection/insert 3234 543 3777 32875 3234 543)
    (test/new-selection compute-new-selection/insert 40 10 40 10 50 10)
    (test/new-selection compute-new-selection/insert 50 0 50 0 50 0)
    (test/new-selection compute-new-selection/insert 50 0 50 10 60 0)

    ; compute-new-index/delete : index index non-negative-integer -> index
    (test (compute-new-index/delete 213 400 3209) 213)
    (test (compute-new-index/delete 342 200 42) 300)
    (test (compute-new-index/delete 100 90 20) 90)
    
    ; test/new-selection : (index non-negative-integer index non-negative-integer -> (index non-negative-integer values)) index non-negative-integer index non-negative-integer expected -> void
    ; compute-new-selection/delete : index non-negative-integer index non-negative-integer -> (index non-negative-integer values)    (test
    (test/new-selection compute-new-selection/delete 100 100 300 300 100 100)
    (test/new-selection compute-new-selection/delete 3000 30 20 34 2966 30)
    (test/new-selection compute-new-selection/delete 100 100 100 300 100 0)
    (test/new-selection compute-new-selection/delete 100 300 100 200 100 100)
    
    ; test/new-selection/replace : index non-negative-integer index non-negative-integer non-negative-integer expected -> void
    (test/new-selection/replace 100 100 200 300 400 100 100)
    (test/new-selection/replace 100 100 20 30 40 110 100)
    (test/new-selection/replace 100 100 100 100 200 300 0)
    (test/new-selection/replace 100 100 100 40 60 160 60)
    
    ; line-index : string index -> index
    (test (line-index "" 0) 0)
    (test (line-index "\n" 1) 1)
    (test (line-index " " 1) 0)
    (test (line-index "(define (foo bar)\n  (foo bar))" 22) 18)
    
    ; line-end-index : string index -> index
    (test (line-end-index "" 0) 0)
    (test (line-end-index " " 0) 1)
    (test (line-end-index " \n " 0) 1)
    (test (line-end-index " \n " 2) 3)
    (test (line-end-index "(define (foo bar)\n  (foo bar))" 8) 17)
    (test (line-end-index "(define (foo bar)\n  (foo bar))" 28) 30)
    
    ; line-text : string index -> string
    (test (line-text/index "" 0) "")
    (test (line-text/index "Hi! How are you doing?" 5) "Hi! How are you doing?")
    (test (line-text/index "\n" 0) "")
    (test (line-text/index "\n" 1) "")
    (test (line-text/index "a\nb" 1) "a")
    (test (line-text/index "a\nb" 2) "b")
    (test (line-text/index "(define (foo bar)\n  (foo bar))" 8) "(define (foo bar)")
    (test (line-text/index "(define (foo bar)\n  (foo bar))" 28) "  (foo bar))")
;    (test (line-text/index "(define (foo bar)\n  (foo bar))" 17 2) "(define (foo bar)\n  (foo bar))")
    
;    ; test/indent : string index expected -> void
;    (test/indent "" 0 "" 0 0)
;    (test/indent "\n" 0 "\n" 0 0)
;    (test/indent "\n" 1 "\n" 1 0)
;    (test/indent "(\n)" 2 "(\n )" 2 1)
;    (test/indent "(define foo\n)" 12 "(define foo\n  )" 12 2)
;    (test/indent "(define fact\n(lambda (n)\n()))" 25 "(define fact\n(lambda (n)\n  ()))" 25 2)
;    (test/indent "(define fact\n  (lambda (n)\n()))" 27 "(define fact\n  (lambda (n)\n    ()))" 27 4)
;    (test/indent "(define fact\n  (lambda (n)\n()))" 13 "(define fact\n  (lambda (n)\n()))" 13 0)
;    (test/indent "(define fact\n (lambda (n)\n()))" 13 "(define fact\n  (lambda (n)\n()))" 13 1)
;    (test/indent "(define fact\n   (lambda (n)\n()))" 13 "(define fact\n  (lambda (n)\n()))" 13 -1)
    
    ; filter-double : ('a list) -> ('a list)
    (test (filter-double (list)) (list))
    (test (filter-double (list 'a)) (list 'a))
    (test (filter-double (list 1 2 3 3 4 4 4 6)) (list 1 2 3 4 6))
    (test (filter-double (list 0 1 2 3 1 4)) (list 0 1 2 3 4))
    (test (filter-double (list 1 0 2 1)) (list 1 0 2))
    (test (filter-double (list 1 2 3 1)) (list 1 2 3))
    (test (filter-double (list 1 2 3 1 4)) (list 1 2 3 4))
    (test (filter-double (list "e" "e")) (list "e"))
    (test (filter-double (list "" "r" "r" "")) (list "" "r"))
    (test (filter-double (list 'symbol 'define 'define-struct 'fact 'foo 'bar 'fact)) (list 'symbol 'define 'define-struct 'fact 'foo 'bar))
    
    ; test/list-ref/safe : list non-negative-integer expected -> void
    (test/list-ref/safe (list 0 1 2 3 4 5 6) 0 0)
    (test/list-ref/safe (list '0 '1 '2 '3 '4 '5 '6) 1 '1)
    (test/list-ref/safe (list 0 1 2 3 4) -1 "Out of bounds.")
    (test/list-ref/safe (list) 0 "Out of bounds.")
    (test/list-ref/safe (list 0 1 2 3 4 5) 5 5)
    (test/list-ref/safe (list 0 1 2 3 4 5) 6 "Out of bounds.")

    ; prefix/string? : string -> string -> boolean
    (test true  ((prefix/string? "") ""))
    (test true  ((prefix/string? "") "ANYTHING"))
    (test false ((prefix/string? "ANYTHING") ""))
    (test true  ((prefix/string? "define") "define-struct"))
    (test false ((prefix/string? "struct") "define-struct"))
    (test false ((prefix/string? "top") "#%top"))
    
    ; list-gcd : (('a list) list) -> ('a list)
    (test (list-gcd (list)) (list))
    (test (list-gcd (list (list 1 2 3 4 5 6 7 8) (list 1 2 3 4 5 6 8 9) (list 1 2 3 4) (list 1 2 3 4 5))) (list 1 2 3 4))
    (test (list-gcd (list (list) (list #\n #\h))) (list))
    (test (list-gcd (map string->list (list "define-struct" "define-values" "define" "define-syntax"))) (string->list "define"))
    (test (list-gcd (map string->list (list "define-struct" "define-values" "define-syntax"))) (string->list "define-"))

    )

  (tests))