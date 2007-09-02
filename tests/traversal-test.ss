(module traversal-test mzscheme
  (require (lib "plt-match.ss")
           (lib "etc.ss")
           (lib "list.ss")
           "utilities.ss"
           "traversal.ss"
           "test-harness.ss")
  
  
  (define (tests)
    ; test/find : (pos (syntax list) -> (union syntax false)) -> index string expected -> void
    (define ((test/find find) index text expected)
      (let ([stx/false (find (index->syntax-pos index) (string->syntax-list text))])
        (test  (and stx/false (get-subtext/stx text stx/false)) expected)))
    
    ; find-pos : pos (syntax list) -> (union syntax false)
    ; test/find-pos : index string expected -> void
    (define test/find-pos
      (test/find find-pos))
    ; find-pos-parent : pos (syntax list) -> (union syntax false)
    ; test/find-pos-parent : index string expected -> void
    (define test/find-pos-parent
      (test/find find-pos-parent))

        (define test/find-pos-sibling-backward
      (test/find find-pos-sibling-backward))
    

    (define test/find-pos-sibling-forward
      (test/find find-pos-sibling-forward))
    
    ; make-test/find-all : ((syntax -> boolean) (syntax list) -> (syntax list)) -> (syntax -> boolean) string expected -> void
    (define ((make-test/find-all find-all) pred text expected)
      (let ([stx-list (find-all pred (string->syntax-list text))]
            [fun (lambda (stx) (get-subtext/stx text stx))])
        (test (map fun stx-list) expected)))
    ; find-all : (syntax -> boolean) (syntax list) -> (syntax list)
    ; test/find-all : (syntax -> boolean) string expected -> void
    (define test/find-all (make-test/find-all find-all))
    ; find-all/metric : (syntax -> boolean) (syntax -> non-negative-integer) (syntax list) -> (syntax list)
    ; test/find-all/metric : (syntax -> non-negative-integer) -> index string expected -> void
    (define (test/find-all/metric metric) 
      (make-test/find-all (lambda (pred stx-list) (find-all/metric   pred metric stx-list))))
    ; find-all-forward : (syntax -> boolean) pos (syntax list) -> (syntax list)
    ; test/find-all-forward : pos -> index string expected -> void
    (define (test/find-all-forward metric-base) 
      (make-test/find-all (lambda (pred stx-list) (find-all-forward  pred (index->syntax-pos metric-base) stx-list))))
    ; find-all-backward : (syntax -> boolean) pos (syntax list) -> (syntax list)
    ; test/find-all-backward : pos -> index string expected -> void
    (define (test/find-all-backward metric-base) 
      (make-test/find-all (lambda (pred stx-list) (find-all-backward pred (index->syntax-pos metric-base) stx-list))))
    ; find-all-nearest : (syntax -> boolean) pos (syntax list) -> (syntax list)
    ; test/find-all-nearest : pos -> index string expected -> void
    (define (test/find-all-nearest metric-base) 
      (make-test/find-all (lambda (pred stx-list) (find-all-nearest  pred (index->syntax-pos metric-base) stx-list))))
      
    ; in-syntax? : pos syntax -> boolean
    ; pred : index -> syntax -> boolean
    (define ((pred index) stx)
      (in-syntax? (index->syntax-pos index) stx))
    ; pred/stx? : (any -> boolean) -> syntax -> boolean
    (define ((pred/stx? pred) stx)
      (pred (syntax-e stx)))
    ; number/stx? : syntax -> boolean
    (define number/stx? (pred/stx? number?))
    ; symbol/stx? : syntax -> boolean
    (define symbol/stx? (pred/stx? symbol?))
    ; string/stx? : syntax -> boolean
    (define string/stx? (pred/stx? string?))
    ; list/stx? : syntax -> boolean
    (define list/stx?   (pred/stx? list?))
    ; vector/stx? : syntax -> boolean
    (define vector/stx? (pred/stx? vector?))
    ; pair/stx? : syntax -> boolean
    (define pair/stx?   (pred/stx? pair?))
    ; boolean/stx? : syntax -> boolean
    (define boolean/stx? (pred/stx? boolean?))
    ; char/stx? : syntax -> boolean
    (define char/stx?   (pred/stx? char?))
    
    ; stxy->text-col-symbol : string -> (syntax (2 list))
    (define ((stxy->text-col-symbol text) stxy)
      (list (get-subtext/stx text (first stxy)) (syntax-column (second stxy)) (syntax-e (second stxy))))
    
    ; find-ellipsis : pos (syntax list) -> syntax
    ; test/ellipsis : index string expected -> void
    (define (test/ellipsis index text expected)
      (let ([result
             (with-handlers ([voice-exn? (lambda args "NO----")])
               (get-subtext/stx text (find-ellipsis (index->syntax-pos index) (string->syntax-list text))))])
        (test  result expected)))
    
    ; find-siblings-ellipsis : pos (syntax list) -> (syntax (2 list))
    ; test/-siblings-ellipsis : index string expected -> void
    (define (test/-siblings-ellipsis index text expected)
      (let ([result
             (with-handlers ([voice-exn? (lambda args (list "NO----" -1 '||))])
               (let ([stxy (find-siblings-ellipsis (index->syntax-pos index) (string->syntax-list text))])
                 ((stxy->text-col-symbol text) stxy)))])
        (test  result expected)))
    
    ; find-all-ellipsis : pos (syntax list) -> (syntax list)
    ; test/-all-ellipsis : index string expected -> void
    (define (test/-all-ellipsis index text expected)
      (let ([result
             (let ([stxy-list (find-all-ellipsis (index->syntax-pos index) (string->syntax-list text))])
               (map (stxy->text-col-symbol text) stxy-list))])
        (test  result expected)))
    
    ; test/ward : (pos (syntax list) -> (union syntax false)) -> index string expected -> void
    (define ((test/ward fun) index text expected)
      (let ([stx/false
             (with-handlers ([voice-exn? (lambda args false)])
               (fun (index->syntax-pos index) (string->syntax-list text)))])
          (test (and stx/false (get-subtext/stx text stx/false)) expected)))
    
    ; find-pos-forward : pos (syntax list) -> (union syntax false)
    ; test/forward : index string expected -> void
    (define test/forward  (test/ward find-pos-sibling-forward))
    ; find-pos-backward : pos (syntax list) -> (union syntax false)
    ; test/backward : index string expected -> void
    (define test/backward (test/ward find-pos-sibling-backward))
    
    ; find-placeholder : boolean pos (syntax list) -> syntax
    ; test/placeholder : boolean -> index string expected -> void
    (define ((test/placeholder backward?) index text expected)
      (let ([result
             (with-handlers ([voice-exn? id])
               (let* ([pos      (index->syntax-pos index)]
                      [stx-list (string->syntax-list text)]
                      [stx      (find-placeholder backward? pos stx-list)])
                 (get-subtext/stx text stx)))])
        (test result expected)))
    
    ; test/placeholder-forward : index string expected -> void
    (define test/placeholder-forward (test/placeholder false))
    
    ; test/placeholder-backward : index string expected -> void
    (define test/placeholder-backward (test/placeholder true))
            
    ; find-pos-mark-forward : pos (syntax list) -> (union syntax false)
    ; test/find : (pos (syntax list) -> (union syntax false)) -> index string expected -> void
    ; test/pos-mark-forward : index string expected -> void
    (define test/pos-mark-forward
      (test/find find-pos-mark-forward))
    
    ; find-all-pos-mark-forward : pos (syntax list) -> (syntax list)
    ; test/pos-all-mark-forward : index string expected -> void
    (define (test/pos-all-mark-forward index text expected)
      (let ([stx-list (find-all-pos-mark-forward (index->syntax-pos index) (string->syntax-list text))]
            [fun (lambda (stx) (get-subtext/stx text stx))])
        (test (map fun stx-list) expected)))
    
    ; find-pos-fill-forward : pos (syntax list) -> (union syntax false)
    ; test/pos-fill-forward : index string expected -> void
    (define test/pos-fill-forward
      (test/find find-pos-fill-forward))
    
    ; find-distance/metric : (syntax -> boolean) (syntax -> non-negative-integer) (syntax list) integer -> syntax
    ; test/distance/metric : (syntax -> boolean) (syntax -> non-negative-integer) string integer expected -> void
    (define (test/distance/metric pred metric text distance expected)
      (let ([result (with-handlers ([voice-exn? (lambda args "NOTHING-FOUND")])
                      (get-subtext/stx text (find-distance/metric pred metric (string->syntax-list text) distance)))])
        (test result expected)))
    
    ; find-end : pos (syntax list) -> (union syntax false)
    ; test/end : index string expected -> void
    ;; obsolete
    #;(define (test/end index text expected)
      (let ([result (with-handlers ([voice-exn? (lambda args "NOT FOUND")])
                      (get-subtext/stx text (find-end (index->pos index) (string->syntax-list text))))])
        (test result expected)))
    
    ; find-rank : (syntax -> boolean) pos (syntax -> non-negative-integer) (syntax list) -> non-negative-integer
    ; test/rank : (syntax -> boolean) index (syntax -> non-negative-integer) string expected -> void
    (define (test/rank pred index metric text expected)
      (let ([result (with-handlers ([voice-exn? (lambda (exn) "no rank")])
                      (find-rank pred (index->pos index) metric (string->syntax-list text)))])
        (test result expected)))
#|
    (define (test/down column pos text expected)
      ((test/find (lambda (pos lst) (find-pos-down column pos lst))) pos text expected))
                                                                   
    (print-tests 'bad)
    
    (test/down 9 9
"(define xxx
  (foo xxxx)
  (add1 x))" "xxxx")
    
    (test/down 4 16
"(define xxx
  (foo xxxx)
  (add1 x))" "add1")

    (test/down 9 9
"(define xxx
  (foo            xxx)
  (add1 x))" "foo")

    (test/down 0 0 
"(define x
add1)" "add1")
    
    (test/down 9 9
"(define xxx (stuff


boing)        (foo a b c))" "boing")
    
    (test/down 9 9
"(define xxx (stuff)


              (foo a b c))" "(foo a b c)")
    |#
    
    ; test/find-pos : index string expected -> void
    (test/find-pos 0 "" false)
    (test/find-pos 2  "(  )" "(  )")
    (test/find-pos 2  "((  ))" "(  )")
    (test/find-pos 3  "(a  b)" "(a  b)")
    (test/find-pos 0 "(define (foo bar) (bar foo))" "(define (foo bar) (bar foo))") 
    (test/find-pos 1 "(define (foo bar) (bar foo))" "define")
    (test/find-pos 7 "(define (foo bar) (bar foo))" "(define (foo bar) (bar foo))")
    (test/find-pos 8 "(define (foo bar) (bar foo))" "(foo bar)")
    (test/find-pos 10 "(define (foo bar) (bar foo))" "foo")
    (test/find-pos 0 " foobar " false)
    (test/find-pos 1 " foobar " "foobar")

    ; test/find-pos-parent : index string expected -> void
    (test/find-pos-parent 0 "" false)
    (test/find-pos-parent 2 "(  )" false)
    (test/find-pos-parent 2 "((  ))" "((  ))")
    (test/find-pos-parent 3 "(a  b)" false)
    (test/find-pos-parent 0 " foobar " false)
    (test/find-pos-parent 1 " foobar " false)
    (test/find-pos-parent 0 "(define (foo bar) (bar foo))" false)
    (test/find-pos-parent 1 "(define (foo bar) (bar foo))" "(define (foo bar) (bar foo))")
    (test/find-pos-parent 7 "(define (foo bar) (bar foo))"  false)
    (test/find-pos-parent 8 "(define (foo bar) (bar foo))" "(define (foo bar) (bar foo))")
    (test/find-pos-parent 10 "(define (foo bar) (bar foo))" "(foo bar)")
    (test/find-pos-sibling-forward 8 "(define (foo bar) (bar foo))" "(bar foo)")  
    (test/find-pos-sibling-forward 18 "(define (foo bar) (bar foo))" false)
    (test/find-pos-sibling-forward 11 "(define (z foo bar a b c) (bar foo))" "bar")
    (test/find-pos-sibling-backward 8 "(define (foo bar) (bar foo))" "define")
    (test/find-pos-sibling-backward 18 "(define (foo bar) (bar foo))" "(foo bar)")
    (test/find-pos-sibling-backward 11 "(define (z foo bar a b c) (bar foo))" "z")
    

    ; test/find-all : (syntax -> boolean) string expected -> void
    (test/find-all (pred 0) "" empty)
    (test/find-all (pred 0) "(define (foo bar) (bar foo))" (list "(define (foo bar) (bar foo))"))
    (test/find-all (pred 1) "(define (foo bar) (bar foo))" (list "(define (foo bar) (bar foo))" "define"))
    (test/find-all (pred 29) " ( a ( b ( c ) d ) e ) ( f ( g ) h ) " (list "( f ( g ) h )" "( g )" "g"))
    (test/find-all list/stx? "(define (foo bar) (bar foo))" (list "(define (foo bar) (bar foo))" "(foo bar)" "(bar foo)"))
    (test/find-all symbol/stx? "(define \"string\" (foo bar) 'alpha  (bar 8 foo))" (list "define" "foo" "bar" "'" "alpha" "bar" "foo"))
    (test/find-all number/stx? "((define (foo x y z) (+ x y z)) 0 -1.4 (+ 1+0i 1/8))" (list "0" "-1.4" "1+0i" "1/8"))
    
    ; syntax-list-last-position : (syntax list) -> pos
    (test (syntax-pos->index (syntax-list-last-position (string->syntax-list "(define (foo bar) (bar foo))"))) 28)
    (test (syntax-pos->index (syntax-list-last-position (string->syntax-list ""))) 0)
    (test (syntax-pos->index (syntax-list-last-position (string->syntax-list "   "))) 0)
    (test (syntax-pos->index (syntax-list-last-position (string->syntax-list "  (    )  "))) 8)
    (test (syntax-pos->index (syntax-list-last-position (string->syntax-list "   (   foo   bar   ) (   bar   foo  )  "))) 37)
    
    
    ; metric-forward : pos pos -> syntax -> non-negative-integer
    (test ((metric-forward (index->syntax-pos 10) (index->syntax-pos 20)) (first  (string->syntax-list "(define (foo bar) (bar foo))"))) 10)
    (test ((metric-forward (index->syntax-pos 10) (index->syntax-pos 20)) (second (string->syntax-list "(foo bar) (bar foo)"))) 0)
    (test ((metric-forward (index->syntax-pos 10) (index->syntax-pos 20)) (third  (string->syntax-list "(foo bar) (bar foo) (fboaor)"))) 10)

    ; metric-backward : pos pos -> syntax -> non-negative-integer
    (test ((metric-backward (index->syntax-pos 5) (index->syntax-pos 16)) (first  (string->syntax-list "(define (foo bar) (bar foo))"))) 5)
    (test ((metric-backward (index->syntax-pos 5) (index->syntax-pos 16)) (second (string->syntax-list "(foo bar) (bar foo)"))) 11)
    (test ((metric-backward (index->syntax-pos 5) (index->syntax-pos 16)) (third  (string->syntax-list "(foo bar) (bar foo) (fboaor)"))) 1)

    ; metric-nearest : pos -> syntax -> non-negative-integer
    (test ((metric-nearest (index->syntax-pos 10)) (first  (string->syntax-list "(define (foo bar) (bar foo))"))) 10)
    (test ((metric-nearest (index->syntax-pos 10)) (second (string->syntax-list "(foo bar) (bar foo)"))) 0)
    (test ((metric-nearest (index->syntax-pos 10)) (third  (string->syntax-list "(foo bar) (bar foo) (meta hyper)"))) 10)
    
    ; find-all/metric : (syntax -> boolean) (syntax -> non-negative-integer) (syntax list) -> (syntax list)
    ((test/find-all/metric syntax-index) number/stx? "" (list))
    ((test/find-all/metric syntax-index) number/stx? " 10 (9 8) ( () 7 ret) " (list "10" "9" "8" "7"))
    ((test/find-all/metric (metric-nearest (index->syntax-pos 22))) number/stx? " 10 (9 8) ( () 7 ret) " (list "7" "8" "9" "10"))
    
    ; test/find-all-forward : pos -> index string expected -> void
    ((test/find-all-forward 0) list/stx? "" (list))
    ((test/find-all-forward 0) list/stx? "  	   " (list))
    ((test/find-all-forward 0) list/stx? "  de 	  edsd #'r |jhjh| \"Ouaf\"  736.34 " (list "#'r"))
    ((test/find-all-forward 0) list/stx? "(define (foo bar) (bar foo))" (list "(define (foo bar) (bar foo))" "(foo bar)" "(bar foo)"))
    ((test/find-all-forward 1) list/stx? "(define (foo bar) (bar foo))" (list "(foo bar)" "(bar foo)" "(define (foo bar) (bar foo))"))
    
    ((test/find-all-forward 5) void "(a b c '(1 2 3) e f g)" 
                               (list "c" "'" "'(1 2 3)" "(1 2 3)" "1" "2" "3"
                                     "e" "f" "g" "(a b c '(1 2 3) e f g)" "a" "b"))

    ; test/backward : index string expected -> void
    ((test/find-all-backward 0) symbol/stx? "" (list))
    ((test/find-all-backward 0) symbol/stx? "  	   " (list))
    ((test/find-all-backward 0) symbol/stx? "  de 	  edsd #'r |jhjh| \"Ouaf\"  736.34 " (list "|jhjh|" "r" "#'" "edsd" "de"))
    ((test/find-all-backward 0) symbol/stx? "(define (foo bar) (bar foo))" (list "foo" "bar" "bar" "foo" "define"))
    ((test/find-all-backward 1) symbol/stx? "(define (foo bar) (bar foo))" (list "define" "foo" "bar" "bar" "foo"))

    ; test/find-all-nearest : pos -> index string expected -> void
    ((test/find-all-nearest 0) pair/stx? "" (list))
    ((test/find-all-nearest 0) pair/stx? "  	   " (list))
    ((test/find-all-nearest 0) pair/stx? "  de 	  edsd #'r |jhjh| \"Ouaf\"  736.34 " (list "#'r"))
    ((test/find-all-nearest 0) pair/stx? "(define (foo bar) (bar foo))" (list "(define (foo bar) (bar foo))" "(foo bar)" "(bar foo)"))
    ((test/find-all-nearest 3) pair/stx? "(define (foo bar) (bar foo))" (list "(define (foo bar) (bar foo))" "(foo bar)" "(bar foo)"))
    ((test/find-all-nearest 5) pair/stx? "(define (foo bar) (bar foo))" (list "(foo bar)" "(define (foo bar) (bar foo))" "(bar foo)"))
    ((test/find-all-nearest 4) pair/stx? "(define (foo bar) (bar foo))" (list "(define (foo bar) (bar foo))" "(foo bar)" "(bar foo)")) ; undefined case ; not specified.

    ; test/ellipsis : index string expected -> void
    (test/ellipsis 2 "(something ---)" "something")
    (test/ellipsis 3 "(something ---)" "something")
    (test/ellipsis 1 "(something ---)" "something")
    (test/ellipsis 0 "(something ---)" "NO----")
    (test/ellipsis 0 "something ---" "something")
    (test/ellipsis 0 "[name expr] ---" "[name expr]")
    (test/ellipsis 1 "[name expr] ---" "[name expr]")
    (test/ellipsis 6 "[name expr] ---" "[name expr]")
    (test/ellipsis 0 "something --- ---" "something")
    (test/ellipsis 7 "(let ([$name$ $binding$] ---) $body$ ---)" "[$name$ $binding$]")
    (test/ellipsis 30 "(let ([$name$ $binding$] ---) $body$ ---)" "$body$")
    
    ; test/-siblings-ellipsis : index string expected -> void
    (test/-siblings-ellipsis 0 "something ---" (list "something" 10 '---))
    (test/-siblings-ellipsis 0 "something" (list "NO----" -1 '||))
    (test/-siblings-ellipsis 0 "some things ---" (list "NO----" -1 '||))
    (test/-siblings-ellipsis 5 "some things ---" (list "things" 12 '---))
    (test/-siblings-ellipsis 0 "something --- ---" (list "something" 10 '---))
    (test/-siblings-ellipsis 0 "[name expr] ---" (list "[name expr]" 12 '---))
    (test/-siblings-ellipsis 1 "[name expr] ---" (list "[name expr]" 12 '---))
    (test/-siblings-ellipsis 6 "[name expr] ---" (list "[name expr]" 12 '---))
    (test/-siblings-ellipsis 0 "(something) ---" (list "(something)" 12 '---))
    (test/-siblings-ellipsis 0 "(something ---)" (list "NO----" -1 '||))
    (test/-siblings-ellipsis 1 "(something ---)" (list "something" 11 '---))
    (test/-siblings-ellipsis 7 "(let ([$name$ $binding$] ---) $body$ ---)" (list "[$name$ $binding$]" 25 '---))
    (test/-siblings-ellipsis 30 "(let ([$name$ $binding$] ---) $body$ ---)" (list "$body$" 37 '---))
    
    ; test/-all-ellipsis : index string expected -> void
    (test/-all-ellipsis 0 "" ()) 
    (test/-all-ellipsis 0 "something" ())
    (test/-all-ellipsis 0 "something ---" (list (list "something" 10 '---)))
    (test/-all-ellipsis 0 "something --- ---" (list (list "something" 10 '---)))
    (test/-all-ellipsis 1 "(something ---)" (list (list "something" 11 '---)))
    (test/-all-ellipsis 1 "(something ---) ---" (list (list "(something ---)" 16 '---)
                                                 (list "something" 11 '---)))
    (test/-all-ellipsis 2 "((something ---) ---)" (list (list "(something ---)" 17 '---)
                                                   (list "something" 12 '---)))
    (test/-all-ellipsis 3 "((something ---) ---) ---" (list (list "((something ---) ---)" 22 '---)
                                                       (list "(something ---)" 17 '---)
                                                       (list "something" 12 '---)))
    (test/-all-ellipsis 4 "(((something ---) ---) ---)" (list (list "((something ---) ---)" 23 '---)
                                                         (list "(something ---)" 18 '---)
                                                         (list "something" 13 '---)))
    (test/-all-ellipsis 5 "((((something ---) ---) ---) ---)" (list (list "(((something ---) ---) ---)" 29 '---)
                                                               (list "((something ---) ---)" 24 '---)
                                                               (list "(something ---)" 19 '---)
                                                               (list "something" 14 '---)))
    (test/-all-ellipsis 7 "((((((something ---) ---) ---) ---) ---) ---)" (list (list "(((((something ---) ---) ---) ---) ---)" 41 '---)
                                                                           (list "((((something ---) ---) ---) ---)" 36 '---)
                                                                           (list "(((something ---) ---) ---)" 31 '---)
                                                                           (list "((something ---) ---)" 26 '---)
                                                                           (list "(something ---)" 21 '---)
                                                                           (list "something" 16 '---)))
    (test/-all-ellipsis 7 "((((((some things ---) ---) ---) ---) ---) ---)" (list (list "(((((some things ---) ---) ---) ---) ---)" 43 '---)
                                                                             (list "((((some things ---) ---) ---) ---)" 38 '---)
                                                                             (list "(((some things ---) ---) ---)" 33 '---)
                                                                             (list "((some things ---) ---)" 28 '---)
                                                                             (list "(some things ---)" 23 '---)))
    (test/-all-ellipsis 12 "((((((some things ---) ---) ---) ---) ---) ---)" (list (list "(((((some things ---) ---) ---) ---) ---)" 43 '---)
                                                                              (list "((((some things ---) ---) ---) ---)" 38 '---)
                                                                              (list "(((some things ---) ---) ---)" 33 '---)
                                                                              (list "((some things ---) ---)" 28 '---)
                                                                              (list "(some things ---)" 23 '---)
                                                                              (list "things" 18 '---)))
    (test/-all-ellipsis 12 "((((((some things ---) ---) ---) (a ---) ---) ---) ---)" (list (list "(((((some things ---) ---) ---) (a ---) ---) ---)" 51 '---)
                                                                                      (list "((((some things ---) ---) ---) (a ---) ---)" 46 '---)
                                                                                      (list "((some things ---) ---)" 28 '---)
                                                                                      (list "(some things ---)" 23 '---)
                                                                                      (list "things" 18 '---)))
    (test/-all-ellipsis 34 "((((((some things ---) ---) ---) (a ---) ---) ---) ---)" (list (list "(((((some things ---) ---) ---) (a ---) ---) ---)" 51 '---)
                                                                                      (list "((((some things ---) ---) ---) (a ---) ---)" 46 '---)
                                                                                      (list "(a ---)" 41 '---)
                                                                                      (list "a" 36 '---)))
    (test/-all-ellipsis 33 "((((((some things ---) ---) ---) (a ---) ---) ---) ---)" (list (list "(((((some things ---) ---) ---) (a ---) ---) ---)" 51 '---)
                                                                                      (list "((((some things ---) ---) ---) (a ---) ---)" 46 '---)
                                                                                      (list "(a ---)" 41 '---)))


    ; test/forward : index string expected -> void
    (test/forward 0 "" false)
    (test/forward 0 "a b" "b")
    (test/forward 0 " a b " "a")
    (test/forward 2 "a b" false)
    (test/forward 0 "(define (foo bar) (bar foo))" false)
    (test/forward 1 "(define (foo bar) (bar foo))" "(foo bar)")
    (test/forward 8 "(define (foo bar) (bar foo))" "(bar foo)")
    (test/forward 9 "(define (foo bar) (bar foo))" "bar")
    (test/forward 23 "(define (foo bar) (bar foo))" false)
    
    ; test/backward : index string expected -> void
    (test/backward 0 "" false)
    (test/backward 0 "a b" false)
    (test/backward 1 "a b" "a")
    (test/backward 1 " a b " false)
    (test/backward 4 " a b " "b")
    (test/backward 0 "(define (foo bar) (bar foo))" false)
    (test/backward 7 "(define (foo bar) (bar foo))" "define")
    (test/backward 12 "(define (foo bar) (bar foo))" "foo")
    (test/backward 16 "(define (foo bar) (bar foo))" "bar")
    (test/backward 17 "(define (foo bar) (bar foo))" "(foo bar)")
    (test/backward 18 "(define (foo bar) (bar foo))" "(foo bar)")
    (test/backward 19 "(define (foo bar) (bar foo))" false)
    
    ; test/placeholder-forward : index string expected -> void
    (test/placeholder-forward  0 "$expr$" "$expr$")
    (test/placeholder-forward  0 "(let ([$name$ $binding$] ---) $body$ ---)" "$name$")
    (test/placeholder-forward 10 "(let ([foo $binding$] ---) $body$ ---)" "$binding$")
    (test/placeholder-forward 13 "(let ([foo $binding$] ---) $body$ ---)" "$body$")
    (test/placeholder-forward 14 "(let ([foo 234] ---) $body$ ---)" "$body$")
    (test/placeholder-forward 24 "(let ([foo 234] ---) foo ---)" (make-voice-exn "Unable to find the next placeholder."))
    (test/placeholder-forward 36 "(let ([$name$ $binding$] ---) $body$ ---)" "$name$")
    
    ; test/pos-mark-forward : index string expected -> void
    (test/pos-mark-forward  0 "" false)
    (test/pos-mark-forward  0 " foo bar " "foo")
    (test/pos-mark-forward  3 " (  ) foo bar " "foo")
    (test/pos-mark-forward  5 " ( (  ) ) foo bar " "foo")
    (test/pos-mark-forward 14 "(define (foo bar) (bar foo))" "(bar foo)")
    (test/pos-mark-forward 27 "(module nothing mzscheme ())" false)
    (test/pos-mark-forward  0 "$placeholder$ something" "something")
    
    ; test/pos-all-mark-forward : index string expected -> void
    (test/pos-all-mark-forward 0 "" empty)
    (test/pos-all-mark-forward 0 "a b c d e f g h " (list "a" "b" "c" "d" "e" "f" "g" "h"))
    (test/pos-all-mark-forward 5 "a b c d e f g h " (list "d" "e" "f" "g" "h"))
    (test/pos-all-mark-forward 9 "a (( b )  c  ( d) )   e (f g (h i j) )" (list "c" "( d)" "e" "(f g (h i j) )"))
    (test/pos-all-mark-forward 0 "$placeholder$ something" (list "something"))

    ; test/pos-fill-forward : index string expected -> void
    (test/pos-fill-forward  0 "" false)
    (test/pos-fill-forward  9 "a (( b )  c  ( d) )   e (f g (h i j) )" "e")
    (test/pos-fill-forward  0 " foo bar " false)
    (test/pos-fill-forward  1 "(foo bar)" false)
    (test/pos-fill-forward  1 "(foo)bar" "bar")
    (test/pos-fill-forward  1 "((foo) bar) dummy" "dummy")
    (test/pos-fill-forward  2 "((foo))bar" "bar")
    (test/pos-fill-forward 13 "(define (foo bar) (bar foo))" "(bar foo)")
    (test/pos-fill-forward 11 "(something) $placeholder$" false)
    
    ; test/distance/metric : (syntax -> boolean) (syntax -> non-negative-integer) string integer expected -> void
    (test/distance/metric number/stx? (metric-nearest (index->pos 0)) "" 2 "NOTHING-FOUND")
    (test/distance/metric number/stx? (metric-nearest (index->pos 0)) "123" 1 "123")
    (test/distance/metric number/stx? (metric-nearest (index->pos 0)) " 1 2 3 4 5 " 1 "1")
    (test/distance/metric number/stx? (metric-nearest (index->pos 0)) " 1 2 3 4 5 " 3 "3")
    (test/distance/metric number/stx? (metric-nearest (index->pos 0)) " 1 2 3 4 5 " 6 "NOTHING-FOUND")
    
    ; test/end : index string expected -> void
    #;((test/end 0 "" "NOT FOUND")
    (test/end 8 "(((a b c)) d)" "((a b c))")
    (test/end 9 "((((a b c))) d)" "(((a b c)))")
    (test/end 7 "(((a b c)) d)" "((a b c))")
    (test/end 3 "(a b c)" "b")
    (test/end 5 "a  b  c" "NOT FOUND")
    (test/end 1 "a  b  c" "NOT FOUND")
    (test/end 0 "a  b  c" "a")
    (test/end 6 "(a b c)" "(a b c)")
    (test/end 12 "((a ((b c) d)) e) f" "(a ((b c) d))")
    (test/end 4 "(a b c)" "NOT FOUND"))
    
    ; test/rank : (syntax -> boolean) index (syntax -> non-negative-integer) string expected -> void
    ;(test/rank number/stx? 10 (metric-forward 0 40) " 1 2 3 43 32 7654 alpha 54 'beta 09  " 4)
    
    )
  
  (tests))