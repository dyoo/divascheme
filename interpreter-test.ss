(module interpreter-test mzscheme
  (require (lib "class.ss")
           (lib "etc.ss")
           (lib "list.ss") 
           (lib "struct.ss")
           "test-harness.ss"
           "utilities.ss"
           "structures.ss"
           "templates.ss"
           "actions.ss"
           "interpreter.ss")
  
  (define (tests)
    ; empty-world : World
    (define empty-world (make-World ""
                                    empty
                                    (index->syntax-pos 0)
                                    #f
                                    0
                                    (index->syntax-pos 0)
                                    0
                                    (default-Next-f)
                                    (default-Previous-f)
                                    false
                                    false
                                    false
                                    (default-Magic-f)
                                    (default-Pass-f)
                                    false
                                    ""))

    ; eval-asts : index non-negative-integer index non-negative-integer string (union string false) (ast list) -> world
    (define (eval-asts cursor-index selection-length mark-index mark-length text clipboard-content asts)
      (let* ([actions  (make-object actions%)]
             [world    (copy-struct World empty-world
                                    [World-text text]
                                    [World-syntax-list (string->syntax-list text)]
                                    [World-cursor-position (index->syntax-pos cursor-index)]
                                    [World-selection-length selection-length]
                                    [World-mark-position (index->syntax-pos mark-index)]
                                    [World-mark-length mark-length])]
             [_        (set-clipboard-content clipboard-content)]
             [eval-PST (lambda (world ast)
                         (with-handlers ([voice-exn? (lambda args world)]
                                         [voice-exn/world? (lambda (exn) (voice-exn/world-world exn))])
                           (parameterize ([current-actions actions])
                             (eval-Protocol-Syntax-Tree world ast))))])
        (foldl (lambda (ast acc) (eval-PST acc ast)) world asts)))
        
    ; test/asts : index non-negative-integer string (ast list) expected -> void
    (define (test/asts cursor-index selection-length text asts . expected)
      (let ([world (eval-asts cursor-index selection-length 0 0 text false asts)])
        (test (list [World-cursor-index world]
                    [World-selection-length world]
                    [World-text world])
              expected)))
    
    ; test/ast : index non-negative-integer string ast expected -> void
    (define (test/ast cursor-index selection-length text ast . expected)
      (eval `(,test/asts ,cursor-index ,selection-length ,text (,list ,ast) ,@expected)))
    
    ; test/mark-asts : index non-negative-integer index non-negative-integer string (ast list) expected -> void
    (define (test/mark-asts cursor-index selection-length mark-index mark-length text asts . expected)
      (let ([world (eval-asts cursor-index selection-length mark-index mark-length text false asts)])
        (test (list [World-cursor-index world]
                    [World-selection-length world]
                    [World-mark-index world]
                    [World-mark-length world]
                    [World-text world])
              expected)))
    
    ; test/mark-ast : index non-negative-integer index non-negative-integer string ast expected -> void
    (define (test/mark-ast cursor-index selection-length mark-index mark-length  text ast . expected)
      (eval `(,test/mark-asts ,cursor-index ,selection-length ,mark-index ,mark-length  ,text (,list ,ast) ,@expected)))
    
    ; test/clip-asts : index non-negative-integer string (union false string) (ast list) expected -> void
    (define (test/clip-asts cursor-index selection-length text clipboard-content asts . expected)
      (let ([world (eval-asts cursor-index selection-length 0 0 text clipboard-content asts)])
        (test (list [World-cursor-index world]
                    [World-selection-length world]
                    [World-text world]
                    (get-clipboard-content))
              expected)))
    
    ; test/clip-ast : index non-negative-integer string false ast expected -> void
    (define (test/clip-ast cursor-index selection-length text clipboard-content ast . expected)
      (eval `(,test/clip-asts ,cursor-index ,selection-length ,text ,clipboard-content (,list ,ast) ,@expected)))
    
    ; inc/dec-what-distance : (union What false) int -> What
    ; test/inc/dec-what-distance : ((union What false) int -> What) -> (union What false) int expected -> void
    (define ((test/inc/dec-what-distance inc/dec-what-distance) what/false x expected)
      (let ([whatdn (inc/dec-what-distance what/false x)])
        (test (WhatDN-distance whatdn) expected)))
    
    ; inc-what-distance : (union What false) int -> What
    ; test/inc-what-distance : (union What false) int expected -> void
    (define test/inc-what-distance
      (test/inc/dec-what-distance inc-what-distance))
      
    ; dec-what-distance : (union What false) int -> What
    ; test/dec-what-distance : (union What false) int expected -> void
    (define test/dec-what-distance
      (test/inc/dec-what-distance dec-what-distance))
    
    ; inc/dec-Loc-distance : Loc int -> Loc
    ; test/inc/dec-Loc-distance : (Loc int -> Loc) -> Loc int expected -> void
    (define ((test/inc/dec-Loc-distance inc/dec-Loc-distance) loc n expected)
      (let ([loc (inc/dec-Loc-distance loc n)])
        (test (WhatDN-distance (Loc-what loc)) expected)))
      
    ; inc-Loc-distance : Loc int -> Loc
    ; test/inc-Loc-distance : Loc int expected -> void
    (define test/inc-Loc-distance
      (test/inc/dec-Loc-distance inc-Loc-distance))
    
    ; dec-Loc-distance : Loc int -> Loc
    ; test/dec-Loc-distance : Loc int expected -> void
    (define test/dec-Loc-distance
      (test/inc/dec-Loc-distance dec-Loc-distance))
    
    ; revert-cursor : World World -> World
    ; test/revert-cursor : cursor-index -> void
    (define (test/revert-cursor cursor-index)
      (let ([old-world (copy-struct World empty-world
                                    [World-cursor-position (index->syntax-pos cursor-index)])]
            [new-world empty-world])
        (test (World-cursor-index (revert-cursor old-world new-world)) 
              cursor-index)))

    (print-tests true)
    
  
    ; test/inc-what-distance : (union What false) int expected -> void
    (test/inc-what-distance (make-WhatN (make-The-Symbol 'dummy)) 76 77)
    (test/inc-what-distance (make-WhatN (make-Symbol-Noun 'hips)) 89 90)
    (test/inc-what-distance (make-WhatDN 0 (make-The-Symbol 'gloups)) 27 27)
    (test/inc-what-distance (make-WhatDN -3 (make-Symbol-Noun 'patatra)) 12 9)
    
    ; test/dec-what-distance : (union What false) int expected -> void
    (test/dec-what-distance (make-WhatN (make-The-Symbol 'singing)) 12 -11)
    (test/dec-what-distance (make-WhatDN 4 (make-Symbol-Noun 'rain)) -1 5)
    
    ; test/inc-Loc-distance : Loc int expected -> void
    (test/inc-Loc-distance (make-Loc (make-After)  (make-WhatN (make-The-Symbol 'Numb))) 0 1)
    (test/inc-Loc-distance (make-Loc (make-Before) (make-WhatN (make-Symbol-Noun '|Coma White|))) 8 9)
    (test/inc-Loc-distance (make-Loc (make-After)  (make-WhatDN -3 (make-Symbol-Noun  '|In The End|))) -2 -5)
    (test/inc-Loc-distance (make-Loc (make-Before) (make-WhatDN  4 (make-The-Symbol '|Nothing|))) -1 3)
    
    ; test/dec-Loc-distance : Loc int expected -> void
    (test/dec-Loc-distance (make-Loc (make-Before) (make-WhatDN 0 (make-Symbol-Noun '|Here With Me|))) 0 0)
    (test/dec-Loc-distance (make-Loc (make-After)  (make-WhatDN 2 (make-The-Symbol '|Behind Blue Eyes|))) 1 1)
    (test/dec-Loc-distance (make-Loc (make-Before) (make-WhatN (make-The-Symbol '|Thank You|))) 3 -2)
    (test/dec-Loc-distance (make-Loc (make-After)  (make-WhatN (make-Symbol-Noun '|Antechrist Superstar|))) -3 4)
    
    ; test/revert-cursor : cursor-index -> void
    (test/revert-cursor 0)
    (test/revert-cursor 1)
    (test/revert-cursor 2)
    (test/revert-cursor 3)
    (test/revert-cursor 4)
    (test/revert-cursor 5)
    (test/revert-cursor 6)
    (test/revert-cursor 7)
    (test/revert-cursor 3749)
    
    ; eval-What/open : What/false -> symbol/false
    (test (eval-What/open false) false)
    (test (eval-What/open (make-WhatN (make-Symbol-Noun '|Disposable Teens|))) '|Disposable Teens|)
    (test (eval-What/open (make-WhatDN 1 (make-Symbol-Noun '|The Dope Show|))) '|The Dope Show|)
    
    ; make-make-metric : World (pos pos -> metric) -> pos -> metric
    (test ((make-make-metric empty-world (lambda (base last) `(BONG ,base ,last))) 'BANG) (list 'BONG 'BANG (index->syntax-pos 0)))
    
    ; make-metric-w/world : World (pos pos -> metric) -> metric
    (test (make-metric-w/world empty-world (lambda (base last) `(BONG ,base ,last))) (list 'BONG (index->syntax-pos 0) (index->syntax-pos 0)))
    
    
    ; test/ast : index non-negative-integer string ast expected -> void
    (test/ast 0 0 ""
              (make-Verb (make-Command 'Open)
                         false
                         false)
              1 6 "($expr$ ---)")
    
    (test/ast 0 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))"
              (make-Verb (make-Command 'Open)
                         false
                         false)
              1 6 "($expr$ ---) (define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))")

    (test/ast 0 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))"
              (make-Verb (make-Command 'Open)
                         false
                         (make-WhatN (make-Symbol-Noun 'let)))
              7 6 "(let ([$name$ $binding$] ***)\n  $body$ +++) (define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))")
    
    (test/ast 0 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))"
              (make-Verb (make-Command 'Open)
                         false
                         (make-WhatN (make-The-Symbol 'let)))
              0 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))")

    (test/ast 0 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))"
              (make-Verb (make-Command 'Open)
                         false
                         (make-WhatDN 1 (make-Symbol-Noun 'foo)))
              5 6 "(foo $expr$ ---) (define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))")

    (test/ast 0 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))"
              (make-Verb (make-Command 'Open)
                         (make-Loc (make-After)
                                   (make-WhatN (make-Symbol-Noun 'define)))
                         false)
              32 6 "(define (foo x y z) (+ x y z)) ($expr$ ---) (define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))")
    
    (test/ast 0 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))"
              (make-Verb (make-Command 'Open)
                         (make-Loc (make-Before)
                                   (make-WhatDN 3 (make-The-Symbol 'foo)))
                         (make-WhatN (make-Symbol-Noun 'id)))
              73 6 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define ((id $expr$) foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))")

    ; test/asts : index non-negative-integer string (ast list) expected -> void
    (test/asts 0 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))"
               (list (make-Verb (make-Command 'Open)
                                false
                                (make-WhatDN 1 (make-Symbol-Noun 'foo)))
                     (make-Verb (make-Command 'Open)
                                false
                                (make-WhatN (make-Symbol-Noun 'let)))
                     (make-Verb (make-Command 'Open)
                                false
                                false))
               13 6 "(foo (let ([($expr$ ---) $binding$]\n           [$name$ $binding$] +++)\n       $body$ +++) $expr$ ---) (define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))")
    
    ; test/ast : index non-negative-integer string ast expected -> void
    (test/ast 0 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))"
              (make-Verb (make-Command 'Open-Square)
                         false
                         false)
              1 6 "[$expr$ ---] (define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))")
    
    (test/ast 30 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))"
              (make-Verb (make-Command 'Open-Square)
                         (make-Loc (make-Before)
                                   (make-WhatDN 1 (make-The-Symbol '+))) 
                         (make-WhatN (make-Symbol-Noun 'foobar)))
              59 6 "(define (foo x y z) (+ x y z))(define (foo x y z) ([foobar $expr$ ---] + x y z))(define (foo x y z) (+ x y z))")
    
    ; test/ast : index non-negative-integer string ast expected -> void
    (test/ast 0 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))"
              (make-Verb (make-Symbol-Cmd 'foo)
                         false
                         false)
              3 0 "foo (define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))")

    (test/ast 0 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))"
              (make-Verb (make-Symbol-Cmd 'foo)
                         (make-Loc (make-After) (make-WhatDN 2 (make-Symbol-Noun 'define)))
                         false)
              64 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z)) foo (define (foo x y z) (+ x y z))")
    
    ; test/ast : index non-negative-integer string ast expected -> void
    (test/ast 0 0 "(some things) (tree)"
              (make-Verb (make-Command 'Close)
                         false
                         false)
              0 0 "(some things) (tree)")
    
    (test/ast 4 6 "(id $expr$)"
              (make-Verb (make-Command 'Close)
                         false
                         false)
              4 6 "(id $expr$)")
    
    (test/ast 5 6 "(foo $expr$ ---)"
              (make-Verb (make-Command 'Close)
                         false
                         false)
              5 0 "(foo)")
    
    (test/ast 7 6 "(cond [$test$ $expr$ ---] ---)"
              (make-Verb (make-Command 'Close)
                         false
                         false)
              6 0 "(cond)")
    (test/ast 14 6 "(cond [$test$ $expr$ ---] ---)"
              (make-Verb (make-Command 'Close)
                         false
                         false)
              7 6 "(cond [$test$] ---)")
 
    ; test/asts : index non-negative-integer string (ast list) expected -> void
    (test/asts 0 0 ""
               (list (make-Verb (make-Symbol-Cmd 'define)
                                false
                                false)
                     (make-Verb (make-Symbol-Cmd 'fact)
                                false
                                false)
                     (make-Verb (make-Symbol-Cmd 'n)
                                false
                                false)
                     (make-Verb (make-Command 'Close)
                                false
                                false)
                     (make-Verb (make-Symbol-Cmd 'cond)
                                false
                                false)
                     (make-Verb (make-Command 'Open)
                                false
                                (make-WhatN (make-Symbol-Noun '=)))
                     (make-Verb (make-Symbol-Cmd 'n)
                                false
                                false)
                     (make-Verb (make-Symbol-Cmd '|0|)
                                false
                                false)
                     (make-Verb (make-Command 'Close)
                                false
                                false)
                     (make-Verb (make-Symbol-Cmd '|1|)
                                false
                                false)
                     (make-Verb (make-Command 'Close)
                                false
                                false)
                     (make-Verb (make-Command 'Close)
                                false
                                false)
                     (make-Verb (make-Command 'Open)
                                false
                                false)
                     (make-Verb (make-Symbol-Cmd '*)
                                false
                                false)
                     (make-Verb (make-Symbol-Cmd 'n)
                                false
                                false)
                     (make-Verb (make-Command 'Open)
                                false
                                (make-WhatN (make-Symbol-Noun 'fact)))
                     (make-Verb (make-Command 'Open)
                                false
                                (make-WhatN (make-Symbol-Noun '-)))
                     (make-Verb (make-Symbol-Cmd 'n)
                                false
                                false)
                     (make-Verb (make-Symbol-Cmd '|1|)
                                false
                                false)
                     (make-Verb (make-Command 'Close)
                                false
                                false)
                     (make-Verb (make-Command 'Close)
                                false
                                false)
                     (make-Verb (make-Command 'Close)
                                false
                                false)
                     (make-Verb (make-Command 'Close)
                                false
                                false)
                     (make-Verb (make-Command 'Close)
                                false
                                false)
                     (make-Verb (make-Command 'Open)
                                false
                                (make-WhatN (make-Symbol-Noun 'fact)))
                     (make-Verb (make-Symbol-Cmd '|10|)
                                false
                                false)
                     (make-Verb (make-Command 'Close)
                                false
                                false))
               84 0 "(define (fact n)\n  (cond\n    [(= n 0) 1]\n    [else (* n (fact (- n 1)))])) (fact 10)")
    
    ; test/ast : index non-negative-integer string ast expected -> void
    (test/ast 0 0 "(define (foo x y z) (+ x y z))"
              (make-Verb (make-Command 'Insert)
                         (make-Loc (make-Before)
                                   (make-WhatN (make-Symbol-Noun 'define)))
                         false)
              0 0 "(define (foo x y z) (+ x y z))")
    
    (test/ast 0 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))"
              (make-Verb (make-Command 'Insert)
                         (make-Loc (make-Before)
                                   (make-WhatDN 2 (make-The-Symbol 'define)))
                         false)
              31 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))")

    (test/ast 0 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))"
              (make-Verb (make-Command 'Insert)
                         (make-Loc (make-After)
                                   (make-WhatDN 3 (make-The-Symbol 'y)))
                         false)
              46 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))")

    ; test/asts : index non-negative-integer string (ast list) expected -> void
    (test/asts 0 0 ""
               (list (make-Verb (make-Command 'Open)
                                false
                                (make-WhatN (make-Symbol-Noun 'cond)))
                     (make-Verb (make-Command 'Insert)
                                (make-Loc (make-Before)
                                          (make-WhatN (make-Symbol-Noun 'cond)))
                                false))
               0 0 "(cond\n  [$test$ $expr$ ---] +++\n  [else $expr$ ---])")
    
    ; test/ast : index non-negative-integer string ast expected -> void
    (test/ast 0 0 ""
              (make-Verb (make-Command 'Select)
                         false
                         false)
              0 0 "")
    
    (test/ast 28 0 "(define (foo bar) (bar foo))(define (foo bar) (bar foo))(define (foo bar) (bar foo))"
              (make-Verb (make-Command 'Select)
                         false
                         (make-WhatN (make-Symbol-Noun 'define)))
              28 28 "(define (foo bar) (bar foo))(define (foo bar) (bar foo))(define (foo bar) (bar foo))")
    
    (test/ast 56 0 "(define (foo bar) (bar foo))(define (foo bar) (bar foo))(define (foo bar) (bar foo))"
              (make-Verb (make-Command 'Select)
                         false
                         (make-WhatN (make-Symbol-Noun 'define)))
              56 28 "(define (foo bar) (bar foo))(define (foo bar) (bar foo))(define (foo bar) (bar foo))")

    (test/ast 0 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))"
              (make-Verb (make-Command 'Search-Forward)
                         false
                         (make-WhatDN 2 (make-Symbol-Noun '+)))
              50 9 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))")

    (test/ast 30 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))"
              (make-Verb (make-Command 'Search-Forward)
                         false
                         (make-WhatDN 3 (make-Symbol-Noun 'define)))
              90 30 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))")
    
    (test/ast 30 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))"
              (make-Verb (make-Command 'Search-Backward)
                         false
                         (make-WhatDN 3 (make-Symbol-Noun 'define)))
              120 30 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))")
    
    (test/ast 30 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))"
              (make-Verb (make-Command 'Search-Backward)
                         (make-Loc (make-After)
                                   (make-WhatDN 5 (make-The-Symbol 'foo)))
                         (make-WhatDN 3 (make-The-Symbol 'define)))
              121 6 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))")
    
    ; test/ast : index non-negative-integer string ast expected -> void
    (test/ast 0 0 ""
              (make-Verb (make-Command 'Next)
                         false
                         false)
              0 0 "")
    
    (test/ast 32 43 "(define (foo bar) (+ (bar foo) 1 2 980 (* 4 3)))"
              (make-Verb (make-Command 'Next)
                         false
                         false)
              32 43 "(define (foo bar) (+ (bar foo) 1 2 980 (* 4 3)))")
    
    (test/ast 0 0 ""
              (make-Verb (make-Command 'Previous)
                         false
                         false)
              0 0 "")
    
    (test/ast 56 14 "(module test mzscheme (require (lib \"dummy.ss\")) (printf \"Be Happy!~n\"))"
              (make-Verb (make-Command 'Previous)
                         false
                         false)
              56 14 "(module test mzscheme (require (lib \"dummy.ss\")) (printf \"Be Happy!~n\"))")
                                       
    ; test/asts : index non-negative-integer string (ast list) expected -> void
    (test/asts 30 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))"
               (list
                (make-Verb (make-Command 'Search-Backward)
                           (make-Loc (make-After)
                                     (make-WhatDN 5 (make-The-Symbol 'foo)))
                           (make-WhatDN 3 (make-The-Symbol 'define))))
              121 6 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))")
    
    (test/asts 30 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))"
               (list
                (make-Verb (make-Command 'Search-Backward)
                           (make-Loc (make-After)
                                     (make-WhatDN 5 (make-The-Symbol 'foo)))
                           (make-WhatDN 3 (make-The-Symbol 'define)))
                (make-Verb (make-Command 'Next)
                           false
                           false))
               91 6 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))")
    
    (test/asts 80 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))"
               (list
                (make-Verb (make-Command 'Search-Backward)
                           (make-Loc (make-Before)
                                     (make-WhatN (make-Symbol-Noun 'foo)))
                           (make-WhatN (make-The-Symbol 'z)))
                (make-Verb (make-Command 'Previous)
                           false
                           false))
               57 1 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))")
#;    
    (test/asts 80 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))"
               (list
                (make-Verb (make-Command 'Search-Backward)
                           (make-Loc (make-Before)
                                     (make-Under-Cursor))
                           (make-Under-Cursor))
                (make-Verb (make-Command 'Next)
                           false
                           false)
                (make-Verb (make-Command 'Previous)
                           false
                           false))
               80 9 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))")
  #;  
    (test/asts 80 9 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))"
               (list
                (make-Verb (make-Command 'Search-Forward)
                           (make-Loc (make-Before)
                                     (make-Under-Cursor))
                           (make-Under-Cursor))
                (make-Verb (make-Command 'Next)
                           false
                           false)
                (make-Verb (make-Command 'Previous)
                           false
                           false))
               80 9 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))")

    ; test/ast : index non-negative-integer string ast expected -> void
    (test/ast 0 0 ""
              (make-Verb (make-Command 'Cancel)
                         false
                         false)
              0 0 "")
    
    (test/ast 47 87 "(lambda (x) x) (lambda (y) y)"
              (make-Verb (make-Command 'Cancel)
                         false
                         false)
              47 87 "(lambda (x) x) (lambda (y) y)")
    
    ; test/asts : index non-negative-integer string (ast list) expected -> void
    (test/asts 0 0 ""
               (list (make-Verb (make-Command 'Open)
                                false
                                false)
                     (make-Verb (make-Command 'Cancel)
                                false
                                false))
               0 0 "")
    
    (test/asts 0 0 ""
               (list (make-Verb (make-Command 'Open)
                                false
                                (make-WhatN (make-Symbol-Noun 'id)))
                     (make-Verb (make-Command 'Cancel)
                                false
                                false))
               0 0 "")
    
    (test/asts 10 9 "(foo bar) (foo bar) (foo bar)"
               (list (make-Verb (make-Command 'Open-Square)
                                (make-Loc (make-After)
                                          (make-WhatDN 2 (make-Symbol-Noun 'foo)))
                                false)
                     (make-Verb (make-Command 'Cancel)
                                false
                                false))
               10 9 "(foo bar) (foo bar) (foo bar)")
    
    (test/asts 9 5 "(define (dummy x) (x x))"
               (list (make-Verb (make-Command 'Open-Square)
                                (make-Loc (make-Before)
                                          (make-WhatN (make-The-Symbol 'define)))
                                (make-WhatN (make-Symbol-Noun 'let)))
                     (make-Verb (make-Command 'Close)
                                false
                                false)
                     (make-Verb (make-Command 'Cancel)
                                false
                                false))
               8 6 "([let ([$name$ $binding$] ***)\n   $body$ +++] define (dummy x) (x x))")
    
    (test/asts 6 3 "(fact $n$)"
               (list (make-Verb (make-Symbol-Cmd '|100|)
                                false
                                false)
                     (make-Verb (make-Command 'Cancel)
                                false
                                false)
                     (make-Verb (make-Symbol-Cmd '|10|)
                                false
                                false))
               9 0 "(fact 10)")
                     
    (test/asts 60 30 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))"
               (list (make-Verb (make-Command 'Search-Forward)
                                false
                                (make-WhatN (make-Symbol-Noun '+)))
                     (make-Verb (make-Command 'Previous)
                                false
                                false)
                     (make-Verb (make-Command 'Previous)
                                false
                                false)
                     (make-Verb (make-Command 'Next)
                                false
                                false)
                     (make-Verb (make-Command 'Previous)
                                false
                                false)
                     (make-Verb (make-Command 'Next)
                                false
                                false)
                     (make-Verb (make-Command 'Next)
                                false
                                false)
                     (make-Verb (make-Command 'Cancel)
                                false
                                false))
               60 30 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))")
    
    (test/asts 0 0 ""
               (list (make-Verb (make-Command 'Open)
                                false
                                false)
                     (make-Verb (make-Command 'Open)
                                false
                                false)
                     (make-Verb (make-Command 'Open)
                                false
                                false)
                     (make-Verb (make-Command 'Cancel)
                                false
                                false)
                     (make-Verb (make-Command 'Cancel)
                                false
                                false))
               1 6 "($expr$ ---)")
    
    ; test/ast : index non-negative-integer string ast expected -> void
    (test/ast 0 0 ""
              (make-Verb (make-Command 'Undo)
                         false
                         false)
              0 0 "")
    
    (test/ast 42 67 "(Something some texts ...)"
              (make-Verb (make-Command 'Undo)
                         false
                         false)
              42 67 "(Something some texts ...)")
    
    (test/ast 0 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))"
              (make-Verb (make-Command 'Undo)
                         false
                         false)
              0 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))")
    
    (test/ast 0 0 ""
              (make-Verb (make-Command 'Redo)
                         false
                         false)
              0 0 "")
    
    (test/ast 97 124 "(Other things ) ..."
              (make-Verb (make-Command 'Redo)
                         false
                         false)
              97 124 "(Other things ) ...")
    
    (test/ast 0 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))"
              (make-Verb (make-Command 'Redo)
                         false
                         false)
              0 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))")
    
    ; test/asts : index non-negative-integer string (list ast) expected -> void
    (test/asts 25 3 "(foo bar) (foo bar) (foo bar)"
               (list (make-Verb (make-Command 'Search-Forward)
                                false
                                (make-WhatN (make-Symbol-Noun 'foo)))
                     (make-Verb (make-Command 'Undo)
                                false
                                false))
               0 9 "(foo bar) (foo bar) (foo bar)")
    
    (test/asts 25 3 "(foo bar) (foo bar) (foo bar)"
               (list (make-Verb (make-Symbol-Cmd 'let)
                                false
                                false)
                     (make-Verb (make-Command 'Undo)
                                false
                                false))
               25 3 "(foo bar) (foo bar) (foo bar)")
    
    (test/asts 25 3 "(foo bar) (foo bar) (foo bar)"
               (list (make-Verb (make-Symbol-Cmd 'let)
                                false
                                false)
                     (make-Verb (make-Command 'Redo)
                                false
                                false))
               32 6 "(foo bar) (foo bar) (foo (let ([$name$ $binding$] ***)\n                           $body$ +++))")
    
    (test/asts 25 3 "(foo bar) (foo bar) (foo bar)"
               (list (make-Verb (make-Symbol-Cmd 'let)
                                false
                                false)
                     (make-Verb (make-Command 'Undo)
                                false
                                false)
                     (make-Verb (make-Command 'Redo)
                                false
                                false))
               32 6 "(foo bar) (foo bar) (foo (let ([$name$ $binding$] ***)\n                           $body$ +++))")
    
    (test/asts 0 0 ""
               (list (make-Verb (make-Symbol-Cmd 'let)
                                false
                                false)
                     (make-Verb (make-Symbol-Cmd 'id)
                                false
                                false)
                     (make-Verb (make-Symbol-Cmd 'cond)
                                false
                                false)
                     (make-Verb (make-Command 'Undo)
                                false
                                false)
                     (make-Verb (make-Command 'Undo)
                                false
                                false)
                     (make-Verb (make-Command 'Redo)
                                false
                                false)
                     (make-Verb (make-Command 'Undo)
                                false
                                false)
                     (make-Verb (make-Command 'Undo)
                                false
                                false)
                     (make-Verb (make-Command 'Redo)
                                false
                                false)
                     (make-Verb (make-Command 'Redo)
                                false
                                false))
               10 9  "(let ([id $binding$]\n      [$name$ $binding$] +++)\n  $body$ +++)")
    
    
    ; test/ast : index non-negative-integer string ast expected -> void
    (test/ast 0 0 ""
              (make-Verb (make-Command 'Out)
                         false
                         false)
              0 0 "")
    
    (test/ast 14 1 "(define (fact n) n)"
              (make-Verb (make-Command 'Out)
                         false
                         false)
              8 8 "(define (fact n) n)")
    
    (test/ast 0 19 "(define (fact n) n)"
              (make-Verb (make-Command 'Out)
                         false
                         false)
              0 19 "(define (fact n) n)")

    (test/ast 2 0 "(  )"
              (make-Verb (make-Command 'Out)
                         false
                         false)
              0 4 "(  )")
    
    ; test/asts : index non-negative-integer string (list ast) expected -> void
    (test/asts 0 0 "(define (foo bar) (bar foo))"
               (list (make-Verb (make-Command 'Search-Forward)
                                false
                                (make-WhatN (make-The-Symbol 'bar)))
                     (make-Verb (make-Command 'Out)
                                false
                                false))
               8 9 "(define (foo bar) (bar foo))")
    
    (test/asts 0 0 "(define (foo bar) (bar foo))"
               (list (make-Verb (make-Command 'Search-Forward)
                                false
                                (make-WhatN (make-The-Symbol 'bar)))
                     (make-Verb (make-Command 'Out)
                                false
                                false)
                     (make-Verb (make-Command 'Cancel)
                                false
                                false))
               13 3 "(define (foo bar) (bar foo))")
                
    (test/asts 0 0 "(define (foo bar) (bar foo))"
               (list (make-Verb (make-Command 'Search-Forward)
                                false
                                (make-WhatN (make-The-Symbol 'bar)))
                     (make-Verb (make-Command 'Out)
                                false
                                false)
                     (make-Verb (make-Command 'Undo)
                                false
                                false))
               8 9 "(define (foo bar) (bar foo))")
    
    (test/asts 0 0 ""
               (list (make-Verb (make-Command 'Open)
                                false
                                false)
                     (make-Verb (make-Command 'Out)
                                false
                                false))
               0 12 "($expr$ ---)")

    (test/asts 0 0 ""
               (list (make-Verb (make-Command 'Open)
                                false
                                false)
                     (make-Verb (make-Command 'Out)
                                false
                                false)
                     (make-Verb (make-Command 'Cancel)
                                false
                                false))
               1 6 "($expr$ ---)")
                     
    (test/asts 0 0 ""
               (list (make-Verb (make-Command 'Open)
                                false
                                false)
                     (make-Verb (make-Command 'Out)
                                false
                                false)
                     (make-Verb (make-Command 'Undo)
                                false
                                false))
               0 0 "")
                     
    ; test/ast : index non-negative-integer string ast expected -> void
    (test/ast 0 0 ""
              (make-Verb (make-Command 'In)
                         false
                         false)
              0 0 "")
    
    (test/ast 0 9 "(foo bar)"
              (make-Verb (make-Command 'In)
                         false
                         false)
              1 3 "(foo bar)")
    
    (test/ast 0 0 "(foo bar)"
              (make-Verb (make-Command 'In)
                         false
                         false)
              1 3 "(foo bar)")
    
    (test/ast 0 0 " (foo bar)"
              (make-Verb (make-Command 'In)
                         false
                         false)
              0 0 " (foo bar)")
                     
    ; test/asts : index non-negative-integer string (ast list) expected -> void
    (test/asts 0 0 "((foo bar) dummy)"
               (list (make-Verb (make-Command 'In)
                                false
                                false)
                     (make-Verb (make-Command 'In)
                                false
                                false))
               2 3 "((foo bar) dummy)")
    
    (test/asts 0 0 "((foo bar) dummy)"
               (list (make-Verb (make-Command 'In)
                                false
                                false)
                     (make-Verb (make-Command 'Cancel)
                                false
                                false))
               0 0 "((foo bar) dummy)")
    
    (test/asts 0 0 "((foo bar) dummy)"
               (list (make-Verb (make-Command 'In)
                                false
                                false)
                     (make-Verb (make-Command 'Undo)
                                false
                                false))
               1 9 "((foo bar) dummy)")
    
    (test/asts 0 0 "((foo bar) dummy)"
               (list (make-Verb (make-Command 'In)
                                false
                                false)
                     (make-Verb (make-Command 'Out)
                                false
                                false))
               0 17 "((foo bar) dummy)")
               
    
    ; test/ast : index non-negative-integer string ast expected -> void
    (test/ast 0 0 ""
              (make-Verb (make-Command 'Forward)
                         false
                         false)
              0 0 "")
    
    (test/ast 0 0 ""
              (make-Verb (make-Command 'Backward)
                         false
                         false)
              0 0 "")
    
    (test/ast 0 0 "foo bar"
              (make-Verb (make-Command 'Forward)
                         false
                         false)
              0 3 "foo bar")
    
    (test/ast 0 0 "foo bar"
              (make-Verb (make-Command 'Backward)
                         false
                         false)
              0 0 "foo bar")
    
    (test/ast 7 0 "foo bar"
              (make-Verb (make-Command 'Forward)
                         false
                         false)
              7 0 "foo bar")
    
    (test/ast 7 0 "foo bar"
              (make-Verb (make-Command 'Backward)
                         false
                         false)
              4 3 "foo bar")
              
    ; test/asts : index non-negative-integer string (ast list) expected -> void
    (test/asts 0 0 "foo bar"
               (list (make-Verb (make-Command 'Forward)
                                false
                                false)
                     (make-Verb (make-Command 'Backward)
                                false
                                false))
               0 3 "foo bar")
    
    (test/asts 0 0 ""
               (list (make-Verb (make-Symbol-Cmd 'let)
                                false
                                false)
                     (make-Verb (make-Command 'Forward)
                                false
                                false))
               14 9 "(let ([$name$ $binding$] ***)\n  $body$ +++)")
    
    ; test/ast : index non-negative-integer string ast expected -> void
    (test/ast 0 0 ""
              (make-Verb (make-Command 'Delete)
                         false
                         false)
              0 0 "")
    
    (test/ast 23 3 "(define (foo bar) (bar foo))"
              (make-Verb (make-Command 'Delete)
                         false
                         false)
              22 0 "(define (foo bar) (bar))")
    
    (test/ast 0 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))"
              (make-Verb (make-Command 'Delete)
                         false
                         false)
              0 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))")
    
    (test/ast 50 9 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))"
              (make-Verb (make-Command 'Delete)
                         false
                         false)
              49 0 "(define (foo x y z) (+ x y z))(define (foo x y z))(define (foo x y z) (+ x y z))")
    
    ; test/asts : index non-negative-integer string (ast list) expected -> void
    (test/asts 0 0 ""
               (list (make-Verb (make-Symbol-Cmd 'let)
                                false
                                false)
                     (make-Verb (make-Command 'Delete)
                                false
                                false))
               7 0 "(let ([$binding$]\n      [$name$ $binding$] +++)\n  $body$ +++)")
    
    ; test/mark-ast : index non-negative-integer index non-negative-integer string ast expected -> void
    (test/mark-ast 0 0 0 0 ""
                   (make-Verb (make-Command 'Bring)
                              false
                              false)
                   0 0 0 0 "")
    
    (test/mark-ast 3 0 0 0 "(a  c) b d"
                   (make-Verb (make-Command 'Bring)
                              false
                              false)
                   4 0 8 1 "(a b c) d")
    
    (test/mark-ast 3 0 9 1 "(a  c) b d"
                   (make-Verb (make-Command 'Bring)
                              false
                              false)
                   4 0 9 0 "(a d c) b")
    
    (test/mark-ast 5 0 0 1 "b (a  c)"
                   (make-Verb (make-Command 'Bring)
                              false
                              false)
                   4 0 0 7 "(a b c)")
    
    (test/mark-ast 0 0 0 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))"
                   (make-Verb (make-Command 'Bring)
                              false
                              false)
                   0 0 0 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))")

    (test/mark-ast 1 0 0 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))"
                   (make-Verb (make-Command 'Bring)
                              false
                              false)
                   32 0 61 30 "((define (foo x y z) (+ x y z)) define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))")

    (test/mark-ast 0 0 8 11 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))"
                   (make-Verb (make-Command 'Bring)
                              false
                              false)
                   12 0 20 9 "(foo x y z) (define (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))")

    ; test/mark-asts : index non-negative-integer index non-negative-integer string (ast list) expected -> void
    (test/mark-asts 3 0 0 0 "(a  c) b d"
                    (list (make-Verb (make-Command 'Bring)
                                     false
                                     false)
                          (make-Verb (make-Command 'Bring)
                                     false
                                     false))
                    6 0 9 0 "(a b d c)")
    
    (test/mark-asts 3 0 0 0 "(a  c) b d"
                    (list (make-Verb (make-Command 'Bring)
                                     false
                                     false)
                          (make-Verb (make-Command 'Bring)
                                     false
                                     false)
                          (make-Verb (make-Command 'Cancel)
                                     false
                                     false))
                    4 0 8 1 "(a b c) d")
    
    (test/mark-asts 24 3 0 0 "(function foo bar dummy $x$ $y$ $z$) a b c d e f"
                    (list (make-Verb (make-Command 'Bring)
                                     false
                                     false)
                          (make-Verb (make-Command 'Next)
                                     false
                                     false))
                    26 3 35 1 "(function foo bar dummy a $y$ $z$) b c d e f")
    
    (test/mark-asts 24 3 0 0 "(function foo bar dummy $x$ $y$ $z$) a b c d e f"
                    (list (make-Verb (make-Command 'Bring)
                                     false
                                     false)
                          (make-Verb (make-Command 'Previous)
                                     false
                                     false))
                    26 3 35 1 "(function foo bar dummy a $y$ $z$) b c d e f")
    
    ; test/mark-ast : index non-negative-integer index non-negative-integer string ast expected -> void
    (test/mark-ast 0 0 0 0 ""
                   (make-Verb (make-Command 'Push)
                              false
                              false)
                   0 0 0 0 "")
    
    (test/mark-ast 0 0 3 0 "(a  c) b d"
                   (make-Verb (make-Command 'Push)
                              false
                              false)
                   8 1 4 0 "(a b c) d")
     
    (test/mark-ast 0 0 24 3 "(function foo bar dummy $x$ $y$ $z$) a b c d e f"
                   (make-Verb (make-Command 'Push)
                              false
                              false)
                   35 1 26 3 "(function foo bar dummy a $y$ $z$) b c d e f")

    ; test/mark-asts : index non-negative-integer index non-negative-integer string (ast list) expected -> void
    (test/mark-asts 3 0 0 0 "(a  c) b d"
                    (list (make-Verb (make-Command 'Bring)
                                     false
                                     false)
                          (make-Verb (make-Command 'Push)
                                     false
                                     false))
                    4 0 8 1 "(a b c) d")

    ; test/mark-ast : index non-negative-integer index non-negative-integer string ast expected -> void
    (test/mark-ast 0 0 0 0 ""
                   (make-Verb (make-Command 'Exchange)
                              false
                              false)
                   0 0 0 0 "")
    
    (test/mark-ast 432 435 1235 765 "(((foooooo baaaaaaar dummmmy) foooo (baaaar dummmmy foooo) baaar) ) dummmmy"
                   (make-Verb (make-Command 'Exchange)
                              false
                              false)
                   1235 765 432 435 "(((foooooo baaaaaaar dummmmy) foooo (baaaar dummmmy foooo) baaar) ) dummmmy")
    
    ; test/mark-asts : index non-negative-integer index non-negative-integer string (ast list) expected -> void
    (test/mark-asts 432 435 1235 765 "(((foooooo baaaaaaar dummmmy) foooo (baaaar dummmmy foooo) baaar) ) dummmmy"
                    (list (make-Verb (make-Command 'Exchange)
                                     false
                                     false)
                          (make-Verb (make-Command 'Exchange)
                                     false
                                     false))
                    432 435 1235 765 "(((foooooo baaaaaaar dummmmy) foooo (baaaar dummmmy foooo) baaar) ) dummmmy")

    ; test/mark-ast : index non-negative-integer index non-negative-integer string ast expected -> void
    (test/mark-ast 0 0 0 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))"
                   (make-Verb (make-Command 'Mark)
                              false
                              false)
                   0 0 0 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))")
    
    (test/mark-ast 30 6 60 30 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))"
                   (make-Verb (make-Command 'Mark)
                              false
                              false)
                   30 0 30 6 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))")

    (test/mark-ast 13 1 0 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))"
                   (make-Verb (make-Command 'Mark)
                              false
                              false)
                   13 0 13 1 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))")

    (test/mark-ast 30 6 60 30 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))"
                   (make-Verb (make-Command 'Mark)
                              (make-Loc (make-After)
                                        (make-WhatN (make-Symbol-Noun 'define)))
                              false)
                   30 0 30 6 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))")

    (test/mark-ast 30 6 60 30 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))"
                   (make-Verb (make-Command 'Mark)
                              (make-Loc (make-After)
                                        (make-WhatN (make-Symbol-Noun 'define)))
                              (make-WhatDN 2 (make-Symbol-Noun 'foo)))
                   30 6 8 11 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))")
#;
    (test/mark-ast 1 0 0 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))"
                   (make-Verb (make-Command 'Mark)
                              (make-Loc (make-Before) (make-Under-Cursor))
                              false)
                   1 0 1 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))")
    #;
    (test/mark-ast 8 0 0 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))"
                   (make-Verb (make-Command 'Mark)
                              false
                              (make-Under-Cursor))
                   8 0 8 11 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))")

    (test/mark-ast 33 0 0 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))"
                   (make-Verb (make-Command 'Mark)
                              (make-Loc (make-After) (make-WhatDN 3 (make-The-Symbol 'z)))
                              (make-WhatN (make-Symbol-Noun 'foo)))
                   33 0 8 11 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))")

    ; test/mark-asts : index non-negative-integer index non-negative-integer string (ast list) expected -> void
    (test/mark-asts 0 0 0 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))"
                    (list
                     (make-Verb (make-Command 'Mark)
                                false
                                (make-WhatN (make-The-Symbol 'define)))
                     (make-Verb (make-Command 'Next)
                                false
                                false))
                    0 0 31 6 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))")
    
    (test/mark-asts 0 0 0 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))"
                    (list
                     (make-Verb (make-Command 'Mark)
                                false
                                (make-WhatN (make-Symbol-Noun '+)))
                     (make-Verb (make-Command 'Next)
                                false
                                false)
                     (make-Verb (make-Command 'Previous)
                                false
                                false)
                     (make-Verb (make-Command 'UnMark)
                                false
                                false))
                    0 0 20 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))")
    
    ; test/clip-ast : index non-negative-integer string string ast expected -> void
    (test/clip-ast 0 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))" ""
                   (make-Verb (make-Command 'Copy)
                              false
                              false)
                   0 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))" "")
    
    (test/clip-ast 8 11 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))" ""
                   (make-Verb (make-Command 'Copy)
                              false
                              false)
                   8 11 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))" "(foo x y z)")
    
    (test/clip-ast 0 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))" ""
                   (make-Verb (make-Command 'Cut)
                              false
                              false)
                   0 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))" "")
    
    (test/clip-ast 20 9 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))" ""
                   (make-Verb (make-Command 'Cut)
                              false
                              false)
                   19 0 "(define (foo x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))" "(+ x y z)")
    
    (test/clip-ast 0 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))" ""
                   (make-Verb (make-Command 'Paste)
                         false
                         false)
                   0 0 "(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))(define (foo x y z) (+ x y z))" "")
    
    ; test/mark-ast : index non-negative-integer index non-negative-integer string ast expected -> void
    (test/mark-ast 0 0 0 0 ""
                   (make-Verb (make-Command 'Enter)
                              false
                              false)
                   1 0 1 0 "\n")
    
    (test/mark-ast 18 6 0 0 "(define (foo bar) $body$ ---)"
                   (make-Verb (make-Command 'Enter)
                              false
                              false)
                   20 6 0 0 "(define (foo bar)\n  $body$ ---)")
    
    ; test/mark-ast : index non-negative-integer index non-negative-integer string ast expected -> void
    (test/mark-ast 0 0 0 0 ""
                   (make-Verb (make-Command 'Join)
                              false
                              false)
                   0 0 0 0 "")
    
    (test/mark-ast 0 0 0 0 "\n"
                   (make-Verb (make-Command 'Join)
                              false
                              false)
                   0 0 0 0 "")
    
    (test/mark-ast 1 9 0 0 "((foo bar)\n (bar foo))"
                   (make-Verb (make-Command 'Join)
                              false
                              false)
                   1 9 0 0 "((foo bar) (bar foo))")
    
    
    ; test/ast : index non-negative-integer string ast expected -> void
    (test/ast 0 0 ""
              (make-Verb (make-Command 'Transpose)
                         false
                         false)
              0 0 "")
    
    (test/ast 0 7 "(first) (last)"
              (make-Verb (make-Command 'Transpose)
                         false
                         false)
              0 7 "(first) (last)")
    
    (test/ast 8 6 "(first) (last)"
              (make-Verb (make-Command 'Transpose)
                         false
                         false)
              14 0 "(last) (first)")

    (test/ast 7 0 "(first) (last)"
              (make-Verb (make-Command 'Transpose)
                         false
                         false)
              14 0 "(last) (first)")

    (test/ast 14 0 "(last) (first)"
              (make-Verb (make-Command 'Transpose)
                         false
                         false)
              14 0 "(last) (first)")
    

    ; test/asts : index non-negative-integer string (ast list) expected -> void
    (test/asts 0 0 ""
               (list (make-Verb (make-Command 'Magic)
                                false
                                false))
               0 0 "")
    
    (test/asts 0 0 ""
               (list (make-Verb (make-Symbol-Cmd 'd)
                                false
                                false)
                     (make-Verb (make-Command 'Magic)
                                false
                                false))
               1 0 "d")
    
    (test/asts 28 0 "(define (foo bar) (bar foo))"
               (list (make-Verb (make-Symbol-Cmd 'd)
                                false
                                false)
                     (make-Verb (make-Command 'Magic)
                                false
                                false))
               38 6 "(define (foo bar) (bar foo)) (define ($name$ $arg$ ---)\n                               $body$ +++)")
    
    ; test/ast : index non-negative-integer string ast expected -> void
    ; test/asts : index non-negative-integer string (ast list) expected -> void
    
    ; test/ast : index non-negative-integer string ast expected -> void
    ; test/asts : index non-negative-integer string (ast list) expected -> void
    
    ; test/ast : index non-negative-integer string ast expected -> void
    ; test/asts : index non-negative-integer string (ast list) expected -> void
    
    ; test/ast : index non-negative-integer string ast expected -> void
    ; test/asts : index non-negative-integer string (ast list) expected -> void
    
    
    (test/asts 0 0 "";(file->string "/Users/romain/engine.ss")
               (list (make-Verb (make-Command 'Cancel)
                                false
                                false))
               0 0 "")
    

    )
  
  (parameterize ([current-templates (lambda () mzscheme-templates)])
    (tests))
    
  )
