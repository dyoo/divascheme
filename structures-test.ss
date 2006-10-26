(module structures-test mzscheme
  (require "test-harness.ss"
           (lib "etc.ss")
           (lib "list.ss")
           (lib "struct.ss")
           "utilities.ss"
           "structures.ss")

  (define (tests)
    ; empty-world : World
    (define empty-world (make-World ""
                                    empty
                                    (index->syntax-pos 0)
                                    #f
                                    0
                                    (index->syntax-pos 0)
                                    0
                                    (lambda () ())
                                    (lambda () ())
                                    false
                                    false
                                    false
                                    (lambda () ())
                                    (lambda () ())
                                    false
                                    ""))
    ; World-selection : World -> (union string false)
    ; test/selection : index non-negative-integer string expected -> void
    (define (test/selection index length text expected)
      (test (World-selection (copy-struct World empty-world
                                          [World-text text]
                                          [World-cursor-position (index->syntax-pos index)]
                                          [World-selection-length length])) expected))
    ; World-mark : World -> (union string false)
    ; test/mark : index non-negative-integer string expected -> void
    (define (test/mark index length text expected)
      (test (World-mark (copy-struct World empty-world
                                     [World-text text]
                                     [World-mark-position (index->syntax-pos index)]
                                     [World-mark-length length])) expected))
    
    (print-tests true)

    ; sexp-string? : string -> bool
    (test (sexp-string? "") false)
    (test (sexp-string? "()") true)
    (test (sexp-string? " ()") false)
    (test (sexp-string? "() ") false)
    (test (sexp-string? "[]") false)
    (test (sexp-string? "{}") false)
    (test (sexp-string? "( )") true)
    (test (sexp-string? "    ") false)
    (test (sexp-string? "foo") false)
    (test (sexp-string? "(define (foo x y z) (+ x y z))") true)
    (test (sexp-string? "()()") false)
    (test (sexp-string? "(let ([$name$ $binding$] ---) $body$ ---)") true)
    (test (sexp-string? "(~a $expr$ ---)") true)
    (test (sexp-string? "($expr$ ---)") true)
    
    ; test/selection : index non-negative-integer string expected -> void
    (test/selection 0 0 "" false)
    (test/selection 2 3 "(define (foo bar) (bar foo))" "efi")
    (test/selection 5 0 "(let ([x 3][y 4]) (+ x y))" false)
    (test/selection 6 -3 "(raise raise)" "ise")
    
    ; test/mark : index non-negative-integer string expected -> void
    (test/mark 0 0 "" false)
    (test/mark 7 9 "(test/mark 0 0 \"\" false)" "ark 0 0 \"")
    (test/mark 10 0 "(lambda (a b c) (* a b c))" false)
    (test/mark 1 -1 "bottle" "b")

    )
  
  (tests))