(module test-cleanup-whitespace mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2 8))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2 8))
           "../rope.ss"
           "../cleanup-whitespace.ss")
  
  ;; cw: string -> string
  ;; Show how cleanup-whitespace handles strings.
  (define (cw s)
    (let-values ([(a-rope _)
                  (cleanup-whitespace (string->rope s) 0 '())])
      (rope->string a-rope)))
  
  
  ;; cw/m: string (listof natural) -> (listof natural)
  ;; Just focus on how markers are preserved after cleanup.
  (define (cw/m s m)
    (let-values ([(_ markers)
                  (cleanup-whitespace (string->rope s) 0 m)])
      markers))
  
  (define (cw/im s i m) 
    (let-values ([(new-rope markers)
                  (cleanup-whitespace (string->rope s) i m)])
      (list (rope->string new-rope) markers)))
  
  
  (define cleanup-whitespace-tests
    (test-suite
     "cleanup-whitespace.ss"
     (test-case
      "empty case"
      (check-equal? (cw "") ""))
     
     (test-case
      "only whitespace"
      (check-equal? (cw "      ") ""))
     
     (test-case
      "simple case"
      (check-equal? (cw "hello") "hello"))
     
     (test-case
      "another simple case"
      (check-equal? (cw "hello  world") "hello world"))
     
     (test-case
      "spaces around an atom"
      (check-equal? (cw " hello ") "hello"))
     
     (test-case
      "spaces in sexp"
      (check-equal? (cw "(hello world this is a test )")
                    "(hello world this is a test)"))
     
     (test-case
      "newlines"
      (check-equal? (cw "(hello world  \n\n    this is a test )")
                    "(hello world\n\nthis is a test)"))
     
     
     (test-case
      "simple markers 0"
      (check-equal? (cw/m "    " (list 20 30))
                    (list 16 26)))
     
     (test-case
      "simple markers 1"
      (check-equal? (cw/m "hello   world" (list 0 4))
                    (list 0 4)))
     
     (test-case
      "simple markers 2"
      (check-equal? (cw "  hello   world")
                    "hello world")
      (check-equal? (cw/m "  hello   world" (list 2 6))
                    (list 0 4)))
     
     (test-case 
      "cleaning up newlines" 
      (check-equal? (cw/im "\n" 94 '(95 95 99 99))
                    '("\n" (95 95 99 99))))
     
     (test-case
      "simple markers 3"
      (check-equal? (cw/m "    " (list 0 6))
                    (list 0 2)))
     
     (test-case
      "comment cleanup"
      (check-equal? (cw " ;;hello   world  ")
                    ";;hello   world"))
     (test-case
      "simple cond" 
      (check-equal? (cw/im " (cond $expr$ ---) " 0 '(0 19 19 19))
                    '("(cond $expr$ ---)" (0 17 17 17))))
     
     
     (test-case
      "shifting the starting position"
      (check-equal? (cw/im "  " 5874 (list 5876 5876 6163 6163))
                    (list "" (list 5874 5874 6161 6161)))
      (check-equal? (cw/im "   ;; decrease> "
                           5874
                           ' (5877 5885))
                    (list ";; decrease>"
                          ' (5874 5882))))
     
     (test-case 
      "this illustrated a bug" 
      (check-equal? (cw/im "     (y \nblah)))" 9 '(18 22 26 26))
                    '("(y\nblah)))" 
                      (12 16 20 20))))
     
     (test-case
      "another test"
      (check-equal? (cw/im ";; hello" 72 '(72 80))
                    '(";; hello" (72 80))))
     (test-case
      "another test"
      (check-equal? (cw/im "\n;; hello" 72 '(73 81))
                    '("\n;; hello" (73 81))))
     ))
  
  
  (define (string->rope/degenerate a-str)
    (let loop ([acc rope-empty]
               [i 0])
      (cond
        [(< i (string-length a-str))
         (loop (rope-append acc (string->rope (string (string-ref a-str i))))
               (add1 i))]
        [else
         acc])))
  
  
  (define (performance-test)
    (let ([big-rope
           (string->rope/degenerate "'((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((($expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---) $expr$ ---)")])
      (time
       (cleanup-whitespace (rope-append* big-rope
                                         big-rope
                                         big-rope
                                         big-rope
                                         big-rope)
                           0 '()))))
  
  
  
  
  
  (test/text-ui cleanup-whitespace-tests))