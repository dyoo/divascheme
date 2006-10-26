(module parser mzscheme
  (require (lib "unitsig.ss")
	   (prefix lex: (lib "lex.ss" "parser-tools"))
	   (prefix yacc: (lib "yacc.ss" "parser-tools"))
           (lib "class.ss")
           (lib "etc.ss")
           (lib "plt-match.ss")
	   "structures.ss")

  
  (provide make-parser)

  (define voice-debug false)
  (define (voice-printf text . args)
    (when voice-debug
      (apply printf text args)))


  
  ;; Definition of the group of token.
  ;; This is require by the parser.
  (lex:define-empty-tokens empty-token-group
                           (OPEN SQUARE CLOSE
                                 INSERT AFTER BEFORE
                                 SELECT SEARCH TOP BOTTOM
                                 HOLDER
                                 NEXT PREVIOUS CANCEL UNDO REDO MAGIC MAGIC-BASH MAGIC-WRAP PASS PASS-WRAP AGAIN
                                 OUT UP DOWN FORWARD BACKWARD YOUNGER OLDER FIRST LAST
                                 DELETE DEDOUBLE-ELLIPSIS
                                 BRING
                                 PUSH
                                 EXCHANGE
                                 MARK UNMARK
                                 COPY CUT PASTE
                                 DEFINITION USAGE
                                 HIT ONE THE SECOND THIRD FOURTH FIFTH SIXTH SEVENTH EIGHTH NINETH TENTH
                                 ZERO TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE TEN
                                 THIS THAT HERE ALL
                                 QUOTE QUASIQUOTE UNQUOTE UNQUOTE-SPLICING QUOTE-SYNTAX QUASISYNTAX UNSYNTAX UNSYNTAX-SPLICING
                                 ENTER JOIN INDENT
                                 TRANSPOSE
                                 TAG
                                 DUMMY SILENCE))


  (lex:define-tokens token-group (SYMBOL STRING))
  

  (lex:define-lex-abbrevs
   [symbol (complement (concatenation lex:any-string (union eol blank) lex:any-string))]
   [pipe   (concatenation #\| (complement (concatenation lex:any-string #\| lex:any-string)) #\|)]
   [eol    (union #\return #\newline)]
   [blank  (union #\tab #\space)]
   [escape-sequence (concatenation #\\ lex:any-char)]
   [char-sequence-without-double-quote (complement (concatenation lex:any-string #\" lex:any-string))]
   [char-sequence-without-backslash    (complement (concatenation lex:any-string #\\ lex:any-string))]
   [string-pattern (union escape-sequence
                          (intersection char-sequence-without-double-quote
                                        char-sequence-without-backslash))]
   [string-content (repetition 0 +inf.0 string-pattern)]
   [voice-string (concatenation #\" string-content #\")])

  


  ;; The lexer.
  ;; It is taking two arguments: the function to know if there is a silence,
  ;; and the function to define it.
  (define make-lexer
    (lambda ()
      (define (keyword? str)
        (match (string-locale-downcase str)
          ["open"     (token-OPEN)]
          ["square"   (token-SQUARE)]
          ["close"    (token-CLOSE)]
          ["insert"   (token-INSERT)]
          ["after"    (token-AFTER)]
          ["before"   (token-BEFORE)]
          ["select"   (token-SELECT)]
          ["search"   (token-SEARCH)]
          ["top"      (token-TOP)]
          ["bottom"   (token-BOTTOM)]
          ["holder"   (token-HOLDER)]
          ["next"     (token-NEXT)]
          ["previous" (token-PREVIOUS)]
          ["cancel"   (token-CANCEL)]
          ["undo"     (token-UNDO)]
          ["redo"     (token-REDO)]
          ["magic"    (token-MAGIC)]
          ["magic-bash" (token-MAGIC-BASH)]
          ["magic-wrap" (token-MAGIC-WRAP)]
          ["pass"     (token-PASS)]
          ["pass-wrap"  (token-PASS-WRAP)]
          ["again"    (token-AGAIN)]
          ["out"      (token-OUT)]
          ["up"       (token-UP)]
          ["down"       (token-DOWN)]
          ["forward"  (token-FORWARD)]
          ["backward" (token-BACKWARD)]
          ["younger"  (token-YOUNGER)]
          ["older" (token-OLDER)]
          ["voice:-first"    (token-FIRST)]
          ["last"     (token-LAST)]
          ["delete"   (token-DELETE)]
          ["voice:-dedouble-ellipsis" (token-DEDOUBLE-ELLIPSIS)]
          ["put"      (token-PUSH)]
          ["bring"    (token-BRING)]
          ["exchange" (token-EXCHANGE)]
          ["mark"     (token-MARK)]
          ["unmark"   (token-UNMARK)]
          ["copy"     (token-COPY)]
          ["cut"      (token-CUT)]
          ["paste"    (token-PASTE)]
          ["definition" (token-DEFINITION)]
          ["usage"    (token-USAGE)]
          ["hit"      (token-HIT)]
          ["the"      (token-THE)]
          ["second"   (token-SECOND)]
          ["third"    (token-THIRD)]
          ["fourth"   (token-FOURTH)]
          ["fifth"    (token-FIFTH)]
          ["sixth"    (token-SIXTH)]
          ["seventh"  (token-SEVENTH)]
          ["eighth"   (token-EIGHTH)]
          ["nineth"   (token-NINETH)]
          ["tenth"    (token-TENTH)]
          ["zero"     (token-SYMBOL "0")]
          ["one"      (token-SYMBOL "1")]
          ["two"      (token-SYMBOL "2")]
          ["three"    (token-SYMBOL "3")]
          ["four"     (token-SYMBOL "4")]
          ["five"     (token-SYMBOL "5")]
          ["six"      (token-SYMBOL "6")]
          ["seven"    (token-SYMBOL "7")]
          ["eight"    (token-SYMBOL "8")]
          ["nine"     (token-SYMBOL "9")]
          ["ten"      (token-SYMBOL "10")]
          ["this"     (token-THIS)]
          ["that"     (token-THAT)]
          ["'"        (token-SYMBOL "quote")]
          ["`"        (token-SYMBOL "quasiquote")]
          [","        (token-SYMBOL "unquote")]
          [",@"       (token-SYMBOL "unquote-splicing")]
          ["#'"       (token-SYMBOL "quote-syntax")]
          ["#`"       (token-SYMBOL "quasisyntax")]
          ["#,"       (token-SYMBOL "unsyntax")]
          ["#,@"      (token-SYMBOL "unsyntax-splicing")]
          ["enter"    (token-ENTER)]
          ["join"     (token-JOIN)]
          ["indent"   (token-INDENT)]
          ["transpose" (token-TRANSPOSE)]
          ["tag"      (token-TAG)]
          [_          false]))
      (letrec ([voice-lexer
                (lex:lexer
                 [(eof)      (token-DUMMY)]
                 [eol        (token-SILENCE)]
                 [blank      (voice-lexer lex:input-port)]
                 [voice-string (token-STRING lex:lexeme)]
                 [pipe       (token-SYMBOL lex:lexeme)]
                 [symbol     (or (keyword? lex:lexeme) (token-SYMBOL lex:lexeme))])])
        voice-lexer)))


  ;; The parser.
  (define voice-expr-parser
    (lambda (interpreter)
      (yacc:parser
       ;;(debug "voice.table")
       (suppress)
       (error (lambda (token-ok token-name lex:token-value) 
                (raise (list 'parser-error (format "parser error: ~a ~a ~a ~n" token-ok token-name lex:token-value)))))
       
       (tokens empty-token-group token-group)
       (start start)
       (end DUMMY)
       
       
       (grammar
        (start 
         [(start0 start) (raise 'parser-recognition-done-rec)])
        
        (start0
         [(SILENCE)            (void)]
         [(tree)               (interpreter $1)])
        
        (tree
         [(verbN)                (make-Verb $1 false false)]
         [(verbL)                (make-Verb $1 false false)]
         [(verbLW)               (make-Verb $1 false false)]
         [(verbL  location)      (make-Verb $1 $2 false)]
         [(verbLW location)      (make-Verb $1 $2 false)]
         [(verbW  what)          (make-Verb $1 false $2)]
         [(verbLW what)          (make-Verb $1 false $2)]
         [(verbLW location what) (make-Verb $1 $2 $3)])

        ;; Verbs which do not accept any argument
        (verbN
         [(close)    (make-Command $1)]
         [(next)     (make-Command $1)]
         [(previous) (make-Command $1)]
         [(cancel)   (make-Command $1)]
         [(undo)     (make-Command $1)]
         [(redo)     (make-Command $1)]
         [(magic)    (make-Command $1)]
         [(magic-wrap) (make-Command $1)]
         [(pass)     (make-Command $1)]
         [(pass-wrap)  (make-Command $1)]
         [(again)    (make-Command $1)]
         [(up)       (make-Command $1)]
         [(down)     (make-Command $1)]
         [(forward)  (make-Command $1)]
         [(backward) (make-Command $1)]
         [(younger)  (make-Command $1)]
         [(older) (make-Command $1)]
         [(first)    (make-Command $1)]
         [(last)     (make-Command $1)]
         [(delete)   (make-Command $1)]
         [(dedouble-ellipsis) (make-Command $1)]
         [(bring)     (make-Command $1)]
         [(Push)    (make-Command $1)]
         [(exchange) (make-Command $1)]
         [(unmark)   (make-Command $1)]
         [(copy)     (make-Command $1)]
         [(cut)      (make-Command $1)]
         [(paste)    (make-Command $1)]
         [(definition) (make-Command $1)]
         [(usage)    (make-Command $1)]
         [(enter)    (make-Command $1)]
         [(join)     (make-Command $1)]
         [(indent)   (make-Command $1)]
         [(transpose) (make-Command $1)]
         [(object)   (make-Symbol-Cmd $1)])
        
        ;; Verbs which accept only a location, and maybe nothing
        (verbL
         [(insert)   (make-Command $1)])
        
        ;; Verbs which accept only a what, and maybe nothing
        (verbW
         [(magic-bash) (make-Command $1)])
        
        ;; Verbs which accept everything
        (verbLW
         [(open)     (make-Command $1)]
         [(search)   (make-Command $1)]
         [(holder)   (make-Command $1)]
         [(out)      (make-Command $1)]
         [(mark)     (make-Command $1)]
         [(tag)      (make-Command $1)])
        
        
        (open
         [(OPEN)        'Open]
         [(OPEN SQUARE) 'Open-Square])
        
        (close
         [(CLOSE)       'Close])
        
        (insert
         [(INSERT)      'Insert])
        
        (search
         [(SELECT)          'Select]
         [(SEARCH)          'Search-Forward]
         [(SEARCH FORWARD)  'Search-Forward]
         [(SEARCH BACKWARD) 'Search-Backward]
         [(SEARCH TOP)      'Search-Top]
         [(SEARCH BOTTOM)   'Search-Bottom])
        
        (holder
         [(HOLDER)          'Holder-Forward]
         [(HOLDER FORWARD)  'Holder-Forward]
         [(HOLDER BACKWARD) 'Holder-Backward])
        
        (next
         [(NEXT)         'Next]
         [(NEXT hit)     'Next])
        
        (previous
         [(PREVIOUS)     'Previous]
         [(PREVIOUS hit) 'Previous])

        (cancel
         [(CANCEL)       'Cancel])
        
        (undo
         [(UNDO)       'Undo])
        
        (redo
         [(REDO)       'Redo])

        (magic
         [(MAGIC)      'Magic])
        
        (magic-bash
         [(MAGIC-BASH) 'Magic-Bash])
        
        (magic-wrap
         [(MAGIC-WRAP) 'Magic-Wrap])
        
        (pass
         [(PASS)       'Pass])

        (pass-wrap
         [(PASS-WRAP)       'Pass-Wrap])

        (again
         [(AGAIN)      'Again])

        (out
         [(OUT) 'Out])

        (up
         [(UP)  'Up])
        
        (down
         [(DOWN)  'Down])
        
        (forward
         [(FORWARD)     'Forward])
        
        (backward
         [(BACKWARD)    'Backward])

        (younger
         [(YOUNGER)    'First])

        (older
         [(OLDER)    'Last])

        (first
         [(FIRST)    'First])

        (last
         [(LAST)    'Last])

        (delete
         [(DELETE)      'Delete])
        
        (dedouble-ellipsis
         [(DEDOUBLE-ELLIPSIS)      'Dedouble-Ellipsis])
        
        (bring
         [(BRING)        'Bring])
        
        (Push
         [(PUSH)       'Push])

        (exchange
         [(EXCHANGE)    'Exchange])
        
        (mark
         [(MARK)        'Mark])
        
        (unmark
         [(UNMARK)      'UnMark])
        
        (copy
         [(COPY)  'Copy])
        
        (cut
         [(CUT)   'Cut])
        
        (paste
         [(PASTE) 'Paste])
        
        
        (definition
          [(DEFINITION) 'Definition])
        
        (usage
         [(USAGE)      'Usage])
        
        (enter
         [(ENTER)      'Enter])

        (join
         [(JOIN)       'Join])
        
        (indent
         [(INDENT)       'Indent])
        
        (transpose
         [(TRANSPOSE)       'Transpose])

        (tag
         [(TAG)  'Tag])


        
        
        (location
         [(where what) (make-Loc $1 $2)])
        
        (where
         [(AFTER)      (make-After)]
         [(BEFORE)     (make-Before)])
        
        (what
         [()            false]
         [(noun)           (make-WhatN     $1)]
         [(distances noun) (make-WhatDN $1 $2)])

        (distances
         [(second)      2]
         [(THE THIRD)   3]
         [(THE FOURTH)  4]
         [(THE FIFTH)   5]
         [(THE SIXTH)   6]
         [(THE SEVENTH) 7]
         [(THE EIGHTH)  8]
         [(THE NINETH)  9]
         [(THE TENTH)   10])
        
        (second
         [(THE SECOND) (void)]
         [(THE NEXT)       (void)]
         [(THE NEXT hit)   (void)])
        
        (hit
         [(HIT) (void)]
         [(ONE) (void)])
        
        
        (noun
         [(object)     (make-Symbol-Noun $1)]
         [(THE object) (make-The-Symbol  $2)])

        (object
         [(SYMBOL) (string->symbol $1)]
         [(STRING) (string->symbol $1)])
        
        (this
         [(THIS) (void)]
         [(THAT) (void)])))))
  

  ;; This function takes two arguments: 
  ;;  * a function to print message
  ;;  * a function to interpret the abstract syntax tree
  ;; It returns an output-port by which strings can be sent to be analysed by the lexer and the parser.
  (define make-parser
    (lambda (voice-message interpreter)
      (let-values ([(pop push) (make-pipe)])
        (letrec ([parse-thread false]
                 [parse-loop
                  (lambda ()
                    (with-handlers ([list (lambda (exn)
					    (cond
					     [(and (list? exn) (eq? 'parser-error (car exn)))
					      (voice-message (format "Sorry, but I do not understand what you mean. [~a ; parser restarting]" exn))]
					     [else (voice-message (format "Internal Error: ~a [parser restarting]" exn))])
					    (parse-loop))])
				   ((voice-expr-parser interpreter)
				    (lambda ()
				      ((make-lexer) pop)))))]
                 [out-to-parser
                  (lambda (text)
                    (write-string text push))])
	  
          (set! parse-thread (thread parse-loop))
          out-to-parser)))))
