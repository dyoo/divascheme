(module text-rope-mixin mzscheme
  
  ;; Provides a text% mixin that adds get-rope and set-rope methods.
  (require (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "etc.ss")
           (lib "lex.ss" "parser-tools")
           (lib "contract.ss")
           (lib "plt-match.ss")
           "rope.ss")
  
  
  
  (define (text-rope-mixin super%)
    (class super%
      (inherit begin-edit-sequence
               end-edit-sequence
               get-start-position
               erase
               insert)
      (define rope rope-empty)
      
      ;; get-rope: -> rope
      ;; Returns the rope reflected by the text.
      (define/public (get-rope)
        rope)
      
      ;; Arbitrary constant for rebalancing the rope.
      (define threshold-for-rebalancing 25)
      
      ;; On changes to the text, we must repair the rope:
      (define/augment (after-delete start len)
        (set! rope (rope-append (subrope rope 0 start)
                                (subrope rope (+ start len))))
        (inner #f after-delete start len))
      
      
      (define/augment (after-insert start len)
        (set! rope (rope-append
                    (rope-append (subrope rope 0 start)
                                 (read-subrope-in-text this start len))
                    (subrope rope start)))
        (when (> (rope-depth rope)
                 threshold-for-rebalancing)
          (set! rope (rope-balance rope)))
        (inner #f after-insert start len))
      
      (super-new)))
  
  
  ;; insert-rope-in-text: text% rope -> void
  ;; Adds the elements of the ropes (characters and snips) into the text.
  ;; We must take special care to copy snips, since snips have a one-to-one
  ;; relationship with an editor.
  (define (insert-rope-in-text a-text a-rope)
    (rope-fold/leaves
     (lambda (snip _)
       (match snip
         [(struct rope:string (s))
          (send a-text insert s (send a-text get-start-position) 'same #f)]
         [(struct rope:special (s))
          (send a-text insert (send s copy) (send a-text get-start-position) 'same #f)]))
     #f
     a-rope))
  
  
  
  ;; Simple lexer: pulls ropes from a port. Assumes specials
  ;; should be unboxed.
  (define mylexer
    (lexer
     [(repetition 0 +inf.0 any-char)
      (string->rope lexeme)]
     [(special)
      (special->rope (unbox lexeme))]
     [(eof) eof]))
  
  ;; read-subrope-in-text: text% number number -> rope
  (define (read-subrope-in-text a-text start len)
    (local
        ((define ip (open-input-text-editor a-text start (+ start len)
                                            (lambda (snip) (box snip))
                                            #f #f)))
      (let loop ([inserted-rope (string->rope "")]
                 [next-chunk (mylexer ip)])
        (cond
          [(eof-object? next-chunk)
           inserted-rope]
          [else
           (loop (rope-append inserted-rope next-chunk)
                 (mylexer ip))]))))
  
  
  ;; open-file is just a testing function: ignore
  (define (open-file filename)
    (local ((define f (make-object frame% "test" #f 400 500))
            (define t (make-object (text-rope-mixin text%)))
            (define c (make-object editor-canvas% f t '(no-hscroll))))
      (send t load-file filename)
      (send t auto-wrap #t)
      (send f show #t)
      t))
  
  
  
  
  
  (provide/contract [text-rope-mixin (class? . -> . class?)]
                    [read-subrope-in-text ((is-a?/c text%)
                                           natural-number/c
                                           natural-number/c
                                           . -> .
                                           rope?)]
                    [insert-rope-in-text ((is-a?/c text%)
                                          rope?
                                          . -> .
                                          any)]))