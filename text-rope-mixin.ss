(module text-rope-mixin mzscheme
  
  ;; Provides a text% mixin that adds get-rope and set-rope methods.
  (require (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "etc.ss")
           (lib "lex.ss" "parser-tools")
           (lib "contract.ss")
           (only (lib "13.ss" "srfi") string-fold)
           "rope.ss")
  
  
  
  (define (text-rope-mixin super%)
    (class super%
      (inherit begin-edit-sequence
               end-edit-sequence
               get-start-position
               erase
               insert)
      (define rope (string->rope ""))
      
      ;; get-rope: -> rope
      ;; Returns the rope reflected by the text.
      (define/public (get-rope)
        rope)
      
      
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
        (inner #f after-insert start len))
      
      (super-new)))
  
  
  ;; insert-rope-in-text: text% rope -> void
  ;; Adds the elements of the ropes (characters and snips) into the text.
  ;; We must take special care to copy snips, since snips have a one-to-one
  ;; relationship with an editor.
  (define (insert-rope-in-text a-text a-rope)
    (rope-fold/leaves
     (lambda (snip _)
       (cond
         [(string? snip)
          (send a-text insert snip (send a-text get-start-position) 'same #f)]
         [else
          (send a-text insert (send snip copy) (send a-text get-start-position) 'same #f)]))
     #f
     a-rope))
  
  
  
  ;; read-subrope-in-text: text% number number -> rope
  (define (read-subrope-in-text a-text start len)
    (local
        (
         ;; Simple lexer: pulls ropes from a port. Assumes specials
         ;; should be unboxed.
         (define mylexer
           (lexer
            [(repetition 0 +inf.0 any-char)
             (string->rope lexeme)]
            [(special)
             (special->rope (unbox lexeme))]
            [(eof) eof]))
         
         (define ip (open-input-text-editor a-text start (+ start len)
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
  
  
  ;; rope-count-whitespace: rope -> natural-number
  ;; Returns the number of whitespace characters in a rope.
  (define (rope-count-whitespace a-rope)
    (local ((define (f string-or-special current-count)
              (cond
                [(string? string-or-special)
                 (+ current-count
                    (count-whitespace-in-string string-or-special))]
                [else
                 current-count]))
            
            (define (count-whitespace-in-string a-str)
              (string-fold (lambda (ch acc)
                             (case ch
                               [(#\space #\tab #\newline #\return)
                                (add1 acc)]
                               [else
                                acc]))
                           0
                           a-str)))
      (rope-fold/leaves f 0 a-rope)))
  
  (provide/contract [text-rope-mixin (class? . -> . class?)]
                    [read-subrope-in-text ((is-a?/c text%)
                                           natural-number/c
                                           natural-number/c
                                           . -> .
                                           rope?)]
                    [insert-rope-in-text ((is-a?/c text%)
                                          rope?
                                          . -> .
                                          any)]
                    [rope-count-whitespace (rope? . -> . natural-number/c)]))