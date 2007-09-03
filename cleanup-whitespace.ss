(module cleanup-whitespace mzscheme
  (require (lib "contract.ss")
           (lib "lex.ss" "parser-tools")
           (lib "etc.ss")
           "rope.ss"
           "semi-read-syntax/lexer.ss")
  
  
  ;; cleanup-whitespace: rope -> rope
  ;; Given a rope with Scheme literals and specials, follows
  ;; standard conventions of removing whitespace around parens.
  (define (cleanup-whitespace a-rope)
    (local ((define ip (open-input-rope a-rope)))
      (rope-balance
       (let loop ([pos-tok (plt-lexer ip)]
                  [kill-leading-whitespace? #t])
         (local ((define tok (position-token-token pos-tok))
                 
                 (define (leave-preserved kill-leading-whitespace?)
                   (rope-append
                    ((if (string? (token-value tok)) string->rope special->rope)
                     (token-value tok))
                    (loop (plt-lexer ip)
                          kill-leading-whitespace?)))
                 
                 (define (handle-space)
                   (rope-append (string->rope " ")
                                (loop (plt-lexer ip) #t))))
           
           (case (token-name tok)
             [(atom)
              (leave-preserved #f)]
             [(special-atom)
              (leave-preserved #f)]
             [(quoter-prefix)
              (leave-preserved #t)]
             [(prefix)
              (leave-preserved #t)]
             [(suffix )
              (leave-preserved #f)]
             [(space)
              (handle-space)]
             [(end) (string->rope "")]))))))
  
  
  (provide/contract [cleanup-whitespace (rope? . -> . rope?)]))