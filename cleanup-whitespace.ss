(module cleanup-whitespace mzscheme
  (require (lib "contract.ss")
           (lib "lex.ss" "parser-tools")
           (lib "etc.ss")
           "rope.ss"
           "semi-read-syntax/lexer.ss")
  
  
  ;; cleanup-whitespace: rope (listof natural-number) -> rope
  ;; Given a rope with Scheme literals and specials, follows
  ;; standard conventions of removing whitespace around parens.
  ;; Markers within the rope will be shifted according to deleted whitespace.
  (define (cleanup-whitespace a-rope markers)
    (local ((define ip (open-input-rope a-rope)))
      (let loop ([pos-tok (plt-lexer ip)]
                 [kill-leading-whitespace? #t]
                 [markers markers]
                 [acc (string->rope "")])
        (local ((define tok (position-token-token pos-tok))
                
                (define (leave-preserved kill-leading-whitespace?)
                  (loop (plt-lexer ip)
                        kill-leading-whitespace?
                        markers
                        (rope-append acc
                                     ((if (string? (token-value tok))
                                          string->rope
                                          special->rope)
                                      (token-value tok)))))
                
                (define (handle-space)
                  (local ((define next-pos-tok (plt-lexer ip))
                          (define next-tok (position-token-token next-pos-tok))
                          (define whitespace
                            (regexp-replace* "([ \t]*)([\r\n]+)"
                                             (token-value tok)
                                             "\\2")))
                    (cond
                      [(member (token-name next-tok) (list 'end 'suffix))
                       (loop next-pos-tok #t markers acc)]
                      [kill-leading-whitespace?
                       (loop next-pos-tok #t markers
                             (rope-append acc (string->rope (truncate-white-footer whitespace))))]
                      [else
                       (loop next-pos-tok #t markers
                             (rope-append acc (string->rope (trim-white-footer whitespace))))]))))
          
          (case (token-name tok)
            [(atom)
             (leave-preserved #f)]
            [(special-atom)
             (leave-preserved #f)]
            [(quoter-prefix)
             (leave-preserved #t)]
            [(prefix)
             (leave-preserved #t)]
            [(suffix)
             (leave-preserved #f)]
            [(space)
             (handle-space)]
            [(end)
             (values acc markers)])))))
  
  ;; trim-white-footer: string -> string
  ;; Removes all but one whitespace from the end of a string.
  (define (trim-white-footer a-space)
    (cond
      [(regexp-match "[\r\n]" a-space)
       (regexp-replace "[ ]+$" a-space "")]
      [else
       (regexp-replace "[ ]+$" a-space " ")]))
  
  
  ;; truncate-white-footer: string -> string
  ;; Removes whitespace from the end of a string.
  (define (truncate-white-footer a-space)
    (regexp-replace "[ ]+$" a-space ""))
  
  
  (provide/contract [cleanup-whitespace ((rope? (listof natural-number/c))
                                         . ->* .
                                         (rope? (listof natural-number/c)))]))