(module cleanup-whitespace mzscheme
  (require (lib "contract.ss")
           (lib "lex.ss" "parser-tools")
           (lib "etc.ss")
           "rope.ss"
           (lib "plt-match.ss")
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
                 
                 (define (trim-white-footer a-space)
                   (cond
                     [(regexp-match "[\r\n]" a-space)
                      (regexp-replace "[ ]+$" a-space "")]
                     [else
                      (regexp-replace "[ ]+$" a-space " ")]))
                 
                 (define (truncate-white-footer a-space)
                   (regexp-replace "[ ]+$" a-space ""))
                 
                 (define (handle-space)
                   (local ((define next-pos-tok (plt-lexer ip))
                           (define next-tok (position-token-token next-pos-tok))
                           (define whitespace
                             (regexp-replace* "([ \t]*)([\r\n]+)"
                                              (token-value tok)
                                              "\\2")))
                     ;; FIXME: handle newlines properly
                     (cond
                       [(member (token-name next-tok) (list 'end 'suffix))
                        (loop next-pos-tok #t)]
                       [kill-leading-whitespace?
                        (rope-append (string->rope
                                      (truncate-white-footer whitespace))
                                     (loop next-pos-tok #t))]
                       [else
                        (rope-append (string->rope
                                      (trim-white-footer whitespace))
                                     (loop next-pos-tok #t))]))))
           
           (case (token-name tok)
             [(atom)
              (leave-preserved #f)]
             [(special-atom)
              (cond [(annotation? (token-value tok))
                     (leave-preserved kill-leading-whitespace?)]
                    [else
                     (leave-preserved #f)])]
             [(quoter-prefix)
              (leave-preserved #t)]
             [(prefix)
              (leave-preserved #t)]
             [(suffix)
              (leave-preserved #f)]
             [(space)
              (handle-space)]
             [(end) (string->rope "")]))))))
  
  
  (define-struct annotation (label))
  
  (define (insert-annotate a-rope label index)
    (cond [(< index (rope-length a-rope))
           (rope-append
            (rope-append (subrope a-rope 0 index)
                         (special->rope (make-annotation label)))
            (subrope a-rope index))]))
  
  (define (strip-annotations a-rope)
    (match a-rope
      [(struct rope:string (s))
       a-rope]
      [(struct rope:special (s))
       (cond [(annotation? s)
              (string->rope "")]
             [else a-rope])]
      [(struct rope:concat (l r len))
       (local ((define new-left (strip-annotations l))
               (define new-right (strip-annotations r)))
         (cond
           [(and (eq? new-left l)
                 (eq? new-right r))
            a-rope]
           [else
            (make-rope:concat
             new-left new-right
             (+ (rope-length new-left)
                (rope-length new-right)))]))]))
  
  
  
  
  (provide/contract [cleanup-whitespace (rope? . -> . rope?)]))