(module cleanup-whitespace mzscheme
  (require (lib "contract.ss")
           (lib "lex.ss" "parser-tools")
           (lib "etc.ss")
           (lib "port.ss")
           (lib "list.ss")
           (only (lib "13.ss" "srfi") string-prefix?)
           "rope.ss"
           "semi-read-syntax/lexer.ss")
  
  
  ;; cleanup-whitespace: rope -> rope
  ;;
  ;; Given a rope with Scheme literals and specials, follows
  ;; standard conventions of removing whitespace around parens.
  ;;
  ;; TODO: rather than return a new rope, it should return a list
  ;; of operations we need to do to clean up the rope.
  
  (define (cleanup-whitespace a-rope)
    (local ((define ip (open-input-rope a-rope))
            (define (next-position-token)
              (plt-lexer ip)))
      (let loop ([pos-tok (next-position-token)]
                 [kill-leading-whitespace? #f]
                 [at-beginning-of-line? #t]
                 [acc '()]
                 [count-deleted-chars 0])
        
        (local ((define tok (position-token-token pos-tok))
                (define start-pos 
                  (- (position-offset (position-token-start-pos pos-tok)) 
                     count-deleted-chars))
                
                (define (leave-preserved kill-leading-whitespace? at-beginning-of-line?)
                  (loop (next-position-token)
                        kill-leading-whitespace?
                        at-beginning-of-line?
                        (cons ((if (string? (token-value tok))
                                   string->rope
                                   special->rope)
                               (token-value tok))
                              acc)
                        count-deleted-chars))
                
                (define (handle-space)
                  (local ((define next-pos-token (next-position-token))
                          (define next-tok (position-token-token next-pos-token))
                          
                          (define footer-cleaner-f
                            (if kill-leading-whitespace?
                                (lambda (s) "")
                                (lambda (s) s))))
                    (cond
                      [at-beginning-of-line?
                       (loop next-pos-token
                             #f
                             #t
                             (cons (string->rope (token-value tok)) acc)
                             count-deleted-chars)]
                      [(member (token-name next-tok) (list 'end 'suffix))
                       (let-values ([(new-str) (truncate-all-but-newlines (token-value tok))])
                         (loop next-pos-token
                               #t
                               at-beginning-of-line?
                               (cons (string->rope new-str) acc)
                               (+ count-deleted-chars 
                                  (string-length-delta new-str (token-value tok)))))]
                      
                      [else
                       (local ((define-values (whitespace)
                                 (trim-white-header (token-value tok)))
                               (define-values (new-whitespace)
                                 (footer-cleaner-f whitespace)))
                         (loop next-pos-token
                               #t
                               (contains-newline? new-whitespace)
                               (cons (string->rope new-whitespace) acc)
                               (+ count-deleted-chars 
                                  (string-length-delta 
                                   new-whitespace (token-value tok)))))])))
                
                (define (handle-atom)
                  (cond
                    [(string-prefix? ";" (token-value tok))
                     (let-values ([(cleaned-str new-markers)
                                   (truncate-white-footer (token-value tok))])
                       (loop (next-position-token)
                             #f
                             #f
                             new-markers
                             (cons (string->rope cleaned-str) acc)
                             (+ count-deleted-chars
                                (string-length-delta cleaned-str
                                                     (token-value tok)))))]
                    [else
                     (leave-preserved #f #f)])))
          (case (token-name tok)
            [(atom)
             (handle-atom)]
            [(special-atom)
             (leave-preserved #f #f)]
            [(quoter-prefix)
             (leave-preserved #t #f)]
            [(prefix)
             (leave-preserved #t #f)]
            [(suffix)
             (leave-preserved #f #f)]
            [(space)
             (handle-space)]
            [(end)
             (apply rope-append* (reverse acc))])))))
  
  
  ;; contains-newline?: string -> boolean
  (define (contains-newline? a-str)
    (and (regexp-match #rx"\n" a-str) #t))
  
  
  ;; trim-white-header: string natural-number (listof natural-number) -> (values string (listof natural-number)
  (define (trim-white-header a-str)
    (regexp-replace #rx"^([ \t]+)[\r\n]" a-str "\\1"))
  
  
  
  ;; truncate-white-footer: string -> string
  ;; Removes whitespace from the end of a string.
  (define (truncate-white-footer a-str)
    (regexp-replace #rx"([ \t]+)$" a-str ""))
  
  
  ;; truncate-all-but-newlines: string -> string
  ;; Eats all but newlines.
  (define (truncate-all-but-newlines a-str)
    (regexp-replace* #rx"([^\n])" a-str ""))
  
  
  ;; string-length-delta: string string -> number
  (define (string-length-delta s1 s2) 
    (- (string-length s2) (string-length s1)))
  
  
  (define positive-number/c (and/c integer? (>=/c 1)))
  
  (provide/contract
   [cleanup-whitespace (rope? . -> . rope?)]))