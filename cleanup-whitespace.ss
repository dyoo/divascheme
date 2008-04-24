(module cleanup-whitespace mzscheme
  (require (lib "contract.ss")
           (lib "lex.ss" "parser-tools")
           (lib "etc.ss")
           (lib "port.ss")
           (lib "list.ss")
           (lib "class.ss")
           (only (lib "13.ss" "srfi") string-prefix?)
           "rope.ss"
           "semi-read-syntax/lexer.ss")
  
  
  ;; (make-del number number) represents a deletion operation.
  (define-struct deletion (offset len) #f)
  
  
  
  ;; cleanup-whitespace: rope -> rope
  ;;
  ;; Given a rope with Scheme literals and specials, follows
  ;; standard conventions of removing whitespace around parens.
  ;;
  (define (cleanup-whitespace a-rope)
    (let ([deletions (cleanup-whitespace-operations a-rope)])
      (foldl apply-deletion a-rope deletions)))
  
  
  ;; compute-cleanup-deletions: a-rope -> (listof deletion)
  ;; Computes a list of deletions we need to do to clean up this string.
  (define (cleanup-whitespace-operations a-rope)
    (local ((define ip (open-input-rope a-rope))
            (define (next-position-token)
              (plt-lexer ip)))
      
      (let loop ([pos-tok (next-position-token)]
                 [kill-leading-whitespace? #f]
                 [at-beginning-of-line? #t]
                 [acc '()]
                 [offset 0])
        
        (define (tok) (position-token-token pos-tok))
        
        (define (tok-length) 
          (cond
            [(string? (token-value (tok)))
             (string-length (token-value (tok)))]
            [else
             (send (token-value (tok)) get-count)]))
        
        
        
        (define (leave-preserved kill-leading-whitespace? at-beginning-of-line?)
          (loop (next-position-token)
                kill-leading-whitespace?
                at-beginning-of-line?
                acc
                (+ offset (tok-length))))
        
        (define (handle-space)
          (local ((define next-pos-token (next-position-token))
                  (define next-tok (position-token-token next-pos-token))
                  (define footer-cleaner-f
                    (if kill-leading-whitespace?
                        truncate-all-but-newlines
                        identity)))
            (cond
              [at-beginning-of-line?
               (loop next-pos-token
                     #f
                     #t
                     acc
                     (+ offset (tok-length)))]
              [(member (token-name next-tok) (list 'end 'suffix))
               (let ([new-str (truncate-all-but-newlines (token-value (tok)))])
                 (loop next-pos-token
                       #t
                       at-beginning-of-line?
                       (accumulate (make-deletion offset (string-length-delta (token-value (tok)) new-str))
                                   acc a-rope pos-tok)
                       (+ offset (tok-length))))]
              [else
               (local ((define-values (whitespace)
                         (trim-white-header (token-value (tok))))
                       (define-values (new-whitespace)
                         (footer-cleaner-f whitespace)))
                 (loop next-pos-token
                       #t
                       (contains-newline? new-whitespace)
                       (accumulate
                        (make-deletion offset (string-length-delta (token-value (tok)) new-whitespace))
                        acc a-rope pos-tok)
                       (+ offset (tok-length))))])))
        
        (define (handle-atom)
          (cond
            #;[(string-prefix? ";" (token-value (tok)))
               (let* ([cleaned-str (truncate-white-footer (token-value (tok)))]
                      [delta (string-length-delta (token-value (tok)) cleaned-str)])
                 (loop (next-position-token)
                       #f
                       #f
                       (accumulate
                        (make-deletion (- (+ offset (string-length (token-value (tok)))) delta)
                                       delta)
                        acc a-rope pos-tok)
                       (+ offset (tok-length))))]
            [else
             (leave-preserved #f #f)]))
        (case (token-name (tok))
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
           acc]))))
  
  
  ;; accumulate: deletion (listof deletion) -> (listof deletion)
  ;; Add the deletion operation in, as long as it has an effect.
  (define (accumulate a-deletion acc a-rope tok)
    (cond
      [(= (deletion-len a-deletion) 0)
       acc]
      [else
       (cons a-deletion acc)]))
  
  
  ;; rope-delete: rope number -> rope
  (define (rope-delete a-rope offset len)
    (rope-append (subrope a-rope 0 offset)
                 (subrope a-rope (+ offset len))))
  
  ;; apply-deletion: rope deletion -> rope
  (define (apply-deletion a-deletion a-rope)
    (rope-delete a-rope (deletion-offset a-deletion) (deletion-len a-deletion)))
  
  
  
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
    (abs (- (string-length s2) (string-length s1))))
  
  
  (define positive-number/c (and/c integer? (>=/c 1)))
  
  (provide/contract
   [struct deletion ([offset natural-number/c]
                     [len natural-number/c])]
   [cleanup-whitespace (rope? . -> . rope?)]
   [cleanup-whitespace-operations (rope? . -> . (listof deletion?))]))