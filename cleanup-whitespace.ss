(module cleanup-whitespace mzscheme
  (require (lib "contract.ss")
           (lib "lex.ss" "parser-tools")
           (lib "etc.ss")
           (lib "port.ss")
           (lib "list.ss")
           "rope.ss"
           "semi-read-syntax/lexer.ss")
  
  
  ;; cleanup-whitespace: rope (listof natural-number) -> rope
  ;; Given a rope with Scheme literals and specials, follows
  ;; standard conventions of removing whitespace around parens.
  ;; Positional markers within the rope will be shifted according to deleted whitespace.
  (define (cleanup-whitespace a-rope start-pos markers)
    (local ((define ip (relocate-input-port
                        (open-input-rope a-rope)
                        #f #f (add1 start-pos))))
      (let loop ([pos-tok (plt-lexer ip)]
                 [kill-leading-whitespace? #t]
                 [markers (map add1 markers)]
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
                  (local ((define next-pos-token (plt-lexer ip))
                          (define start-pos (position-offset (position-token-start-pos next-pos-token)))
                          (define next-tok (position-token-token next-pos-token))
                          
                          (define footer-cleaner-f
                            (if kill-leading-whitespace?
                                truncate-white-footer
                                trim-white-footer)))
                    (cond
                      [(member (token-name next-tok) (list 'end 'suffix))
                       (loop next-pos-token #t
                             (truncate-all (token-value tok) start-pos markers)
                             acc)]
                      [else
                       (local ((define-values (whitespace new-markers-1)
                                 (trim-white-header (token-value tok) start-pos markers))
                               (define-values (new-whitespace new-markers-2)
                                 (footer-cleaner-f whitespace start-pos new-markers-1)))
                         (loop next-pos-token #t
                               new-markers-2
                               (rope-append acc (string->rope new-whitespace))))]))))
          
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
             (values acc (map sub1 markers))])))))
  
  
  ;; trim-white-header: string natural-number (listof natural-number) -> (values string (listof natural-number)
  (define (trim-white-header a-str start-index markers)
    (let loop ([a-str a-str]
               [markers markers])
      (let-values ([(new-str markers)
                    (adjust-markers "([ \t]+)[\r\n]"
                                    1
                                    a-str
                                    start-index
                                    markers)])
        (cond
          [(string=? new-str a-str)
           (values new-str markers)]
          [else
           (loop new-str markers)]))))
  
  
  
  ;; trim-white-footer: string -> string
  ;; Removes all but one whitespace from the end of a string.
  (define (trim-white-footer a-str start-index markers)
    (cond
      [(regexp-match "[\r\n]" a-str)
       (values (regexp-replace "[ ]+$" a-str "")
               markers)]
      [else
       (values (regexp-replace "[ ]+$" a-str " ")
               markers)]))
  
  
  ;; truncate-white-footer: string -> string
  ;; Removes whitespace from the end of a string.
  (define (truncate-white-footer a-space start-index markers)
    (values (regexp-replace "[ ]+$" a-space "")
            markers))
  
  
  ;; truncate-all: string natural-number (listof natural-number) -> (listof natural-number)
  (define (truncate-all a-str start-index markers)
    markers)
  
  
  ;; adjust-markers: regex number string number (listof number) -> (values string (listof number))
  ;; Does the hard work in dropping the whitespace and recomputing the markers.
  (define (adjust-markers deleting-regex nth-group a-str start-index markers)
    (cond
      [(regexp-match-positions deleting-regex a-str)
       =>
       (lambda (matches)
         (local ((define-values (start end)
                   (values (car (list-ref matches nth-group))
                           (cdr (list-ref matches nth-group)))))
           (let loop ([markers markers]
                      [i start])
             (cond
               [(= i end)
                (values (string-append
                         (substring a-str 0 start)
                         (substring a-str end))
                        markers)]
               [else
                (loop (decrease> (+ start-index i) markers) (add1 i))]))))]
      [else
       (values a-str markers)]))
  
  
  ;; decrease>: number number -> number
  ;; If the character we're deleting affects the marker, shift all the markers down by one.
  (define (decrease> index markers)
    (map (lambda (m) (if (> m index) (sub1 m) m)) markers))
  
  
  
  (define positive-number/c (and/c integer? (>=/c 1)))
  
  (provide/contract [cleanup-whitespace ((rope? natural-number/c (listof natural-number/c))
                                         . ->* .
                                         (rope? (listof natural-number/c)))]))