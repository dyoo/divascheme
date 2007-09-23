(module cleanup-whitespace mzscheme
  (require (lib "contract.ss")
           (lib "lex.ss" "parser-tools")
           (lib "etc.ss")
           (lib "port.ss")
           (lib "list.ss")
           (only (lib "13.ss" "srfi") string-prefix?)
           "rope.ss"
           "semi-read-syntax/lexer.ss")
  
  
  ;; cleanup-whitespace: rope (listof natural-number) -> rope
  ;; Given a rope with Scheme literals and specials, follows
  ;; standard conventions of removing whitespace around parens.
  ;; Positional markers within the rope will be shifted according to deleted whitespace.
  (define (cleanup-whitespace a-rope at-index markers)
    (local ((define ip (relocate-input-port
                        (open-input-rope a-rope)
                        #f #f
                        (add1 at-index)))
            (define (next-position-token)
              (plt-lexer ip)))
      (let loop ([pos-tok (next-position-token)]
                 [kill-leading-whitespace? #t]
                 [markers (map add1 markers)]
                 [acc (string->rope "")]
                 [count-deleted-chars 0])
        (local ((define tok (position-token-token pos-tok))
                (define start-pos 
                  (- (position-offset (position-token-start-pos pos-tok)) 
                     count-deleted-chars))
                
                (define (leave-preserved kill-leading-whitespace?)
                  (loop (next-position-token)
                        kill-leading-whitespace?
                        markers
                        (rope-append acc
                                     ((if (string? (token-value tok))
                                          string->rope
                                          special->rope)
                                      (token-value tok)))
                        count-deleted-chars))
                
                (define (handle-space)
                  (local ((define next-pos-token (next-position-token))
                          (define next-tok (position-token-token next-pos-token))
                          
                          (define footer-cleaner-f
                            (if kill-leading-whitespace?
                                truncate-white-footer
                                trim-white-footer)))
                    (cond
                      [(member (token-name next-tok) (list 'end 'suffix))
                       (let-values ([(new-str new-markers)
                                     (truncate-all-but-newlines 
                                      (token-value tok)
                                      (- (position-offset (position-token-start-pos pos-tok))
                                         count-deleted-chars) 
                                      markers)])
                         (loop next-pos-token #t
                               new-markers
                               (rope-append acc (string->rope new-str))
                               (+ count-deleted-chars 
                                  (string-length-delta new-str (token-value tok)))))]
                      
                      [else
                       (local ((define-values (whitespace new-markers-1)
                                 (trim-white-header (token-value tok) start-pos markers))
                               (define-values (new-whitespace new-markers-2)
                                 (footer-cleaner-f whitespace start-pos new-markers-1)))
                         (loop next-pos-token #t
                               new-markers-2
                               (rope-append acc (string->rope new-whitespace))
                               (+ count-deleted-chars 
                                  (string-length-delta 
                                   new-whitespace (token-value tok)))))])))
                
                (define (handle-atom)
                  (cond
                    [(string-prefix? ";" (token-value tok))
                     (let-values ([(cleaned-str new-markers)
                                   (truncate-white-footer
                                    (token-value tok) start-pos markers)])
                       (loop (next-position-token) #f new-markers
                             (rope-append acc (string->rope cleaned-str))
                             (+ count-deleted-chars
                                (string-length-delta cleaned-str
                                                     (token-value tok)))))]
                    [else
                     (leave-preserved #f)])))
          
          (case (token-name tok)
            [(atom)
             (handle-atom)]
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
      (let-values ([(new-str new-markers)
                    (regex-delete-and-adjust #rx"([ \t]+)[\r\n]"
                                             a-str
                                             start-index
                                             markers)])
        (cond
          [(string=? new-str a-str)
           (values new-str new-markers)]
          [else
           (loop new-str new-markers)]))))
  
  
  ;; trim-white-footer: string -> string
  ;; Removes all but one whitespace from the end of a string.
  (define (trim-white-footer a-str start-index markers)
    (cond
      [(regexp-match #rx"[\r\n]" a-str)
       (regex-delete-and-adjust #rx"([ \t]+)$" a-str start-index markers)]
      [else
       (regex-delete-and-adjust #rx"[ \t]([ \t]*)$" a-str start-index markers)]))
  
  
  ;; truncate-white-footer: string -> string
  ;; Removes whitespace from the end of a string.
  (define (truncate-white-footer a-str start-index markers)
    (regex-delete-and-adjust #rx"([ \t]+)$" a-str start-index markers))
  
  
  ;; truncate-all: string natural-number (listof natural-number) -> (listof string natural-number)
  (define (truncate-all-but-newlines a-str start-index markers)
    (let-values ([(new-str new-markers)
                  (regex-delete-and-adjust* #rx"([^\n]+)" a-str start-index markers)])
      (values new-str new-markers)))
  
  
  ;; regex-delete-and-adjust: regex string number (listof number) -> (values string (listof number)) 
  ;; Does the hard work in dropping the whitespace and recomputing the markers.
  (define (regex-delete-and-adjust deleting-regex a-str at-index markers)
    (cond
      [(regexp-match-positions deleting-regex a-str)
       =>
       (lambda (matches)
         (local ((define-values (start end)
                   (values (car (second matches))
                           (cdr (second matches)))))
           (values (string-append (substring a-str 0 start)
                                  (substring a-str end))
                   (adjust-markers/delete markers (+ at-index start)
                                          (- end start)))
           (let loop ([markers markers]
                      [i start])
             (cond
               [(= i end)
                (values (string-append
                         (substring a-str 0 start)
                         (substring a-str end))
                        markers)]
               [else
                (loop (decrease> (+ at-index start) markers)
                      (add1 i))]))))]
      [else
       (values a-str markers)]))
  
  
  ;; Adjusts the markers in response to a deletion. 
  (define (adjust-markers/delete markers delete-start length)
    (let loop ([markers markers]
               [i 0])
      (cond
        [(= i length)
         markers]
        [else
         (loop (decrease> delete-start markers)
               (add1 i))])))
  
  
  ;; regex-delete-and-adjust*: regex string natural-number (listof natural-number) -> (values string (listof natural-number)) 
  ;; Applies regex-delete-and-adjust till we hit a fixed point. 
  (define (regex-delete-and-adjust* regex a-str start-index markers)
    (let loop ([a-str a-str]
               [markers markers])
      (let-values ([(new-str new-markers)
                    (regex-delete-and-adjust
                     regex a-str start-index markers)])
        (cond
          [(string=? new-str a-str)
           (values new-str new-markers)]
          [else
           (loop new-str new-markers)]))))
  
  
  
  ;; decrease>: number number -> number
  ;; If the character we're deleting affects the marker, shift all the markers down by one.
  (define (decrease> index markers)
    (map (lambda (m)
           (if (> m index)
               (max (sub1 m) 1)
               m))
         markers))
  
  
  (define (string-length-delta s1 s2) 
    (- (string-length s2) (string-length s1)))
  
  
  (define positive-number/c (and/c integer? (>=/c 1)))
  
  (provide/contract
   [cleanup-whitespace ((rope? natural-number/c (listof natural-number/c))
                        . ->* .
                        (rope? (listof natural-number/c)))]))