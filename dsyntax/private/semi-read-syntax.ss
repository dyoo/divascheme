(module semi-read-syntax mzscheme
  (require (lib "lex.ss" "parser-tools")
           (lib "etc.ss")
           (lib "contract.ss")
           (lib "list.ss")
           (only (lib "1.ss" "srfi") take)
           "parse-plt-scheme.ss")
  
  (provide/contract [semi-read/syntax (any/c input-port? . -> . syntax?)])
  
  (define (semi-read/syntax source ip)
    (local
        [(define-values (pipe-ip pipe-op) (make-pipe #f source source))
         (define translation-table (feed-tokens ip pipe-op))]
      (repair-stx (read-syntax source pipe-ip) translation-table)))
  
  
  (define (repair-stx stx translation-table)
    (local [(define (used-to-be-a-dot? stx)
              (and (hash-table-get translation-table (syntax-position stx) #f) #t))
            
            (define (remake-stx datum)
              (datum->syntax-object stx datum stx stx))
            
            (define (repair-list)
              (define (repair-list/fast items unchanged)
                (if (empty? items)
                    stx
                    (let ([sub (repair-stx (first items) translation-table)])
                      (if (eq? sub (first items))
                          (repair-list/fast (rest items) (add1 unchanged))
                          (repair-list/slow (rest items)
                                            (cons sub (reverse
                                                       (take (syntax->list stx) unchanged))))))))
              
              (define (repair-list/slow items result)
                (if (empty? items)
                    (remake-stx (reverse result))
                    (repair-list/slow (rest items)
                                      (cons (repair-stx (first items) translation-table)
                                            result))))
              
              (repair-list/fast (syntax->list stx) 0))]
      
      (syntax-case stx ()
        [(item ...)
         (repair-list)]
        [#(item ...)
         (remake-stx (list->vector
                      (map (lambda (stx) (repair-stx stx translation-table))
                           (vector->list (syntax-e stx)))))]
        [(first . rest)
         (remake-stx (cons (repair-stx #'first translation-table)
                           (repair-stx #'rest translation-table)))]
        [_ (and (symbol? (syntax-e stx))
                (used-to-be-a-dot? stx))
           (datum->syntax-object stx '|.| stx stx)]
        [else stx])))
  
  
  
  ;; feed-tokens: input-port output-port -> (hash-table-of number string)
  ;;
  ;; Writes out tokens from the input-port into the output-port, translating
  ;; troublesome ones (dots, comments) into a form that syntax-object->datum will
  ;; be happy with.
  ;; 
  ;; Returns a hash table mapping positions to places where we had to do this
  ;; patching to the contents of the input port.
  (define (feed-tokens from-ip to-op)
    (define ht (make-hash-table 'equal))
    (let loop ()
      (local [(define pos-token (plt-lexer from-ip))
              (define token (position-token-token pos-token))
              (define type (token-name token))
              (define val (token-value token))]
        (cond
          [(eq? type 'end)
           (close-output-port to-op)]
          [else (case type
                  [(atom)
                   (cond
                     [(equal? val ".")
                      (hash-table-put! ht (position-offset (position-token-start-pos pos-token)) val)
                      (display "X" to-op)]
                     [else
                      (display val to-op)])]
                  [(space)
                   (display val to-op)]
                  [(special-atom)
                   (display val to-op)]
                  [(prefix)
                   (display val to-op)]
                  [(suffix)
                   (display val to-op)]
                  [(quoter-prefix)
                   (display val to-op)])
                (loop)])))
    ht))