(module semi-read-syntax mzscheme
  (require (lib "lex.ss" "parser-tools")
           (lib "etc.ss")
           (lib "contract.ss")
           (lib "list.ss")
           (lib "42.ss" "srfi")
           (only (lib "1.ss" "srfi") take)
           (only (lib "13.ss" "srfi") string-prefix?)
           "lexer.ss")
  
  (provide/contract [semi-read-syntax-list (any/c input-port? . -> . (listof syntax?))])
  
  
  (define (semi-read-syntax-list source ip)
    (local
        [(define (make-counting-pipe)
           (let-values ([(ip op) (make-pipe #f source source)])
             (port-count-lines! ip)
             (values ip op)))
         (define-values (pipe-ip pipe-op) (make-counting-pipe))
         (define translation-table (feed-tokens ip pipe-op))]
      (let loop ([stx (read-syntax source pipe-ip)])
        (cond [(eof-object? stx) '()]
              [else
               (cons (repair-stx stx translation-table)
                     (loop (read-syntax source pipe-ip)))]))))
  
  
  
  (define (repair-stx stx translation-table)
    
    (local [(define (was-translated? stx)
              (and (hash-table-get translation-table (syntax-position stx) #f) #t))
            
            (define (untranslate stx translation-table)
              (remake-stx
               (string->symbol (hash-table-get translation-table (syntax-position stx)))))
            
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
      
      [let ((result (syntax-case stx ()
                      [(quote-1 (quote-2 datum))
                       (was-translated? stx)
                       (remake-stx (list (repair-stx #'datum translation-table)))]
                      
                      [(item ...)
                       (repair-list)]
                      
                      [#(item ...)
                       (remake-stx (list->vector
                                    (map (lambda (stx) (repair-stx stx translation-table))
                                         (vector->list (syntax-e stx)))))]
                      [(first . rest)
                       (remake-stx (cons (repair-stx #'first translation-table)
                                         (repair-stx #'rest translation-table)))]
                      [_ (and (symbol? (syntax-e stx)) (was-translated? stx))
                         (untranslate stx translation-table)]
                      
                      [else stx])))
        result]))
  
  
  
  
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
                     [(string-prefix? ";" val)
                      (hash-table-put! ht (position-offset (position-token-start-pos pos-token)) val)
                      (do-ec (:range x (string-length val))
                             (display "X" to-op))]
                     [else
                      (display val to-op)])]
                  [(space)
                   (display val to-op)]
                  
                  [(special-atom)
                   (display val to-op)]
                  
                  [(prefix quoter-prefix)
                   (cond
                     [(string-prefix? "#;" val)
                      (hash-table-put! ht (position-offset (position-token-start-pos pos-token)) val)
                      (display "''" to-op)
                      (display (substring val 2) to-op)]
                     [else
                      (display val to-op)])]
                  
                  [(suffix)
                   (display val to-op)])
                (loop)])))
    ht))