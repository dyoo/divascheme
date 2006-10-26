(module dot-processing mzscheme
  
  (require (lib "list.ss")
           (only (lib "1.ss" "srfi") take)
           "utilities.ss")
  

  (provide parse-syntax/dot)

  (define delimiters "][\t\n \",'`;(){}" )
  (define dot-re
    (regexp (format "([~a])\\.([~a])" delimiters delimiters)))
  
  ;; parse-syntax/dot: string -> (listof syntax)
  (define (parse-syntax/dot text)
    (define munged-text 
      (regexp-replace* dot-re text "\\1~\\2"))
    (print-mem*
     'parse-syntax/dot
     (map (lambda (stx) (repair-stx stx text))
          (string->syntax-list munged-text))))
  
  
  (define (repair-stx stx text)
    (define (used-to-be-a-dot? stx)
      (and
       (= 1 (syntax-span stx))
       (char=? #\. (string-ref text (sub1 (syntax-position stx))))))

    (define (remake-stx datum)
      (datum->syntax-object stx datum stx stx))
    
    
    (define (repair-list)
      (define (repair-list/fast items unchanged)
        (if (empty? items)
            stx
            (let ([sub (repair-stx (first items) text)])
              (if (eq? sub (first items))
                  (repair-list/fast (rest items) (add1 unchanged))
                  (repair-list/slow (rest items)
                                    (cons sub (reverse
                                               (take (syntax->list stx) unchanged))))))))
      
      (define (repair-list/slow items result)
        (if (empty? items)
            (remake-stx (reverse result))
            (repair-list/slow (rest items)
                              (cons (repair-stx (first items) text)
                                    result))))
      
      (repair-list/fast (syntax->list stx) 0))
    

    
    (define (repair-nonsymbolic-datum)
      (let ([result (read
                     (open-input-string
                      (substring text
                                 (sub1 (syntax-position stx))
                                 (+ -1 (syntax-position stx)
                                    (syntax-span stx)))))])
        (if (equal? result (syntax-e stx))
            stx
            (remake-stx result))))
    
    
    (syntax-case stx ()
      [(item ...)
       (repair-list)]
      
      [#(item ...)
       (remake-stx (list->vector
                    (map (lambda (stx) (repair-stx stx text))
                         (vector->list (syntax-e stx)))))]
      [(first . rest)
       (remake-stx (cons (repair-stx #'first text)
                         (repair-stx #'rest text)))]
      [_ (and (symbol? (syntax-e stx))
              (used-to-be-a-dot? stx))
         (datum->syntax-object stx '|.| stx stx)]
      [_ (not (symbol? (syntax-e stx)))
         (repair-nonsymbolic-datum)]
      [else stx])))

