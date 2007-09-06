(module in-something mzscheme
  (require (lib "contract.ss")
           (only (lib "13.ss" "srfi") string-prefix?))
  
  (provide/contract [in-something? (string? . -> . (or/c string? false/c))])
  
  ;; in-something?: string -> (union #f string)
  ;; Returns a true value if the str appears to be part of an incomplete literal.
  ;; The true value can be used to terminate the literal.
  ;;
  ;; (in-something "\"hello") should return "\""
  ;;
  (define (in-something? str)
    (let loop ([i 0]
               [in #f]
               [escaped-char? #f]
               [here-marker #f])
      
      (define (is? c)
        (string-prefix? c str 0 (string-length c) i (string-length str)))
      
      (define (consume c)
        (cond [(not in)
               (loop (add1 i) c #f here-marker)]
              [(string=? in c)
               (loop (add1 i) #f #f here-marker)]
              [else
               (loop (add1 i) in #f here-marker)]))
      
      (define (form-output)
        (cond [here-marker
               (string-append "\n" here-marker)]
              [(and escaped-char? in)
               (string-append "\\" in)]
              [escaped-char? "\\"]
              [else in]))
      
      (define (handle-here-marker)
        (let ([j (+ i
                    (string-length here-marker)
                    2)])
          (cond
            [(and (is? (string-append "\n" here-marker))
                  (or (>= j (string-length str))
                      (char-whitespace? (string-ref str j))))
             (loop j #f #f #f)]
            [else
             (loop (add1 i) #f #f here-marker)])))
      
      (cond
        [(>= i (string-length str))
         (form-output)]
        
        [(and (not here-marker)
              (is? "#<<"))
         (loop (add1 i) #f #f (get-here-string-marker str i))]
        [here-marker
         (handle-here-marker)]
        
        [escaped-char?
         (loop (add1 i) in #f here-marker)]
        [(is? ";")
         (loop (add1 i) "" #f here-marker)]
        [(and (is? "\n") (equal? in ""))
         (loop (add1 i) #f #f here-marker)]
        [(is? "\\")
         (loop (+ 1 i) in #t here-marker)]
        
        [(is? "\"" ) 
         (consume "\""  )]
        [(is? "|")
         (consume "|")]
        [else
         (loop (add1 i) in #f here-marker)])))
  
  
  ;; get-here-string-marker: string index -> (union string #f)
  ;; Returns the terminator characters needed to end
  ;; a here string.
  (define (get-here-string-marker a-str i)
    (let loop ([j (+ i (string-length "#<<"))]
               [chars-so-far/rev '()])
      (cond
        [(or (= j (string-length a-str))
             (char=? (string-ref a-str j) #\newline))
         (list->string (reverse chars-so-far/rev))]
        [else
         (loop (add1 j) (cons (string-ref a-str j) chars-so-far/rev))]))))