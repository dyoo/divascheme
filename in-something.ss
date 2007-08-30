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
  
  ;; TODO: "here" string support
  
  (define (in-something? str)
    (let loop ([i 0]
               [in #f]
               [escaped-char? #f])
      
      (define (is? c)
        (string-prefix? c str 0 (string-length c) i (string-length str)))
      
      (define (consume c)
        (cond [(not in)
               (loop (add1 i) c #f)]
              [(string=? in c)
               (loop (add1 i) #f #f)]
              [else
               (loop (add1 i) in #f)]))
      
      (define (form-output)
        (cond [(and escaped-char? in)
               (string-append "\\" in)]
              [escaped-char? "\\"]
              [else in]))
      (cond
        [(>= i (string-length str))
         (form-output)]
        [escaped-char? (loop (add1 i) in #f)]
        [(is? ";") (loop (add1 i) "" #f)]
        [(and (is? "\n") (equal? in ""))
         (loop (add1 i) #f #f)]
        
        [(is? "\\") (loop (+ 1 i) in #t)]
        [(is? "\"" ) (consume "\"")]
        [(is? "|") (consume "|")]
        [else (loop (add1 i) in #f)]))))