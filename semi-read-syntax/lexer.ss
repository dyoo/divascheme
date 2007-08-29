(module lexer mzscheme
  (require (lib "lex.ss" "parser-tools")
           (lib "contract.ss")
           (lib "etc.ss")
           (prefix : (lib "lex-sre.ss" "parser-tools")))
  
  ;; This is meant to be a lexer for Schemeish languages. We allow
  ;; tokens that normally not represented explicitely in an AST (such as comments)
  ;; because we want to support faithful pretty-printing.
  
  (provide/contract [plt-lexer (input-port? . -> . position-token?)])
  
  
  ;; Here are the standard tokens we're going to handle.
  (define-tokens token (atom special-atom quoter-prefix prefix suffix space end))
  
  (define-lex-abbrev digit (char-set "0123456789"))
  
  (define-lex-abbrev opener-prefix
    (:or ""
         "#"
         "#hash"
         "#hasheq"
         (:: "#" (:+ digit))
         (:: "#;")))
  
  (define-lex-abbrev openers (:or (:: opener-prefix (:or "(" "[" "{"))))
  
  (define-lex-abbrev closers (:or ")" "]" "}"))
  
  (define-lex-abbrev quoters (:or "'" "`" "," ",@"
                                  "#;" ;; nested comments
                                  "#&" ;; boxes
                                  "#'" "#`" "#," "#,@"
                                  "#cs" "#ci"))
  
  (define-lex-abbrev nl (:or "\r\n"
                             "\n"
                             "\r"))
  
  (define-lex-abbrev atom-delims (:or whitespace (char-set "\",'`;()[]{}")))
  
  (define-lex-abbrev escaped-symbol-chars
    (:or (:: "|" (:* (char-complement (char-set "|"))) "|")
         (:: "\\" any-char)))
  
  
  ;; A regular atom is something that doesn't have a pound in front of it.
  ;; We treat pounded atoms separately since they're messy.
  (define-lex-abbrev pound-prefix-free-atom
    (:& (complement (:: "#" any-string))
        atom-chars))
  
  (define-lex-abbrev atom-chars
    (:+ (:or (:& (char-complement (char-set "|\\"))
                 (char-complement (:& any-char atom-delims)))
             escaped-symbol-chars)))
  
  (define-lex-abbrev pounded-atoms
    (:or "#t" "#f" "#T" "#F"
         (:: "#" (char-set "eibodx") pound-prefix-free-atom) ;; numeric constants
         (:: "#%" pound-prefix-free-atom)
         (:: "#:" atom-chars) ;; keywords
         (:: "#\\" any-char)
         (:: "#\\" alphabetic alphabetic (:* alphabetic))
         (:: "#\\" (:? any-char) (:+ digit))
         (:: "#" (:+ digit) "#")
         (:: "#" (:+ digit) "=")))
  
  
  
  (define-lex-abbrev string-literal (::
                                     (:? (:or "#rx" "#px"))
                                     (:? "#") ;; can be a byte string
                                     "\"" 
                                     (:* (:or (char-complement (char-set "\"\\")) 
                                              (:: #\\ any-char)))
                                     "\""))
  
  ;; FIXME: handle specials
  ;; FIXME: handle special comments
  
  
  
  
  (define-lex-abbrev line-comment (:: ";" (:& any-string
                                              (complement (:: any-string nl any-string)))))
  
  
  ;; get-here-string: input port (string -> string) -> string
  ;;
  ;; Reads a here string; adapted from the get-here-string function
  ;; from the scheme-lexer in the syntax-color collection.
  (define (get-here-string i error-k)
    (local
        [(define (get-offset i)
           (let-values (((x y offset) (port-next-location i)))
             offset))
         
         (define (special-read-line i)
           (let ((next (peek-char-or-special i)))
             (cond
               ((or (eq? next #\newline) (not (char? next)))
                null)
               (else
                (read-char i)
                (cons next (special-read-line i))))))]
      (let* ((ender (list->string (special-read-line i)))
             (next-char (peek-char-or-special i)))
        (cond
          ((or (equal? ender "") (not (eq? #\newline next-char)))
           (error-k (string-append "#<<" ender)))
          (else
           (read-char i)
           (let loop ((acc (list (string-append "#<<" ender "\n"))))
             (let* ((next-line (list->string (special-read-line i)))
                    (next-char (peek-char-or-special i)))
               (cond
                 ((not (or (char? next-char) (eof-object? next-char))) ;; a special
                  (error-k (apply string-append (reverse! (cons next-line acc)))))
                 ((equal? next-line ender) ;; end of string
                  (apply string-append (reverse! (cons next-line acc))))
                 ((eof-object? next-char)
                  (error-k (apply string-append (reverse! (cons next-line acc)))))
                 (else
                  (read-char i)
                  (loop (cons (string-append next-line "\n") acc)))))))))))
  
  
  (define (get-nested-comment ip)
    (local ((define nested-lexer
              (begin-lifted
                (lexer
                 ("#|"
                  (lambda (self i loop input-port)
                    (cons "#|"
                          (loop (self input-port) (add1 i)))))
                 ("|#"
                  (lambda (self i loop input-port)
                    (cond
                      [(= i 1)
                       (list "|#")]
                      [else
                       (cons "|#"
                             (loop (self input-port) (sub1 i)))])))
                 (any-char
                  (lambda (self i loop input-port)
                    (cons lexeme (loop (self input-port) i))))
                 ((eof)
                  (lambda (self i loop input-port)
                    (error 'get-nested-comment "expected |#")))))))
      (apply string-append
             (cons "#|"
                   (let loop ([next-action (nested-lexer ip)]
                              [depth 1])
                     (next-action nested-lexer depth loop ip))))))
  
  
  ;; Eats lines, following convention to use backslash as a continuation character.
  (define continuation-line-lexer
    (lexer
     ((:: (:* (:: (complement (:: any-string nl any-string)) "\\" nl))
          (:: (complement (:: any-string nl any-string))) nl)
      lexeme)))
  
  
  (define plt-lexer
    (local ((define lexer
              (lexer-src-pos
               ("#!"
                (cond
                  [(= 0 (position-offset start-pos))
                   (token-atom
                    (string-append "#!" (continuation-line-lexer input-port)))]
                  
                  [else (token-atom lexeme)]))
               
               (openers
                (token-prefix lexeme))
               (closers
                (token-suffix lexeme))
               
               ((repetition 1 +inf.0 whitespace)
                (token-space lexeme))
               
               (quoters
                (token-quoter-prefix lexeme))
               (pounded-atoms
                (token-atom lexeme))
               (pound-prefix-free-atom
                (token-atom lexeme))
               (line-comment
                (token-atom lexeme))
               (string-literal
                (token-atom lexeme))
               ("#<<"
                (token-atom (get-here-string input-port
                                             (lambda (recovery) recovery))))
               ("#|"
                (token-atom (get-nested-comment input-port)))
               
               ((special)
                (token-special-atom lexeme))
               
               ((eof) (token-end lexeme)))))
      (lambda (ip)
        (lexer ip)))))