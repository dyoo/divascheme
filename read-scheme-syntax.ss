(module read-scheme-syntax mzscheme
  ;; Similar to read-syntax, but the intent is to do a few things different.
  ;;
  ;; In particular:
  ;;
  ;; * Comments will be treated as data.
  ;;
  ;; * Dots will treated as symbols.
  ;;
  ;; * Parentheses will remember where the closing-tag line and position are.
  ;;
  ;; Recursive structure stuff will be just another symbol.
  ;;
  ;; This is to make it possible to reprint syntaxes exactly as they appeared
  ;; from a file.
  ;;
  ;;
  ;; Notes: reusing scheme-lexer from the syntax-color collection might not be
  ;; the Right Thing to do here.  In particular, we're losing line and column
  ;; location.  And since scheme-lexer's primary job is to do colorization,
  ;; some tokens are classified weirdly (i.e. quote is a constant?)
  ;;
  
  (provide read-scheme-syntax)
  
  (require (lib "scheme-lexer" "syntax-color")
           (lib "etc.ss")
           (lib "lex.ss" "parser-tools")
           (lib "yacc.ss" "parser-tools")
           (lib "readerr.ss" "syntax"))
  
  
  (define-tokens data (COMMENT NO-COLOR
                               DATUM
                               OPEN-PAREN CLOSE-PAREN
                               OTHER SYMBOL EOF))
  
  (define (token-value-line a-token)
    #f)
  
  (define (token-value-column a-token)
    #f)
  
  (define (token-value-start a-token)
    #f)
  
  (define (token-value-span a-token)
    #f)
  
  
  (define (input-port->token-function inp)
    (define f
      (lambda ()
        (let-values ([(matching-text type paren-shape start-pos end-pos) (scheme-lexer inp)])
          (let ([value (list matching-text type paren-shape start-pos end-pos)])
            (case type
              [(error) (error 'input-port->token-function)]
              [(comment) (token-COMMENT value)]
              [(white-space) (f)]
              
              ;; FIXME: this is wrong
              [(constant) (token-DATUM value)]
              [(string) (token-DATUM value)]
              [(other) (token-DATUM value)]
              [(symbol) (token-DATUM value)]
              
              [(no-color) (token-NO-COLOR value)]
              
              [(parenthesis)
               (if (member matching-text '("(" "[" "{"))
                   (token-OPEN-PAREN value)
                   (token-CLOSE-PAREN value))]
              
              [(eof) (token-EOF (list value))])))))
    f)
  
  
  
  (define (make-scheme-parser source-name)
    (parser
     (start s)
     (end EOF)
     (error (lambda (a name val)
              (raise-read-error
               "read-error"
               source-name
               (token-value-line val)
               (token-value-column val)
               (token-value-start val)
               (token-value-span val))))
     (tokens data)
     (grammar
      (s [(sexp-list) (reverse $1)])
      (sexp-list [(sexp-list sexp) (cons $2 $1)]
                 [() '()])
      (sexp [(DATUM) $1]
            [(OPEN-PAREN sexp-list CLOSE-PAREN)
             (reverse $2)]))))
  
  
  ;; read-scheme-syntax: input-port -> syntax
  ;; not done yet
  (define (read-scheme-syntax inp)
    ((make-scheme-parser "read-scheme-syntax") (input-port->token-function inp))))