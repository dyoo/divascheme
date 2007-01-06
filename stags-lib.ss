(module stags-lib mzscheme
  (require (lib "list.ss")
           (lib "etc.ss")
           (lib "file.ss")
           (lib "stx.ss" "syntax")
           (only (lib "misc.ss" "swindle") mappend)) 
  
  (provide generate-stags-file)
  
  
  
  ;; generate-stags-file: (listof filename) filename -> void
  ;; Writes out an stags file of the given files to the output-file-path.
  (define (generate-stags-file files output-file-path)
    (let ([target-tag-file (open-output-file output-file-path 'replace)]
          [done-stdin false])
      (fprintf target-tag-file "(~n")
      (for-each
       (lambda (f)
         (if (and (equal? f "-") (not done-stdin))
             (begin (process-stdin target-tag-file)
                    (set! done-stdin true))
             (process-file f target-tag-file)))
       files)
      (fprintf target-tag-file ")~n")
      (close-output-port target-tag-file)))
  
  
  
  ;; id->index-sexp: syntax -> (union (listof X) #f)
  ;; Given a syntax of an identifier, returns an s-expression
  ;; of the form (name filename line position)
  ;; where name and filename are strings, and line and position
  ;; are numbers.
  (define (id->index-sexp id)
    (if (or (not (syntax-source id))
            (not (syntax-position id)))
        #f
        (list (symbol->string (syntax-e id)) 
              (syntax-source id) 
              (syntax-line id) 
              (syntax-position id))))
  
  
  
  
  ;; find-defs-in-stx: syntax -> (listof syntax)
  ;; Returns a list of all the names of defined values and syntaxes in stx.
  ;; If something bad happens, returns empty list.
  (define (find-defs-in-stx stx)
    
    ;; extract-defined-from-top-level: syntax -> (listof syntax)
    ;;
    ;; Given a top-level-expression syntax, tries to return all 
    ;; the toplevel defined symbol syntaxes.
    (define (extract-defined-from-top-level stx)
      (syntax-case* stx (module begin %#plain-module-begin) module-or-top-identifier=?
        [(module m-name lang (#%plain-module-begin module-level-expr ...))
         (mappend extract-defined-from-module-level 
                  (syntax->list (syntax (module-level-expr ...))))]
        [(begin top-level-expr ...)
         (mappend extract-defined-from-top-level 
                  (syntax->list (syntax (top-level-expr ...))))]
        [else
         (extract-defined-from-general-top-level stx)]))
    
    
    ;; extract-defined-from-module-level: syntax -> (listof syntax)
    (define (extract-defined-from-module-level stx)
      (syntax-case* stx (provide begin) module-or-top-identifier=?
        [(provide provide-spec ...) empty]
        [(begin module-level-expr ...) 
         (mappend extract-defined-from-module-level 
                  (syntax->list (syntax (module-level-expr ...))))]
        [else (extract-defined-from-general-top-level stx)]))
    
    
    ;; extract-defined-from-general-top-level: syntax -> (listof syntax)
    (define (extract-defined-from-general-top-level stx)
      (syntax-case* stx (define-values define-syntaxes define-values-for-syntax 
                          require require-for-syntax require-for-template)
        module-or-top-identifier=?
        [(define-values (identifier ...) expr) (syntax->list (syntax (identifier ...)))]
        [(define-syntaxes (identifier ...) expr) (syntax->list (syntax (identifier ...)))]
        [(define-values-for-syntax (variable ...) expr) empty]
        [(require require-spec ...) empty]
        [(require-for-syntax require-spec ...) empty]
        [(require-for-template require-spec ...) empty]
        [else 
         ;; expressions don't contribute definitions, so:
         empty]))
    
    (extract-defined-from-top-level (expand stx)))
  
  
  
  ;; fold-syntaxes-over-file: (syntax A -> A) A filename -> A
  ;; Does a fold across all the syntaxes in a file.
  (define (fold-syntaxes-over-file fn acc target-file)
    #;(printf "processing ~a~n" target-file)
    (with-handlers
        ([exn:fail?
          (lambda (exn) (display (format "Error while processing ~a:\n~a\n\n" 
                                         target-file (exn-message exn)))
            acc)])
      (parameterize ([port-count-lines-enabled true]
                     [read-case-sensitive true]
                     [current-load-relative-directory 
                      (path-only (normalize-path target-file))])
        (call-with-input-file* target-file
          (lambda (port)
            (skip-magic! port)
            (let loop ([stx (read-syntax target-file port)]
                       [acc acc])
              (if (eof-object? stx)
                  acc
                  (loop (read-syntax target-file port)
                        (fn (expand stx) acc)))))))))
  
  ;; skip-magic!: ip -> (void)
  ;; skips the magic header off a port, for files that look like scripts.
  (define (skip-magic! port)
    (when (equal? #"#!" (peek-bytes 2 0 port))
      (read-line port))
    (void))
  
  
  ;; get-index-sexps-from-file: input-port -> (listof s-expr)
  ;; Extracts index s-expressions from a file.
  (define (get-index-sexps-from-file input-file)
    (let* ([ids (fold-syntaxes-over-file 
                 (lambda (stx acc)
                   (append (find-defs-in-stx stx) acc))
                 empty
                 input-file)]
           [sexps (filter (lambda (x) x) (map id->index-sexp ids))])
      sexps))
  
  
  (define (process-file input-file target-tag-file)
    (for-each (lambda (index-sexp)
                (write index-sexp target-tag-file)
                (newline target-tag-file))
              (get-index-sexps-from-file input-file)))
  
  
  (define (process-stdin target-tag-file)
    (let loop ([line (read-line)])
      (unless (eof-object? line)
        (process-file line target-tag-file)
        (loop (read-line))))))
