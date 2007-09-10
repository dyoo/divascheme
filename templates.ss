(module templates mzscheme
  (require (lib "plt-match.ss")
           (lib "etc.ss")
           (lib "list.ss")
           (lib "string-constant.ss" "string-constants")
           "utilities.ss"
           "structures.ss"
           "language.ss")
  
  (provide lookup-template
           placeholder/string?
           placeholder/stx?
           placeholder-e/stx
           placeholder-e/string
           ellipsis/stx?
           enter-ellipsis/stx?
           join-ellipsis/stx?
           update-ellipsis/stx?
           holder/ellipsis-tree?
           holder/ellipsis?
           not-holder/ellipsis?
           current-templates
           mzscheme-templates)
  
  (define voice-debug false)
  (define (voice-printf . args)
    (when voice-debug
      (apply printf args)))
  
  
  (define (make-template-table sym/templates-list)
    (define h (make-hash-table 'equal))
    (for-each (lambda (sym/templates)
                (hash-table-put! h (first sym/templates) (rest sym/templates)))
              sym/templates-list)
    h)
  
  
  (define quoting-templates 
    '((|'| "'$expr$")
      (|`| "`$expr$")
      (|,| ",$expr$")
      (|,@| ",@$expr$")
      
      ;; This one is not right, but there's little we can do until we write
      ;; our own reader to handle weird cases like this.
      (|#| "#($expr$)")

      (|#'| "#'$expr$")
      (|#`| "#`$expr$")
      (|#,| "#,$expr$")
      (|#,@| "#,@$expr$")))
  
  (define default-templates
    (make-template-table
     quoting-templates))
  
  (define mzscheme-templates
    (make-template-table 
     (append
      quoting-templates
      '((let  "(let ([$name$ $binding$] ***) \n $body$ +++)"
          "(let $name$ ([$name$ $binding$] ***) \n $body$ +++)"
          "(let ([$name$ $binding$] ---) $body$ ---)")
        (let*  "(let* ([$name$ $binding$] ---) $body$ ---)")
        (letrec  "(letrec ([$name$ $binding$] ---) $body$ ---)")
        (let-values  "(let-values ([($name$ ---) $binding$] ---) $body$ ---)")
        (let*-values  "(let*-values ([($name$ ---) $binding$] ---) $body$ ---)")
        (letrec-values  "(letrec-values ([($name$ ---) $binding$] ---) $body$ ---)")
        (let-syntax  "(let-syntax ([$keyword$ $transformer-spec$] ---) $body$ ---)")
        (letrec-syntax  "(letrec-syntax ([$keyword$ $transformer-spec$] ---) $body$ ---)")
        (let-syntaxes  "(let-syntaxes ([($keyword$ ---) $transformer-spec$] ---) $body$ ---)")
        (letrec-syntaxes  "(letrec-syntaxes ([($keyword$ ---) $transformer-spec$] ---) $body$ ---)")
        (letrec-syntaxes+values  "(letrec-syntax+values ([($keyword$ ---) $transformer-spec$] ---) ([($name$ ---) $expression$] ---) $body)")
        (let-struct  "(let-struct $name$ ($field$ ---) $body$ ---)"
          "(let-struct ($name$ $parent$) ($field$ ---) $body$ ---)")
        (cond  "(cond \n [$test$ $expr$ ---] +++ \n [else $expr$ ---])"
               "(cond [$test$ $expr$ ---] --- [else $expr$ ---])"
               "(cond [$test$ $expr$ ---] ---)")
        (list  "(list $expression$ ---)")
        (apply  "(apply $function$ $arg$ --- $arg-list$)")
        (map  "(map $function$ $list$ ---)")
        (filter  "(filter $function$ $list$ ---)")
        (for-each  "(for-each $function$ $list$ ---)")
        (eval  "(eval $expression$)"
               "(eval $expression$ $environment-specifier$)")
        (match  "(match $expr$ [$pattern$ $expression$ ---] ---)")
        (id  "(id $expr$)")
        (format  "(format $string$ $expr$ ---)") 
        (printf  "(printf $string$ $expr$ ---)") 
        
        (first  "(first $list$)")
        (last-pair  "(last-pair $improper-list$)")
        (reverse  "(reverse $list)")
        (rest  "(rest $list$)")
        (second  "(second $list$)")
        (third  "(third $list$)")
        (fourth  "(fourth $list$)")
        (car  "(car $list$)")
        (cdr  "(cdr $list$)")
        (list-ref  "(list-ref $list$ $number$)")
        (list-tail  "(list-tail $list$ $number$)")
        (append  "(append $list$ ---)")
        (length  "(length $list$)")
        (cons  "(cons $element$ $list$)")
        (empty?  "(empty? $list$)")
        
        (load  "(load $filename$)")
        (dynamic-wind  "(dynamic-wind (lambda () $expression$) (lambda () $expession$) (lambda () $expression$))"
                       "(dynamic-wind before thunk after)")
        
        (define  "(define ($name$ $arg$ ---) \n $body$ +++)"
          "(define $name$ $expr$)"
          "(define ($name$ $arg$ --- . $arg$) \n $body$ ---)")
        (define-syntax  "(define-syntax $keyword$ $transformer-spec$)")
        (define-syntaxes  "(define-syntaxes ($keyword$ ---) $transformer-spec$)")
        (define-struct  "(define-struct $name$ ($field$ ---))"
          "(define-struct $name$ ($field$ ---) $inspector$)"
          "(define-struct ($name$ $parent$) ($field$ ---))"
          "(define-struct ($name$ $parent$) ($field$ ---) $inspector$)")
        (define-values  "(define-values ($name$ ---) $body$)")
        
        (values  "(values $expr$ ---)")
        (lambda  "(lambda ($name$ ---) $body$ ---)"
          "(lambda ($name$ --- . $name$) $body$ --)"
          "(lambda $formals$ $body$ ---)")
        (case-lambda  "(case-lambda [($name$ ---) $body$] ---)"
                      "(case-lambda [$formals$ $body$] ---)")
        (with-handlers  "(with-handlers ([$exn?$ (lambda (exn) $handler-expression$)] ***) \n $body$ +++)"
          "(with-handlers ([$exn?$ (lambda (exn) $handler-expression$)] ---) $body$ ---)"
          "(with-handlers ([(lambda (exn) $expression$) (lambda (exn) $handler-expression$)] ---) $body$ ---)"
          "(with-handlers ([$exn?$ $handler$] ---) $body$ ---)")
        (raise  "(raise $exception$)")
        (set!  "(set! $name$ $expression$)")
        (set!-values  "(set!-values ($name$ ---) $expression$)")
        (|.|  "($expr$ $expr$ --- . $expr$)")
        (infix  "($expr$ $expr$ --- . $expr$ . $expr$ $expr$ ---)")
        (quote  "(quote $expr$)")
        (quasiquote  "(quasiquote $expr$)")
        (unquote  "(unquote $expr$)")
        (unquote-splicing  "(unquote-splicing $expr$)")
        (quote-syntax  "(quote-syntax $expr$)")
        (quasisyntax  "(quasisyntax $expr$)")
        (unsyntax  "(unsyntax $expr$)")
        (unsyntax-splicing  "(unsyntax-splicing $expr$)")
        
        (#%app  "(#%app $expression$ ---)")
        (#%datum  "(#%datum . $datum$)")
        (#%top  "(#%top . $variable$)")
        
        (with-continuation-mark  "(with-continuation-mark $expr$ $expr$ $expr$)")
        (define-values-for-syntax  "(define-values-for-syntax ($variable$ ---) $expression$)")
        (require  "(require $require-spec$ ***)")
        (require-for-syntax  "(require-for-syntax $require-spec$ ---)")
        (require-for-template  "(require-for-template $require-spec$ ---)")
        
        (if  "(if $test$ $expression$ $expression$)"
             "(if $test$ $expression$)")
        (when  "(when $test$ $expression$ $expression$ ---)")
        (unless  "(unless $test$ $expression$ $expression$ ---)")
        (begin  "(begin $expression$ ---)")
        (begin0  "(begin0 $expression$ $expression$ ---)")
        
        (read-syntax  "(read-syntax $source-name$ $input-port$)")
        (syntax  "(syntax $expression$)")
        (syntax-position  "(syntax-position $syntax$)")
        (syntax-span  "(syntax-span $syntax$)")
        (syntax-column  "(syntax-column $syntax$)")
        (syntax-line  "(syntax-line $syntax$)")
        (expand  "(expand $stx-or-expression$)")
        (syntax-rules  "(syntax-rules ($literal-identifier$ ---) [($ignored-identifier$ . $pattern$) $template$] ---)")
        (syntax-case  "(syntax-case $stx-expr$ ($literal-identifier$ ---) [$pattern$ $fender-expr$ $expr$ ---] ---)")
        (with-syntax  "(with-syntax ([$pattern$ $stx-expr$] ---) $body$ ---)")
        (syntax-id-rules  "(syntax-id-rules (literal-identifier ---) [$pattern$ $template$] ---)")
        
        (provide  "(provide $provide-spec$ ---)")
        (module  "(module $name$ $language-name$ \n $definition-or-expression$ \n $definition-or-expression$ +++)"
          "(module $name$ $language-name$ $definition-or-expression$ $definition-or-expression$ ---)")))))
  
  
  
  ;; beginner-templates : (Template list)
  (define beginner-templates
    (make-template-table  
     (append
      quoting-templates
      '((define "(define ($name$ $name$ $name$ ---) $expression$)"
          "(define $name$ $expression$)"
          "(define $name$ (lambda ($name$ $name$ ---) $expression$))")
        (define-struct "(define-struct $name$ ($name$ ---))")
        (cond "(cond [$test$ $expression$] --- [else $expression$])")
        (if "(if $test$ $expression$ $expression$)")
        (and "(and $expression$ $expression$ $expression$ ---)")
        (or "(or $expression$ $expression$ $expression$ ---)")
        
        (first "(first $list$)")
        (reverse "(reverse $list)")
        (rest "(rest $list$)")
        (second "(second $list$)")
        (third "(third $list$)")
        (fourth "(fourth $list$)")
        (append "(append $list$ $list$)")
        (length "(length $list$)")
        (cons "(cons $element$ $list$)")
        (empty? "(empty? $list$)")
        
        (symbol? "(symbol? $expression$)")
        (symbol=? "(symbol=? $expression$)")
        
        (* "(* $number$ $number$ $number$ ---)")
        (+ "(+ $number$ $number$ $number$ ---)")
        (- "(- $number$ $number$ ---)")
        (/ "(/ $real$ $real$ $real$ ---)")
        (< "(< $real$ $real$ $real$ ---)")
        (> "(> $real$ $real$ $real$ ---)")
        (<= "(<= $real$ $real$ $real$ ---)")
        (>= "(>= $real$ $real$ ---)")
        (abs "(abs $number$)")
        (cos "(cos $number$)")
        (sin "(sin $number$)")
        (tan "(tan $number$)")))))
  
  ;; beginner-w/list-abbrev-templates : (Template list)
  (define beginner-w/list-abbrev-templates
    (make-template-table 
     (append 
      quoting-templates
      '((define "(define ($name$ $name$ $name$ ---) $expression$)"
          "(define $name$ $expression$)"
          "(define $name$ (lambda ($name$ $name$ ---) $expression$))")
        (define-struct "(define-struct $name$ ($name$ ---))")
        (cond "(cond [$test$ $expression$] --- [else $expression$])"
              "(cond [$test$ $expression$] [$test$ $expression$] ---)")
        (if "(if $test$ $expression$ $expression$)")
        (and "(and $expression$ $expression$ $expression$ ---)")
        (or "(or $expression$ $expression$ $expression$ ---)")
        (quote "(quote $expr$)")
        (quasiquote "(quasiquote $expr$)")
        (unquote "(unquote $expr$)")
        (unquote-splicing "(unquote-splicing $expr$)")
        
        (first "(first $list$)")
        (reverse "(reverse $list)")
        (rest "(rest $list$)")
        (second "(second $list$)")
        (third "(third $list$)")
        (fourth "(fourth $list$)")
        (car "(car $list$)")
        (cdr "(cdr $list$)")
        (append "(append $list$ $list$)")
        (length "(length $list$)")
        (cons "(cons $element$ $list$)")
        (empty? "(empty? $list$)")
        
        (symbol? "(symbol? $expression$)")
        (symbol=? "(symbol=? $expression$)")
        
        (* "(* $number$ $number$ $number$ ---)")
        (+ "(+ $number$ $number$ $number$ ---)")
        (- "(- $number$ $number$ ---)")
        (/ "(/ $real$ $real$ $real$ ---)")
        (< "(< $real$ $real$ $real$ ---)")
        (> "(> $real$ $real$ $real$ ---)")
        (<= "(<= $real$ $real$ $real$ ---)")
        (>= "(>= $real$ $real$ ---)")
        (abs "(abs $number$)")
        (cos "(cos $number$)")
        (sin "(sin $number$)")
        (tan "(tan $number$)")))))
  
  ;; intermediate-templates : (Template list)
  (define intermediate-templates
    (make-template-table 
     (append
      quoting-templates
      '((define "(define ($name$ $name$ $name$ ---) $expression$)"
          "(define $name$ $expression$)"
          "(define $name$ (lambda ($name$ $name$ ---) $expression$))")
        (define-struct "(define-struct $name$ ($name$ ---))")
        (local "(local ($definition$ ---) $expression$)")
        (letrec "(letrec ([$name$ $expression-for-let$] ---) $expression$)")
        (let "(let ([$name$ $expression-for-let$] ---) $expression$)")
        (let* "(let* ([$name$ $expression-for-let$] ---) $expression$)")
        (cond "(cond [$test$ $expression$] --- [else $expression$])"
              "(cond [$test$ $expression$] [$test$ $expression$] ---)")
        (if "(if $test$ $expression$ $expression$)")
        (and "(and $expression$ $expression$ $expression$ ---)")
        (or "(or $expression$ $expression$ $expression$ ---)")
        (time "(time $expression$)")
        (quote "(quote $expr$)")
        (quasiquote "(quasiquote $expr$)")
        (unquote "(unquote $expr$)")
        (unquote-splicing "(unquote-splicing $expr$)")
        
        (first "(first $list$)")
        (reverse "(reverse $list)")
        (rest "(rest $list$)")
        (second "(second $list$)")
        (third "(third $list$)")
        (fourth "(fourth $list$)")
        (car "(car $list$)")
        (cdr "(cdr $list$)")
        (append "(append $list$ $list$)")
        (length "(length $list$)")
        (cons "(cons $element$ $list$)")
        (empty? "(empty? $list$)")
        
        (symbol? "(symbol? $expression$)")
        (symbol=? "(symbol=? $expression$)")
        
        (* "(* $number$ $number$ $number$ ---)")
        (+ "(+ $number$ $number$ $number$ ---)")
        (- "(- $number$ $number$ ---)")
        (/ "(/ $real$ $real$ $real$ ---)")
        (< "(< $real$ $real$ $real$ ---)")
        (> "(> $real$ $real$ $real$ ---)")
        (<= "(<= $real$ $real$ $real$ ---)")
        (>= "(>= $real$ $real$ ---)")
        (abs "(abs $number$)")
        (cos "(cos $number$)")
        (sin "(sin $number$)")
        (tan "(tan $number$)")))))
  
  ;; intermediate-w/lambda-templates : (Template list)
  (define intermediate-w/lambda-templates
    (make-template-table 
     (append 
      quoting-templates
      '((define "(define ($name$ $name$ $name$ ---) $expression$)"
          "(define $name$ $expression$)")
        (define-struct "(define-struct $name$ ($name$ ---))")
        (lambda "(lambda ($name$ $name$ ---) $expression$)")
        (local "(local ($definition$ ---) $expression$)")
        (letrec "(letrec ([$name$ $expression$] ---) $expression$)")
        (let "(let ([$name$ $expression$] ---) $expression$)")
        (let* "(let* ([$name$ $expression$] ---) $expression$)")
        (cond "(cond [$test$ $expression$] --- [else $expression$])"
              "(cond [$test$ $expression$] [$test$ $expression$] ---)")
        (if "(if $test$ $expression$ $expression$)")
        (and "(and $expression$ $expression$ $expression$ ---)")
        (or "(or $expression$ $expression$ $expression$ ---)")
        (time "(time $expression$)")
        (quote "(quote $expr$)")
        (quasiquote "(quasiquote $expr$)")
        (unquote "(unquote $expr$)")
        (unquote-splicing "(unquote-splicing $expr$)")
        
        (first "(first $list$)")
        (reverse "(reverse $list)")
        (rest "(rest $list$)")
        (second "(second $list$)")
        (third "(third $list$)")
        (fourth "(fourth $list$)")
        (car "(car $list$)")
        (cdr "(cdr $list$)")
        (append "(append $list$ $list$)")
        (length "(length $list$)")
        (cons "(cons $element$ $list$)")
        (empty? "(empty? $list$)")
        
        (symbol? "(symbol? $expression$)")
        (symbol=? "(symbol=? $expression$)")
        
        (* "(* $number$ $number$ $number$ ---)")
        (+ "(+ $number$ $number$ $number$ ---)")
        (- "(- $number$ $number$ ---)")
        (/ "(/ $real$ $real$ $real$ ---)")
        (< "(< $real$ $real$ $real$ ---)")
        (> "(> $real$ $real$ $real$ ---)")
        (<= "(<= $real$ $real$ $real$ ---)")
        (>= "(>= $real$ $real$ ---)")
        (abs "(abs $number$)")
        (cos "(cos $number$)")
        (sin "(sin $number$)")
        (tan "(tan $number$)")))))
  
  ;; advanced-templates : (Template list)
  (define advanced-templates
    (make-template-table 
     (append
      quoting-templates 
      '((define "(define ($name$ $name$ ---) $expression$)"
          "(define $name$ $expression$)")
        (define-struct "(define-struct $name$ ($name$ ---))")
        (begin "(begin $expression$ $expression$ ---)")
        (begin0 "(begin0 $expression$ $expression$ ---)")
        (set! "(set! $name$ $expression$)")
        (delay "(delay $expression$)")
        (lambda "(lambda ($name$ ---) $expression$)")
        (local "(local ($definition$ ---) $expression$)")
        (letrec "(letrec ([$name$ $expression$] ---) $expression$)")
        (shared "(shared ([$name$ $expression$] ---) $expression$)")
        (let "(let ([$name$ $expression$] ---) $expression$)"
          "(let $name$ ([$name$ $expression$] ---) $expression$)")
        (let* "(let* ([$name$ $expression$] ---) $expression$)")
        (recur "(recur $name$ ([$name$ $expression$] ---) $expression$)")
        (cond "(cond [$test$ $expression$] --- [else $expression$])"
              "(cond [$test$ $expression$] [$test$ $expression$] ---)")
        (case "(case $expression$ [($choice$ $choice$ ---) $expression$] [($choice$ $choice$ ---) $expression$] ---)"
          "(case $expression$ [($choice$ $choice$ ---) $expression$] --- [else $expression$])")
        (if "(if $test$ $expression$ $expression$)")
        (when "(when $expression$ $expression$)")
        (unless "(unless $expression$ $expression$)")
        (and "(and $expression$ $expression$ $expression$ ---)")
        (or "(or $expression$ $expression$ $expression$ ---)")
        (time "(time $expression$)")
        (quote "(quote $expr$)")
        (quasiquote "(quasiquote $expr$)")
        (unquote "(unquote $expr$)")
        (unquote-splicing "(unquote-splicing $expr$)")
        
        (first "(first $list$)")
        (reverse "(reverse $list)")
        (rest "(rest $list$)")
        (second "(second $list$)")
        (third "(third $list$)")
        (fourth "(fourth $list$)")
        (car "(car $list$)")
        (cdr "(cdr $list$)")
        (append "(append $list$ $list$)")
        (length "(length $list$)")
        (cons "(cons $element$ $list$)")
        (empty? "(empty? $list$)")
        
        (symbol? "(symbol? $expression$)")
        (symbol=? "(symbol=? $expression$)")
        
        (* "(* $number$ $number$ $number$ ---)")
        (+ "(+ $number$ $number$ $number$ ---)")
        (- "(- $number$ $number$ ---)")
        (/ "(/ $real$ $real$ $real$ ---)")
        (< "(< $real$ $real$ $real$ ---)")
        (> "(> $real$ $real$ $real$ ---)")
        (<= "(<= $real$ $real$ $real$ ---)")
        (>= "(>= $real$ $real$ ---)")
        (abs "(abs $number$)")
        (cos "(cos $number$)")
        (sin "(sin $number$)")
        (tan "(tan $number$)")))))
  
  
  ;; current-templates : void -> void -> (Template list)
  (define (current-templates)
    default-templates
    ;; Currently, language-specific template support is disabled until we can 
    ;; figure out a less intrusive way to introduce them during typing.
    #;(match (get-language-name)
        [(? (lambda (a) (string=? a (string-constant beginning-student)))) beginner-templates]
        [(? (lambda (a) (string=? a (string-constant beginning-student/abbrev)))) beginner-w/list-abbrev-templates]
        [(? (lambda (a) (string=? a (string-constant intermediate-student)))) intermediate-templates]
        [(? (lambda (a) (string=? a (string-constant intermediate-student/lambda)))) intermediate-w/lambda-templates]
        [(? (lambda (a) (string=? a (string-constant advanced-student)))) advanced-templates]
      [_ default-templates]))
  
  
  (define (template-has-parentheses? tmpl)
    (and (regexp-match "^\\(.*\\)$" tmpl)
         #t))
  
  
  ;; lookup-template : (union symbol false) non-negative-integer boolean boolean -> sexp-string
  (define (lookup-template symbol template-number open? wrap?)
    (define templates 
      (and symbol
           (hash-table-get (current-templates) symbol (lambda () #f))))
    
    (if templates
        
        (let* ([number-in-list
                (if wrap?
                    (modulo template-number (length templates))
                    template-number)]
               [t (list-ref/safe templates number-in-list)]
               [l (length templates)]
               [p (template-has-parentheses? t)])
          (if (and open? (not p))
              (values (format "(~a)" t) l)
              (values t l)))
        
        (cond
          [(and open? symbol)
           (values (format "(~a $expr$ ---)" symbol) 1)]
          [open?
           (values "($expr$ ---)" 1)]
          [else
           (values #f 0)])))
  
  
  ;; placeholder/stx? : syntax -> boolean
  (define (placeholder/stx? stx)
    (match (syntax-e stx)
      [(? symbol? x) (placeholder/string? (symbol->string x))]
      [_ false]))
  
  ;; placeholder/string? : string -> boolean
  (define (placeholder/string? str)
    (let ([lst (string->list str)])
      (if (> 2 (length lst))
          false
          (let ([first-char (first lst)]
                [last-char  (first (reverse lst))])
            (and (eq? first-char #\$)
                 (eq? last-char #\$))))))
  
  ;; placeholder-e/stx : syntax -> symbol
  (define (placeholder-e/stx stx)
    (string->symbol (placeholder-e/string (symbol->string (syntax-e stx)))))
  
  ;; placeholder-e/string : string -> string
  (define (placeholder-e/string str)
    (substring str 1 (sub1 (string-length str))))
  
  ;; ellipsis/stx? : syntax -> boolean
  (define (ellipsis/stx? stx)
    (or (eq? '--- (syntax-e stx))
        (eq? '+++ (syntax-e stx))
        (eq? '*** (syntax-e stx))))
  
  ;; enter-ellipsis/stx? : syntax -> boolean
  (define (enter-ellipsis/stx? stx)
    (or (eq? '+++ (syntax-e stx))
        (eq? '*** (syntax-e stx))))
  
  ;; join-ellipsis/stx? : syntax -> boolean
  (define (join-ellipsis/stx? stx)
    (eq? '+++ (syntax-e stx)))
  
  ;; update-ellipsis/stx? : syntax -> (union string false)
  (define (update-ellipsis/stx? stx)
    (if (eq? '*** (syntax-e stx))
        "+++"
        false))
  
  ;; holder/ellipsis? : syntax -> boolean
  (define (holder/ellipsis? right-stx)
    (or (placeholder/stx? right-stx)
        (ellipsis/stx? right-stx)))
  
  ;; not-holder/ellipsis? : syntax -> boolean
  (define (not-holder/ellipsis? stx)
    (not (holder/ellipsis? stx)))
  
  ;; holder/ellipsis-tree? : syntax -> boolean
  (define (holder/ellipsis-tree? stx)
    (cond
      [(atomic/stx? stx) (holder/ellipsis? stx)]
      [else (match (stx->lst stx)
              [(list) false]
              [(list lst ...) (andmap holder/ellipsis-tree? lst)])])))