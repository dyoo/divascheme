(module woot mzscheme
  (require (lib "struct.ss")
           (lib "plt-match.ss"))
  
  ;;
  ;; TYPES
  ;;
  
  ; id = int
    
  ; paren-type = | 'toplevel | 'paren | 'bracket | 'brace | 'vector
  ;              | 'paren-dotted | ???
  
  (define-struct exp (
    paren-type ; paren-type
    id ; id
    id-start ; id
    sexps ; list[sexp]
  ))
  
  (define-struct atom (
    contents ; string
    id ; id
  ))
  
  (define-struct tomb-d (
    id ; id
    sexp ; sexp
  ))
  
  (define-struct tomb-m (
    id ; id
    forward-id ; id
  ))
  
  ; w-type = 'space | 'tab | 'newline
  
  (define-struct whitespace (
    w-type ; w-type
  ))
  
  (define-struct comment (
    text ; string
  ))
  
  ; sexp = exp | atom | tomb-d | tomb-m | whitespace | comment
  
  
  (define-struct move (
    id ; id
    id-before ; id
    id-after ; id
    new-id ; id
  ))
  
  (define-struct ins (
    sexp ; sexp
    id-before ; id
    id-after ; id
  ))
  
  (define-struct del (
    id ; id
  ))
  
  ; op = move | ins | del
  
  
  ;;
  ;; GLOBALS
  ;;
  
  (define top-id ???) ; id
  (define top-start-id ???) ; id
  
  ; ast: exp
  ; Our big ol' AST
  (define ast (exp 'toplevel top-id top-start-id empty))
  
  ; index-and-parent-of-in: id -> exp -> int sexp
  ; TODO: cache this (invalidate on move, &c.)
  (define (index-and-parent-of id exp)
    (match exp
      [(struct exp 
  )
  
  
  ; move: move -> void
  ; integrate a move operation into our AST
  ; assumes preconditions are met
  (define (move move-op)
    
  )
)
