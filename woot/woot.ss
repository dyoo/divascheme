(module woot mzscheme
  (require (lib "struct.ss"))
  
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
    id ; id
    contents ; string
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
    id ; id
    w-type ; w-type
  ))
  
  (define-struct comment (
    id ; id
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
  
  ; sexp-id: sexp -> id
  (define (sexp-id sexp)
    (cond
      [(exp? sexp) (exp-id sexp)]
      [(tomb-d? sexp) (tomb-d-id sexp)]
      [(tomb-m? sexp) (tomb-m-id sexp)]
      [(whitespace? sexp) (whitespace-id sexp)]
      [(comment? sexp) (comment-id sexp)]))
  
  ; sexp-sexps: sexp -> list[sexp]
  (define (sexp-sexps sexp)
    (cond
      [(exp? sexp) (exp-sexps sexp)]
      [else empty]))
  
  ; parent-exp-of-in: id sexp -> sexp
  ; TODO: cache this (invalidate on move, &c.)
  (define (parent-exp-of-in id sexp)
    (if (ormap (lambda (sexp) (eq? id (sexp-id sexp))) (sexp-sexps sexp))
        sexp
        (ormap (lambda (sexp) (parent-exp-of-in id sexp)) (sexp-sexps sexp))))
  
  (define (parent-exp-of id) (parent-exp-of-in id ast))
  
  ; exp-of-in: id sexp -> sexp
  ; TODO: cache this (invalidate on move, &c.)
  (define (exp-of-in id sexp)
    (if (eq? id (sexp-id sexp))
        sexp
        (ormap (lambda (sexp) (exp-of-in id sexp)) (sexp-sexps sexp))))
  
  (define (exp-of id) (exp-of-in id ast))
  
  ; move: move -> void
  ; integrate a move operation into our AST
  ; assumes preconditions are met
  (define (move move-op)
    (let* ([y (parent-exp-of id)]
           [x (exp-of-in id y)]
           [x (exp (exp-paren-type 
  )
)
