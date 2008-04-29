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
      [(tomb-d? sexp) (sexp-id (tomb-d-sexp sexp))]
      [(tomb-m? sexp) (tomb-m-id sexp)]
      [(whitespace? sexp) (whitespace-id sexp)]
      [(comment? sexp) (comment-id sexp)]))
  
  ; sexp-sexps: sexp -> list[sexp]
  (define (sexp-sexps sexp)
    (cond
      [(exp? sexp) (exp-sexps sexp)]
      [(tomb-d? sexp) (sexp-sexps (tomb-d-sexp sexp))]
      [else empty]))
  
  (define (sexps-cur c) (car c))
  (define (sexps-older c) (and (cons? c) (cdr c)))
  (define (sexps-ormap f c)
    (and c (or (f (sexps-cur c)) (sexps-ormap f (sexps-older c)))))
  
  ; replace-exp!: sexp id sexp -> void
  ; (replace-exp! parent id new) replaces the sexp with id id in parent with new
  
  ; parent-exp-of-in: id sexp -> exp
  ; TODO: cache this (invalidate on move, &c.)
  (define (parent-exp-of-in id sexp)
    (if (sexps-ormap (lambda (sexp) (eq? id (sexp-id sexp))) (sexp-sexps sexp))
        sexp
        (sexps-ormap (lambda (sexp) (parent-exp-of-in id sexp)) (sexp-sexps sexp))))
  
  (define (parent-exp-of id) (parent-exp-of-in id ast))
  
  ; sexp-of-in: id sexp -> sexp
  ; TODO: cache this (invalidate on move, &c.)
  (define (sexp-of-in id sexp)
    (if (eq? id (sexp-id sexp))
        sexp
        (sexps-ormap (lambda (sexp) (exp-of-in id sexp)) (sexp-sexps sexp))))
  
  (define (exp-of id) (exp-of-in id ast))
  
  ; chase-exp: sexp -> exp
  ; try coercing sexp into exp if possible
  (define (chase-exp sexp)
    (cond
      [(exp? sexp) sexp]
      [(tomb-d? sexp) (chase-exp (tomb-d-sexp sexp))]
      [(tomb-m? sexp) (chase-exp (exp-of (tomb-d-forward-id sexp)))]
      [else #f]))
  
  ; insert-before-first-larger-id: sexp exp id id -> void
  (define (insert-before-first-larger-id sexp parent id-before id-after)
    (local (define (insert-here sexps)
             (if (or (not sexps)
                     (eq? id-after (sexp-id (sexps-cur sexps)))
                     (> (sexp-id sexp) (sexp-id (sexps-cur sexps))))
               (insert-exp! sexps sexp)
               (insert-here (sexps-older sexps))))
           (define (insert-find sexps)
             (if (eq? id-before (sexp-id (sexps-cur sexps)))
               (insert-here (sexps-older sexps))
               (insert-find (sexps-older sexps))))
      (if (eq? id-before (exp-id-start parent))
        (insert-here (exp-sexps exp))
        (insert-find (exp-sexps exp)))))
  
  ; apply-move: move -> void
  ; integrate a move operation into our AST
  ; assumes preconditions are met
  (define (apply-move op)
    (let* ([y (parent-exp-of (move-id op))]
           [x (exp-of-in (move-id op) y)])
      (replace-exp! y (move-id op) (tomb-m (move-id op) (move-new-id op)))
      (insert-before-first-larger-id
        (exp (exp-paren-type x) (move-new-id op) (exp-id-start x) (exp-sexps x))
        (parent-exp-of (move-id-before op))
        (move-id-before op)
        (move-id-after op))))
  
  ; apply-ins: ins -> void
  ; integrate an insert operation into our AST
  ; assumes preconditions are met
  (define (apply-ins op)
    (insert-before-first-larger-id
      sexp
      (parent-exp-of (ins-id-before op))
      (ins-id-before op)
      (ins-id-after op)))
  
  ; apply-del: del -> void
  ; integrate a delete operation into our AST
  ; assumes preconditions are met
  (define (apply-del op)
    (let* ([y (parent-exp-of (del-id op))]
           [x (exp-of-in (del-id op) y)])
      (replace-exp! y (del-id op) (tomb-d (del-id op) x))))
  
  ; apply-op: op -> void
  ; integrate an operation into our AST
  ; assumes preconditions are met
  (define (apply-op op)
    (cond
      [(move? op) (apply-move op)]
      [(ins? op) (apply-ins op)]
      [(del? op) (apply-del op)]))
)
