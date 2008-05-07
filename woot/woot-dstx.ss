(module woot-dstx mzscheme
  (require "../dsyntax/dsyntax.ss")
  (define (woot-lookup prop)
    (λ (sexp)
      (dstx-property-ref sexp
                         prop
                         (λ (x) #f))))
  (define tomb? (woot-lookup 'tomb))
  (define (tomb-d? sexp)
    (eq? (tomb? sexp)
         'tomb-d))
  (define (tomb-m? sexp)
    (eq? (tomb? sexp)
         'tomb-m))
  (define woot-id
    (woot-lookup 'id))
  (define (woot-exp paren-type
                    id
                    id-start
                    sexps)
    (dstx-property-set (dstx-property-set (new-fusion (par-open paren-type)
                                                      sexps
                                                      (par-close paren-type))
                                          'id
                                          id)
                       'id-start
                       id-start))
  (define (par-pair type)
    (cond
      [(eq? type 'paren)
       (cons "(" ")")]
      [(eq? type 'square)
       (cons "[" "]")]
      [(eq? type 'curl)
       (cons "{" "}")]
      [(eq? type 'top-level)
       (cons "" "")]
      [(eq? type 'quote)
       (cons "'(" ")")]
      [(eq? type 'line-comment)
       (cons ";" "")]
      [(eq? type 'sexp-comment)
       (cons "#;" "")]
      #;more?))
  (define (par-open type)
    (car (par-pair type)))
  (define (par-close type)
    (cdr (par-pair type)))
  
  (define (woot-atom id content)
    (dstx-property-set (new-atom content)
                       'id
                       id))
  (define (woot-tomb-d id sexp)
    (dstx-property-set (dstx-property-set (new-special-atom sexp)
                                          'id
                                          id)
                       'tomb
                       'tomb-d))
  (define (woot-tomb-m id forward-id)
    (dstx-property-set (dstx-property-set (new-special-atom id)
                                          'id
                                          id)
                       'tomb
                       'tomb-m))
  (define (woot-whitespace id w-type)
    (dstx-property-set (new-space w-type)
                       'id
                       id))
  (define (woot-comment id text)
    (dstx-property-set (new-special-atom text)
                       'comment
                       #t))
  (define top-id
    'top)
  (define top-start-id
    'top-start)
  (define ast
    (dstx-property-set (dstx-property-set (new-fusion ""
                                                      ()
                                                      "")
                                          'id
                                          top-id)
                       'id-start
                       top-start-d))
  (define (sexp-id sexp)
    (dstx-property-ref sexp 'id))
  (define (sexp-sexps sexp)
    (if (fusion? sexp)
        (fusion-children sexp)
        ())) ;should return #f ?
  (define (sexps-cur c)
    (and (list? c)
         (car c)))
  (define (sexps-older c)
    (and (list? c)
         (car c)))
  
  (define (sexps-ormap f c)
    (and c (or (f (sexps-cur c)) (sexps-ormap f (sexps-older c)))))
  
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
        (sexps-ormap (lambda (sexp) (sexp-of-in id sexp)) (sexp-sexps sexp))))
  
  (define (exp-of id) (sexp-of-in id ast))
  
  (define (chase-exp sexp)
    (cond
      [(tomb-d? sexp)
       (chase-exp (special-atom-content sexp))]
      [(tomb-m? sex)
       (chase-exp (sexp-of-in (special-atom-content sexp)
                              ast))]
      [else
       sexp]))
  (define (insert-before-first-larger-id sexp parent id-before id-after)
    (set! ast
          (let*
              ((top-cursor (make-top-level-cursor (fusion-children ast))) ; focus on top
               (above-cursor (focus-find/dstx top-cursor
                                              (λ (sexp)
                                                (eq? sexp parent)))) ; focus on parent
               (front-cursor (focus-in above-cursor))) ; focus on youngest sibling
            (if front-cursor ; If parent has any children
                (let* ((left-cursor (focus-search front-cursor
                                                  focus-older
                                                  (λ (el)
                                                    (eq? (woot-id (cursor-dstx el))
                                                         id-before)))) ; focus on id-before
                       (right-cursor (focus-search left-cursor
                                                   focus-older
                                                   (λ (el)
                                                     (or (not (focus-older el))
                                                         (eq? id-after
                                                              (woot-id (cursor-dstx (focus-older el))))))))) ; focus to left of destination
                  (insert-after right-cursor sexp))
                (inster-after (focus-in/no-snap above-cursor))))))