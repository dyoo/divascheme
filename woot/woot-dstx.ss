(module woot mzscheme
  (require lib "dsyntax.ss"
           (define (exp paren-type
                        id
                        id-start
                        sexps)
             (cursor-dstx-property-set (cursor-dstx-property-set (new-fusion (par-open paren-type)
                                                                             sexps
                                                                             (par-close paren-type))
                                                                 'id
                                                                 id)
                                       'id-start
                                       id-start))
           (define (atom id content)
             (cursor-dstx-property-set (new-atom content)
                                       'id
                                       id))
           (define (tomb-d id sexp)
             (cursor-dstx-property-set (cursor-dstx-property-set (new-special-atom sexp)
                                                                 'id
                                                                 id)
                                       'tomb
                                       'tomb-d))
           (define (tomb-m id forward-id)
             (cursor-dstx-property-set (cursor-dstx-property-set (cursor-dstx-property-set (new-special-atom #f)
                                                                                           'id
                                                                                           id)
                                                                 'forward-id
                                                                 forward-id)
                                       'tomb
                                       'tomb-m))
           (define (whitespace id w-type)
             (cursor-dstx-property-set (new-space (sym-to-space w-type))
                                       'id
                                       id))
           (define (comment id text)
             (cursor-dstx-property-set (new-special-atom text)
                                       'comment
                                       #t))
           (define ast (exp 'top-level
                            top-id
                            top-start-id
                            '()))
           (define (sexp-id sexp)
             (dstx-property-ref sexp 'id))
           (define (sexp-sexps sexp)
             (if (fusion? sexp)
                 (fusion-children sexp)
                 '()))
           (define (sexp-cur c)
             (and (cons? c)
                  (car c)))
           (define (sexp-older c)
             (and (cons? c)
                  (cdr c)))
           ))