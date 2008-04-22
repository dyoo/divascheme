(module weak-set mzscheme
  (require (lib "contract.ss"))
  
  ;; Weak eq? sets, using weak hash tables.
  
  (define-struct weak-set (ht))
  
  (define (new-weak-set)
    (make-weak-set (make-hash-table 'weak)))
  
  (define (weak-set-add! ws an-elt)
    (hash-table-put! (weak-set-ht ws) an-elt #t))
  
  (define (weak-set-remove! ws an-elt)
    (hash-table-remove! (weak-set-ht ws) an-elt))
  
  (define (weak-set-for-each ws f)
    (hash-table-for-each (weak-set-ht ws)
                         (lambda (k v)
                           (f k))))
  
  (define (weak-set-map ws f)
    (hash-table-map (weak-set-ht ws)
                    (lambda (k v)
                      (f k))))
  
  (provide/contract [new-weak-set
                     (-> weak-set?)]
                    [weak-set-add!
                     (weak-set? any/c . -> . any)]
                    [weak-set-for-each
                     (weak-set? (any/c . -> . any) . -> . any)]
                    [weak-set-map
                     (weak-set? (any/c . -> . any) . -> . (listof any/c))]))