(module weak-memoize mzscheme
  ;; I should probably use dherman's memoize package...
  (provide weak-memoize weak-memoize/equal)
  
  ;; weak-memoize: (X -> X) -> (X -> X)
  ;; Applies a weak memoization to see if we can speed up get-move-after-dstx.
  (define (weak-memoize f)
    (let ([ht (make-hash-table 'weak)])
      (lambda (x)
        (cond
          [(hash-table-get ht x #f)
           =>
           (lambda (x)
             x)]
          [else
           (let ([result (f x)])
             (hash-table-put! ht x result)
             result)]))))
  
  (define (weak-memoize/equal f)
    (let ([ht (make-hash-table 'weak 'equal)])
      (lambda (x)
        (cond
          [(hash-table-get ht x #f)
           =>
           (lambda (x)
             x)]
          [else
           (let ([result (f x)])
             (hash-table-put! ht x result)
             result)])))))