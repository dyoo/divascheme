(module utilities mzscheme
  (require (lib "contract.ss")
           "../dsyntax/dsyntax.ss"
           "woot-struct.ss")
  (provide/contract [fresh-woot-id (string? . -> . woot-id?)]
                    [dstx-woot-id (dstx? . -> . (or/c woot-id? false/c))]
                    [deep-attach-woot-ids (dstx? string? . -> . dstx?)]
                    [deep-strip-local-ids (dstx? . -> . dstx?)])
  
  
  ;; fresh-woot-id: string -> woot-id
  ;; Returns a fresh woot id.
  (define (fresh-woot-id host-ip)
    (make-woot-id (next-logical-id) host-ip))
  
  
  ;; dstx-woot-id: dstx -> woot-id
  (define (dstx-woot-id a-dstx)
    (dstx-property-ref a-dstx 'woot-id (lambda () #f)))
  
  
  ;; deep-attach-woot-id: dstx string -> dstx
  ;; Attach new woot identifiers to any dstx that doesn't yet have one.
  (define (deep-attach-woot-ids a-dstx host-ip)
    (dstx-deepmap (lambda (a-dstx)
                    (cond
                      [(dstx-woot-id a-dstx)
                       a-dstx]
                      [else
                       (dstx-property-set a-dstx 'woot-id (fresh-woot-id host-ip))]))
                  a-dstx))
  
  
  ;; deep-strip-local-ids: dstx -> dstx
  ;; Given a dstx, rip out the local ids.
  (define (deep-strip-local-ids a-dstx)
    (dstx-deepmap (lambda (a-dstx)
                    (dstx-property-remove a-dstx 'local-id))
                  a-dstx))
  
  
  
  
  ;; next-logical-id: -> number
  ;; Returns the next logical clock id.
  (define next-logical-id
    (let ([n 0])
      (lambda ()
        (set! n (add1 n))
        n))))