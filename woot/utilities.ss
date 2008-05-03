(module utilities mzscheme
  (require (lib "contract.ss")
           "../dsyntax/dsyntax.ss"
           "woot-struct.ss")
  
  (provide/contract [first-sentinel-woot-id woot-id?]
                    [fresh-woot-id (string? . -> . woot-id?)]
                    [dstx-woot-id (dstx? . -> . (or/c woot-id? false/c))]
                    [dstx-local-id (dstx? . -> . number?)]
                    [dstx-set-woot-id (dstx? woot-id? . -> . dstx?)]
                    [deep-attach-woot-ids (dstx? string? . -> . dstx?)]
                    [deep-strip-local-ids (dstx? . -> . dstx?)])
  
  
  ;; The very first dstx will have this identifier, which is shared among all clients.
  (define first-sentinel-woot-id (make-woot-id 0 "woot"))
  
  
  ;; fresh-woot-id: string -> woot-id
  ;; Returns a fresh woot id.
  (define (fresh-woot-id host-ip)
    (make-woot-id (next-logical-id) host-ip))
  
  
  ;; dstx-woot-id: dstx -> (or/c woot-id false)
  ;; Get the woot id of the dstx.  If not decorated with
  ;; a woot-id, returns false.
  (define (dstx-woot-id a-dstx)
    (dstx-property-ref a-dstx 'woot-id (lambda () #f)))
  
  
  ;; dstx-local-id: dstx -> number
  (define (dstx-local-id a-dstx)
    (dstx-property-ref a-dstx 'local-id))
  
  
  ;; dstx-set-woot-id: dstx woot-id -> dstx
  ;; Set the woot id of the dstx.
  (define (dstx-set-woot-id a-dstx a-woot-id)
    (dstx-property-set a-dstx 'woot-id a-woot-id))
  
  
  ;; deep-attach-woot-id: dstx string -> dstx
  ;; Attach new woot identifiers to any dstx that doesn't yet have one.
  (define (deep-attach-woot-ids a-dstx host-ip)
    (dstx-deepmap (lambda (a-dstx)
                    (cond
                      [(dstx-woot-id a-dstx)
                       a-dstx]
                      [else
                       (dstx-set-woot-id a-dstx (fresh-woot-id host-ip))]))
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