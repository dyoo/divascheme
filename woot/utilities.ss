(module utilities mzscheme
  (require (lib "contract.ss")
           (lib "plt-match.ss")
           "../dsyntax/dsyntax.ss"
           "woot-struct.ss")
  
  (provide/contract [first-sentinel-woot-id woot-id?]
                    [fresh-woot-id (string? . -> . woot-id?)]
                    [dstx-woot-id (dstx? . -> . (or/c woot-id? false/c))]
                    [dstx-all-woot-ids (dstx? . -> . (listof woot-id?))]
                    [dstx-local-id (dstx? . -> . number?)]
                    [dstx-set-woot-id (dstx? woot-id? . -> . dstx?)]
                    [deep-attach-woot-ids (dstx? string? . -> . dstx?)]
                    [deep-strip-local-ids (dstx? . -> . dstx?)])
  
  
  ;; The very first dstx will have this identifier, which is shared among all clients.
  (define first-sentinel-woot-id (make-woot-id 0 "woot"))
  
  
  ;; fresh-woot-id: string -> woot-id
  ;; Returns a fresh woot id.  The host id is meant to uniquely identify
  ;; the entity that generates the woot id.
  (define (fresh-woot-id host-id)
    (make-woot-id (next-logical-id) host-id))
  
  
  ;; dstx-woot-id: dstx -> (or/c woot-id false)
  ;; Get the woot id of the dstx.  If not decorated with
  ;; a woot-id, returns false.
  (define (dstx-woot-id a-dstx)
    (dstx-property-ref a-dstx 'woot-id (lambda () #f)))
  
  
  
  ;; dstx-all-woot-ids: dstx -> (listof woot-id)
  ;; Given a dstx, returns all the woot ids of the dstx.  Does
  ;; a deep traversal.
  (define (dstx-all-woot-ids a-dstx)
    (match a-dstx
      [(struct atom (props content))
       (list (dstx-woot-id a-dstx))]
      [(struct special-atom (props content width))
       (list (dstx-woot-id a-dstx))]
      [(struct space (props content))
       (list (dstx-woot-id a-dstx))]
      [(struct fusion (props prefix children suffix))
       (append (list (dstx-woot-id a-dstx))
               (apply append (map dstx-all-woot-ids children)))]))
  
  
  ;; dstx-local-id: dstx -> number
  (define (dstx-local-id a-dstx)
    (dstx-property-ref a-dstx 'local-id))
  
  
  ;; dstx-set-woot-id: dstx woot-id -> dstx
  ;; Set the woot id of the dstx.
  (define (dstx-set-woot-id a-dstx a-woot-id)
    (dstx-property-set a-dstx 'woot-id a-woot-id))
  
  
  ;; deep-attach-woot-id: dstx string -> dstx
  ;; Attach new woot identifiers to any dstx that doesn't yet have one.
  (define (deep-attach-woot-ids a-dstx host-id)
    (dstx-deepmap (lambda (a-dstx)
                    (cond
                      [(dstx-woot-id a-dstx)
                       a-dstx]
                      [else
                       (dstx-set-woot-id a-dstx (fresh-woot-id host-id))]))
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