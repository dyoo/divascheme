(module woot-text-mixin mzscheme
  
  (require (lib "class.ss")
           (lib "plt-match.ss")
           (lib "mred.ss" "mred")
           "../structures.ss"
           "../dsyntax/dsyntax.ss"
           "self-ip-address.ss"
           (only (lib "13.ss" "srfi") string-join))
  
  (provide woot-text-mixin)
  
  ;; A woot-id is a (list a-number host-string)
  ;; where a-number is a number, and host-string is a string containing an ip address.
  
  
  
  ;; Infrastructure for tying in woot stuff into DivaScheme
  ;; woot-text-mixin: diva-text% -> diva-text%
  (define (woot-text-mixin super%)
    (class super%
      (inherit queue-for-interpretation!)
      
      (define host-ip (self-ip-address))
      
      (define (initialize)
        (super-new))
      
      
      (define/augment (decorate-new-dstx a-dstx)
        (let ([a-dstx (inner a-dstx decorate-new-dstx a-dstx)])
          (attach-woot-ids a-dstx)))
      
      
      ;; Attach new woot identifiers to any dstx that doesn't yet have one.
      (define (attach-woot-ids a-dstx)
        (dstx-deepmap (lambda (a-dstx)
                        (cond
                          [(dstx-property-ref a-dstx 'woot-id (lambda () #f))
                           a-dstx]
                          [else
                           (dstx-property-set a-dstx 'woot-id
                                              (fresh-woot-id host-ip))]))
                      a-dstx))
      
      
      
      
      (define (handle-structured-insert-before a-fcursor a-dstx)
        (printf "inserted ~s~n" (dstx->string a-dstx))
        (void))
      
      (define (handle-structured-insert-after a-fcursor a-dstx)
        (printf "inserted ~s~n" (dstx->string a-dstx))
        (void))
      
      (define (handle-structured-delete a-fcursor a-dstx)
        (printf "deleted ~s~n" (dstx->string a-dstx))
        (void))
      
      
      
      
      
      
      ;; Hooks into on-structured-insert-before.
      (define/augment (on-structured-insert-before a-fcursor a-dstx)
        (when (not (dstx-from-unstructured-editing? a-dstx))
          (handle-structured-insert-before a-fcursor a-dstx))
        (inner (void) on-structured-insert-before a-fcursor))
      
      ;; Hooks into on-structured-insert-after.
      (define/augment (on-structured-insert-after a-fcursor a-dstx)
        (when (not (dstx-from-unstructured-editing? a-dstx))
          (handle-structured-insert-after a-fcursor a-dstx))
        (inner (void) on-structured-insert-after a-fcursor))
      
      ;; Hooks into on-structured-delete.
      (define/augment (on-structured-delete a-fcursor)
        (when (not (dstx-from-unstructured-editing? (cursor-dstx a-fcursor)))
          (handle-structured-delete a-fcursor (cursor-dstx a-fcursor)))
        (inner (void) on-structured-delete a-fcursor))
      
      
      
      ;; Just as an experiment, see that we can queue the following for interpretation.
      #;(thread (lambda ()
                  (let loop ()
                    (sleep 5)
                    (queue-for-interpretation! (make-No-op))
                    (loop))))
      
      (initialize)))
  
  
  
  ;; fresh-woot-id: string -> woot-id
  ;; Returns a fresh woot id.
  (define (fresh-woot-id host-ip)
    (list (next-logical-id) host-ip))
  
  
  ;; dstx-from-unstructured-editing?: dstx -> boolean
  ;; Returns true if we're certain that the dstx came from intermediate insert-mode.
  (define (dstx-from-unstructured-editing? a-dstx)
    (dstx-property-ref a-dstx 'from-unstructured-editing (lambda ()
                                                           #f)))
  
  
  ;; dstx->string: dstx -> string
  ;; For debugging.
  (define (dstx->string a-dstx)
    (match a-dstx
      [(struct atom (props content))
       content]
      [(struct special-atom (props content width))
       (cond [(is-a? content string-snip%)
              (send content get-text 0 (send content get-count))]
             [else
              (format "~s" content)])]
      [(struct space (props content))
       content]
      [(struct fusion (props prefix children suffix))
       (format "~s ~a ~s"
               prefix
               (string-join (map (lambda (a-dstx)
                                   (format "~s" (dstx->string a-dstx)))
                                 children)
                            " ")
               suffix)]))
  
  
  
  ;; next-logical-id: -> number
  ;; Returns the next logical clock id.
  (define next-logical-id
    (let ([n 0])
      (lambda ()
        (set! n (add1 n))
        n))))