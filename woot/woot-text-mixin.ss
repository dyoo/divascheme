(module woot-text-mixin mzscheme
  
  (require (lib "class.ss")
           "../structures.ss"
           "../dsyntax/dsyntax.ss")
  
  (provide woot-text-mixin)
  
  
  ;; Infrastructure for tying in woot stuff into DivaScheme
  ;; woot-text-mixin: diva-text% -> diva-text%
  (define (woot-text-mixin super%)
    (class super%
      (inherit queue-for-interpretation!)
      (super-new)
      
      (define/augment (on-structured-insert-before a-fcursor a-dstx)
        (when (not (dstx-from-unstructured-editing? a-dstx))
          #;(printf "inserted ~s~n" a-dstx)
          (void)) 
        (inner (void) on-structured-insert-before a-fcursor))
      
      (define/augment (on-structured-insert-after a-fcursor a-dstx)
        (when (not (dstx-from-unstructured-editing? a-dstx))
          #;(printf "inserted ~s~n" a-dstx)
          (void)) 
        (inner (void) on-structured-insert-after a-fcursor))
      
      (define/augment (on-structured-delete a-fcursor)
        (when (not (dstx-from-unstructured-editing? (cursor-dstx a-fcursor)))
          #;(printf "deleted ~s~n" (cursor-dstx a-fcursor))
          (void)) 
        (inner (void) on-structured-delete a-fcursor))
      
      
      (define/augment (after-structured-insert-before a-fcursor)
        (inner (void) after-structured-insert-before a-fcursor))
      
      (define/augment (after-structured-insert-after a-fcursor)
        (inner (void) after-structured-insert-after a-fcursor))
      
      (define/augment (after-structured-delete a-fcursor deleted-dstx)
        (inner (void) after-structured-delete a-fcursor deleted-dstx))
      
      
      ;; Just as an experiment, see that we can queue the following for interpretation.
      #;(thread (lambda ()
                  (let loop ()
                    (sleep 5)
                    (queue-for-interpretation! (make-No-op))
                    (loop))))))
  

  
  ;; dstx-from-unstructured-editing?: dstx -> boolean
  ;; Returns true if we're certain that the dstx came from intermediate insert-mode.
  (define (dstx-from-unstructured-editing? a-dstx)
    (dstx-property-ref a-dstx 'from-unstructured-editing (lambda ()
                                                           #f))))