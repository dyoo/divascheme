(module diva-central mzscheme
  
  ;; Central location for triggering global divascheme on-off behavior.
  (require (lib "class.ss")
           (lib "list.ss"))
  
  (provide diva-central%
           make-diva-central-mixin
           (struct diva-switch-on-evt ())
           (struct diva-switch-off-evt ())
           #;(struct diva-label-evt (label)))
  
  (define-struct diva-switch-on-evt ())
  (define-struct diva-switch-off-evt ())  
  
  (define (make-diva-central-mixin shared-diva-central)
    (lambda (super%)
      (class super%
        (super-new)
        (define/public (get-diva-central)
          shared-diva-central))))
  
  
  (define diva-central%
    (class object%      
      (define listeners empty)
      (define divascheme-is-on? #f)
      (super-new)
      
      (define/public (add-listener listener)
        (set! listeners (cons listener listeners)))
      
      (define/public (remove-listener listener)
        (set! listeners (remq listener listeners)))
      
      (define (notify event)
        (for-each (lambda (l) (l event))
                  listeners))
      
      (define/public (switch-toggle)
        (cond
          [divascheme-is-on? (switch-off)]
          [else (switch-on)]))
      
      (define/public (diva-on?)
        divascheme-is-on?)
      
      (define/public (switch-on)
        (notify (make-diva-switch-on-evt))
        (set! divascheme-is-on? #t))
      
      (define/public (switch-off)
        (notify (make-diva-switch-off-evt))
        (set! divascheme-is-on? #f)))))