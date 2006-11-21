(module diva-central mzscheme
  
  ;; Central location for global divascheme actions, like toggling divascheme
  (require (lib "class.ss")
           (lib "list.ss"))
  
  (provide diva-central%
           make-diva-central-mixin
           diva-switch-on-evt
           diva-switch-off-evt)
  
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
      
      (define (notify event)
        (for-each (lambda (l) (l event)) listeners))
      
      (define/public (switch-toggle)
        (cond
          [divascheme-is-on? (switch-off)]
          [else (switch-on)]))
      
      (define/public (diva-on?)
        divascheme-is-on?)
      
      (define/public (switch-on)
        (set! divascheme-is-on? #t)
        (notify (make-diva-switch-on-evt)))
      
      (define/public (switch-off)
        (set! divascheme-is-on? #f)
        (notify (make-diva-switch-off-evt))))))