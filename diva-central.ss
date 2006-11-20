(module diva-central mzscheme
  
  ;; Central location for global divascheme actions, like toggling divascheme
  (require (lib "class.ss")
           (lib "list.ss"))
  (provide make-diva-central-mixin)
  
  
  (define (make-diva-central-mixin)
    (let ([shared-diva-central (new diva-central%)])
      (lambda (super%)
        (class super%
          (super-new)
          (define/public (get-diva-central) shared-diva-central)))))
  
  
  (define diva-central%
    (class object%
      (super-new)
      
      (define listeners empty)
      
      (define/public (add-listener listener)
        (set! listeners (cons listener listeners)))
      
      (define/public (notify event)
        (for-each (lambda (l) (l event)) listeners)))))