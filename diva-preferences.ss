(module diva-preferences mzscheme
  (require (lib "plt-match.ss")
           (lib "class.ss")
           (lib "framework.ss" "framework")
           "diva-central.ss")
  (provide install-diva-central-handler)
  
  (define (install-diva-central-handler diva-central)
    (define (handler evt)
      (match evt
        [(struct diva-switch-on-evt ())
         (preferences:set 'divascheme:on? #t)]
        [(struct diva-switch-off-evt ())
         (preferences:set 'divascheme:on? #f)]))
    (send diva-central add-listener handler))
  
  
  (preferences:set-default 'divascheme:on? #f boolean?))