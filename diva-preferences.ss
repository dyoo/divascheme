(module diva-preferences mzscheme
  (require (lib "plt-match.ss")
           (lib "class.ss")
           (lib "framework.ss" "framework")
           "diva-central.ss")
  (provide install-diva-central-handler
           enable-on-startup?)
  
  (define (install-diva-central-handler diva-central)
    (define (handler evt)
      (match evt
        [(struct diva-switch-on-evt ())
         (preferences:set 'divascheme:on? #t)]
        [(struct diva-switch-off-evt ())
         (preferences:set 'divascheme:on? #f)]
        [else (void)]))
    (send diva-central add-listener handler))
  
  
  (preferences:set-default 'divascheme:on? #f boolean?)

  ;; enable-on-startup?: -> boolean
  ;; Should return true if we should turn Divascheme on.
  (define (enable-on-startup?)
    (preferences:get 'divascheme:on?))
  
  )