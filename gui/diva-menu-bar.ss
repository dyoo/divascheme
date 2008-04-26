(module diva-menu-bar mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "plt-match.ss")
           "../diva-central.ss"
           (prefix stags-gui: "tag-gui.ss"))
  
  (provide diva-menu-bar-mixin)
  
  ;; This adds another menu bar specifically for DivaScheme stuff.
  ;; Base functionality includes adding a disable/enable for DivaScheme,
  ;; and options to load stags.
  
  
  (define enable-divascheme-msg "Enable DivaScheme")
  (define disable-divascheme-msg "Disable DivaScheme")
  (define generate-stags-msg "Generate Navigation Tags...")
  
  
  ;; diva-menu-bar-mixin: frame:basic<%> -> frame:basic<%>
  (define (diva-menu-bar-mixin super%)
    (class super%
      (inherit get-menu-bar)
      
      ;; menu will be our personal menu, containing options
      ;; for disabling or enabling DivaScheme.
      (define diva-menu #f)
      
      ;; We keep status on if DivaScheme is on or not.
      (define enable/disable-menu-item #f)
      (define stags-menu-item #f)
      
      
      ;; initialize: -> void
      ;; Creates the new menu options, and attaches menu items for
      ;; enabling DivaScheme, loading stags.  Other mixins can
      ;; use get-diva-menu to add additional items to us.
      (define (initialize)
        (super-new)
        (set! diva-menu (new menu%
                             [label "DivaScheme"]
                             [parent (get-menu-bar)]))
        
        (set! enable/disable-menu-item
              (new menu-item%
                   [label (if (send (send this get-diva-central) diva-on?)
                              disable-divascheme-msg
                              enable-divascheme-msg)]
                   [parent diva-menu]
                   [callback
                    (lambda (menu-item control-event)
                      (send (send this get-diva-central) switch-toggle))]))
        
        (set! stags-menu-item (new menu-item%
                                   [label generate-stags-msg]
                                   [parent diva-menu]
                                   [callback
                                    (lambda (menu-item control-event)
                                      (stags-gui:generate-navigation-tags-dialog))]))
        
        (send (send this get-diva-central) add-listener handle-diva-central-evt))
      
      
      
      ;; get-diva-menu: -> menu%
      ;; Returns the menu.
      (define/public (get-diva-menu)
        diva-menu)
      
      
      
      ;; handle-diva-central-evt: event -> void
      ;; Handles global events fired off by diva-central.
      (define (handle-diva-central-evt evt)
        (match evt
          [(struct diva-switch-on-evt ())
           (when enable/disable-menu-item
             (send enable/disable-menu-item set-label disable-divascheme-msg))]
          
          [(struct diva-switch-off-evt ())
           (when enable/disable-menu-item
             (send enable/disable-menu-item set-label enable-divascheme-msg))]
          [else
           (void)]))
      
      (define/augment (on-close)
        (inner (void) on-close)
        (send (send this get-diva-central) remove-listener handle-diva-central-evt))
      
      
      
      (initialize))))