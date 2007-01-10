(module diva-file-menu mzscheme
  (require (lib "class.ss")
           (lib "plt-match.ss")
           (lib "mred.ss" "mred")
           "diva-central.ss"
           (prefix stags-gui: "tag-gui.ss"))
  
  (provide diva:menu-option-frame-mixin)
  
  (define enable-divascheme-msg "Enable DivaScheme")
  (define disable-divascheme-msg "Disable DivaScheme")
  (define generate-stags-msg "Generate Navigation Tags...")
  
  ;; We add a small menu option here for people who can't press F4.
  (define (diva:menu-option-frame-mixin super%)
    (class super%
      (inherit get-diva-central)
      (define enable/disable-menu-item #f)
      
      ;; Note: this call to super-new must come AFTER the variable definition
      ;; above or weird things happen.  See:
      ;; http://list.cs.brown.edu/pipermail/plt-scheme/2006-November/015413.html
      (super-new)
      
      (define/override (file-menu:between-print-and-close menu)
        (super file-menu:between-print-and-close menu)
        (set! enable/disable-menu-item
              (new menu-item%
                   [label (if (send (get-diva-central) diva-on?)
                              disable-divascheme-msg
                              enable-divascheme-msg)]
                   [parent menu]
                   [callback
                    (lambda (menu-item control-event)
                      (send (get-diva-central) switch-toggle))]))
        (new menu-item%
             [label generate-stags-msg]
             [parent menu]
             [callback
              (lambda (menu-item control-event)
                (stags-gui:generate-navigation-tags-dialog))]))
      
      
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
        (send (get-diva-central) remove-listener handle-diva-central-evt))
      
      (send (get-diva-central) add-listener handle-diva-central-evt))))