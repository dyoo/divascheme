(module tool mzscheme
  (define oi (current-inspector))
  (current-inspector (make-inspector))
  (require (lib "unitsig.ss")
           (lib "tool.ss" "drscheme")
           (lib "framework.ss" "framework")
           (lib "mred.ss" "mred")
           "language.ss" (lib "unitsig.ss")
	   (lib "class.ss")
           "diva-panel.ss"
           "diva-link.ss"
           "mred-callback.ss"
           "diva-central.ss"
           (prefix marker: "marker.ss")
           "tag-gui.ss")
  (current-inspector oi)
  (print-struct #t)
  
  (provide tool@)
  
  
  ;; We add a small menu option here for people who can't press F4.
  (define (diva:menu-option-frame-mixin super%)
    (class super%
      (override file-menu:between-print-and-close)
      (super-new)
      
      (define (file-menu:between-print-and-close menu)
        (super file-menu:between-print-and-close menu)
        (new menu-item% 
             [label "Enable/Disable DivaScheme"]
             [parent menu]
             [callback 
              (lambda (menu-item control-event)
                (let ([defns
                        (send (get-top-level-focus-window) 
                              get-definitions-text)])
                  (send defns toggle-divascheme)))]))))
  
  
  (define voice@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
      
      ;; ~H~
      
      ;; The definitions and interactions windows have two classes :  the text one and the canvas one.
      ;; The canvas is the containing object and the text is the contained object.
      ;; For one window of DrScheme, there is only one canvas for the definitions window and another one
      ;; for the interactions window, whatever the number of table is.
      ;; At the opposite, there is one text object for each tab, that is there are as many text objects as there are tabs.
      ;; Thus, when one switch from one tab to another one, only the content of the definitions window and the interactions window are changed:
      ;; the field of the canvas object of what it is currently displaying is changed, the previous text object is replaced by the next text object.

      ;; Currently, each class - mixin - is overloaded twice: a panel overloading and a mred overloading.
      ;; The Panel overloading deals with input stuffs, with giving a command to DivaScheme: hitting F4, the DivaBox, etc..
      ;; The MrEd overloading deals with output stuffs, with the actions to be performed on the text according to the given commmand.

      
      (define (phase1)
        (let ([diva-central-mixin (make-diva-central-mixin)])
          
          (define (diva-definitions-canvas-mixin super%)
            (diva-link:canvas-mixin
             (diva-central-mixin super%)))
          
          (define (diva-definitions-text-mixin super%)
            (diva-link:text-mixin
             keymap:remove-chained-keymap
             (voice-mred-text-callback-mixin
              (marker:marker-mixin
               (diva-central-mixin super%)))))
          
          (define (diva-frame-mixin super%)
            (diva-panel:frame-mixin
             (tag-gui-unit:frame-mixin
              (diva:menu-option-frame-mixin
               (diva-central-mixin super%)))))
          
          (drscheme:get/extend:extend-unit-frame diva-frame-mixin)
          (drscheme:get/extend:extend-definitions-canvas diva-definitions-canvas-mixin)
          (drscheme:get/extend:extend-definitions-text diva-definitions-text-mixin)
          
          (preferences:set-default 'divascheme:on? #f boolean?)))
      
      
      (define phase2 void)))
  
  
  (define tool@
    (compound-unit/sig 
      (import (DRSCHEME-TOOL : drscheme:tool^))
      (link (LANGUAGE : language^ (language@ DRSCHEME-TOOL))
	    (VOICE : drscheme:tool-exports^ (voice@ DRSCHEME-TOOL)))
      (export (open VOICE)))))