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
           "diva-file-menu.ss"
           (prefix preferences: "diva-preferences.ss")
           (prefix marker: "marker.ss")
           "tag-gui.ss")
  (current-inspector oi)
  (print-struct #t)
  
  (provide tool@)
  
  
  
  
  
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
      
      
      (define shared-diva-central (new diva-central%))
      
      (define (phase1)
        (let ([diva-central-mixin (make-diva-central-mixin shared-diva-central)])
          
          (define (diva-definitions-canvas-mixin super%)
            (diva-link:canvas-mixin
             (diva-central-mixin super%)))
          
          (define (diva-definitions-text-mixin super%)
            (diva-link:text-mixin
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
          
          (preferences:install-diva-central-handler shared-diva-central)))
      
      
      (define (phase2)
        (queue-callback
         (lambda ()
           (when (preferences:enable-on-startup?)
             (send shared-diva-central switch-on)))
         #f))))
  
  
  (define tool@
    (compound-unit/sig 
      (import (DRSCHEME-TOOL : drscheme:tool^))
      (link (LANGUAGE : language^ (language@ DRSCHEME-TOOL))
	    (VOICE : drscheme:tool-exports^ (voice@ DRSCHEME-TOOL)))
      (export (open VOICE)))))