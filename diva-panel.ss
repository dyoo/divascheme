(module diva-panel mzscheme
  (require (lib "etc.ss")
	   (lib "list.ss")
           (lib "class.ss")
           (lib "plt-match.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           "voice-message.ss"
           "diva-central.ss")
  
  (provide diva-panel:frame-mixin)
  
  ;;
  ;; TABS
  ;;
  
  ;; Here we are handling the tabs for the 300 versions of DrScheme.
  ;; Have a look at an example. We are considering the definitions window.
  ;; The definitions window is made of two classes, the canvas one and the text one.
  ;; The canvas stands for the containing box, the window,
  ;; and the text stands for the content of the window, i.e. the Scheme code which is currently edited.
  ;; When we are switching from a tab to another one, DrScheme changes only the content of the definitions window,
  ;; that is the text object. Thus in facts the field of the canvas object of the definitions window which knows
  ;; what to draw is replaced by the text object of the tab getting the focus.
  
  ;; There is a little problem/bug: in the documentation, set-editor takes two arguments
  ;;  (send an-editor-canvas set-editor edit redraw?) -> void 
  ;; but when DrScheme starts, it tells us that the method set-editor takes only one argument... :-(
  ;; A work around of that is our overriding method can accept two arguments, 
  ;; but call the super one with only one.
  
  ;; TODO : when leaving tab with DivaBox focus, when coming back the DivaBox should get the focus (not the definitions window as currently)

  ;; So what we want here is when we are switching from a tab to another one,
  ;; to save the current Diva-state of the window so that when we are coming back, the window is as we left it.
  ;; Thus, two things to be done:
  ;;  * save the current Diva-state
  ;;  * restore the Diva-state of the new content.
  ;; NB: we have to test if the previous edit and the new are not false because they can be so.
  ;;     for instance, when DrScheme starts, the canvas is made before the text, so the first value is false for the current edit.

 
  (define (diva-panel:frame-mixin super%)
    (class super%
      (inherit ;get-canvas
	       ;caret-hidden? 
	       ;blink-caret
        get-diva-central
        get-area-container)
      
      (super-instantiate ())



      
      (define area (get-area-container))
      
      ;; defining and adding the container of the DivaStuffs.
      (define diva-container-panel
        (new vertical-panel% 
             [parent area]
             [stretchable-height false]
             [stretchable-width true]))
      
      (define (diva-displayed?)
        (member diva-container-panel (send area get-children)))
      
      (define (diva-show)
        (diva-label "DivaScheme: command mode")
        (diva-message "")
        (unless (diva-displayed?)
          (send area change-children (lambda (children) `(,@children ,diva-container-panel)))))
      
      (define (diva-hide)
        (when (diva-displayed?)
          (send area change-children (lambda (children) (remq diva-container-panel children)))))
            
      
      ;; The Voice Label, Message and Question Panel.
      (define diva-label/message/question-panel
        (make-object voice-label/message/question-panel% diva-container-panel))
      
      (define (diva-label label)
        (send diva-label/message/question-panel voice-label label))
      
      (define/public (diva-message message . args)
        (send/apply diva-label/message/question-panel voice-message message args))
      
      (define/public (diva-question question default cancel answer)
        (send diva-label/message/question-panel voice-question question default cancel answer))
      
      
      (define (diva-central-handler evt)
        (match evt
          [(struct diva-switch-on-evt ())
           (diva-show)]
          [(struct diva-switch-off-evt ())
           (diva-hide)]
          [(struct diva-label-evt (label))
           (diva-label label)]
          [else (void)]))
      (send (get-diva-central) add-listener diva-central-handler)
      (when (send (get-diva-central) diva-on?)
        (diva-show)))))
