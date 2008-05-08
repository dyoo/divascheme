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
  
  
  (define (diva-panel:frame-mixin super%)
    (class super%
      (inherit ;get-canvas
        ;caret-hidden? 
        ;blink-caret
        get-diva-central
        get-area-container)
      
      
      ;; defining and adding the container of the DivaStuffs.
      (define diva-container-panel #f)
      ;; The Voice Label, Message and Question Panel.
      (define diva-label/message/question-panel #f)
      
      
      (define (initialize)
        (super-instantiate ())
        (set! diva-container-panel
              (new vertical-panel%
                   [parent (get-area-container)]
                   [stretchable-height false]
                   [stretchable-width true]))
        (set! diva-label/message/question-panel
              (make-object voice-label/message/question-panel% diva-container-panel))
        
        ;; Initially, hide the panel.
        (diva-panel-hide))
      
      
      (define (diva-displayed?)
        (member diva-container-panel (send (get-area-container) get-children)))
      
      (define/public (diva-panel-show)
        (diva-label "DivaScheme: command mode")
        (diva-message "")
        (unless (diva-displayed?)
          (send (get-area-container) change-children (lambda (children) `(,@children ,diva-container-panel)))))
      
      (define/public (diva-panel-hide)
        (when (diva-displayed?)
          (send (get-area-container) change-children (lambda (children) (remq diva-container-panel children)))))
      
      
      
      (define/public (diva-label label)
        (send diva-label/message/question-panel voice-label label))
      
      (define/public (diva-message message . args)
        (send/apply diva-label/message/question-panel voice-message message args))
      
      (define/public (diva-question question default cancel answer)
        (send diva-label/message/question-panel voice-question question default cancel answer))
      
      (initialize))))
