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
        
        
        ;; Initially, we hide the panel.
        (diva-panel-hide))
      
      
      ;; diva-panel-show: -> void
      ;; Makes the panel visible.
      (define/public (diva-panel-show)
        (diva-label "DivaScheme: command mode")
        (diva-message "")
        (send (get-area-container) change-children
              (lambda (children) `(,@children ,diva-container-panel))))
      
      
      ;; diva-panel-hide: -> void
      ;; Hides the panel from view.
      (define/public (diva-panel-hide)
        (send (get-area-container) change-children
              (lambda (children) (remq diva-container-panel children))))
      
      
      ;; diva-label: string -> void
      ;; Sets the label on the left side of the panel.
      (define/public (diva-label label)
        (send diva-label/message/question-panel voice-label label))
      
      
      ;; diva-message: format (listof string) -> void
      ;; Displays a message on the right hand side of the panel.
      (define/public (diva-message message . args)
        (send/apply diva-label/message/question-panel voice-message message args))
      
      
      ;; diva-question: string string ??? ??? -> void
      ;; Prompts the user for an answer.
      (define/public (diva-question question default cancel answer)
        (send diva-label/message/question-panel voice-question question default cancel answer))
      
      (initialize))))
