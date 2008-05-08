(module diva-panel mzscheme
  (require (lib "etc.ss")
	   (lib "list.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework"))
  
  (provide diva-panel:frame-mixin)
  
  
  ;; Provides a bottom frame for informative messages and prompts for DivaScheme.
  ;; This panel can show and hide.
  (define (diva-panel:frame-mixin super%)
    (class super%
      (inherit get-area-container)
      
      ;; defining and adding the container of the DivaStuffs.
      (define diva-container-panel #f)
      ;; The Voice Label, Message and Question Panel.
      (define diva-label/message/question-panel #f)
      
      
      (define (initialize)
        (super-instantiate ())
        (set! diva-container-panel
              (new horizontal-panel%
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
      
      (initialize)))
  
  
  
  ;;
  ;; THE MESSAGE PANEL
  ;;
  
  ;; This is where the engine gives feedback to the user, errors and so on.
  ;; The message panel is divised in three subpanels:
  ;;  - one to indicate the current mode (voice, command, insert)
  ;;  - one for giving feedbacks to the user
  ;;  - one for answering questions
  ;; All These panels need a super panel which contains them.
  
  (define voice-label/message/question-panel%
    (class object%
      (init parent)
      
      
      (define voice-label-width 200)
      (define voice-label/message/question-panel #f)
      ;; The panel showing the current mode.
      (define voice-label-msg #f)
      ;; The feedback panel.
      (define voice-message-msg #f)
      (define voice-question-panel #f)
      (define voice-question-msg #f)
      ;; The panel to answer questions need an object where the user can type.
      (define voice-question-text #f)
      ;; The answering panel.
      (define voice-question-canvas #f)
      
      
      
      (define (initialize)
        (super-new)
        (set! voice-label/message/question-panel
              (new horizontal-panel%
                   [parent parent]
                   [stretchable-height false]
                   [stretchable-width true]
                   [alignment '(left center)]
                   [horiz-margin 10]))
        (set! voice-label-msg
              (new message%
                   [label ""]
                   [parent voice-label/message/question-panel]
                   [stretchable-width true]
                   [stretchable-height false]))
        (set! voice-message-msg
              (new message%
                   [label ""]
                   [parent voice-label/message/question-panel]
                   [stretchable-width true]
                   [stretchable-height false]))
        (set! voice-question-panel
              (new horizontal-panel%
                   [parent voice-label/message/question-panel]
                   [stretchable-height false]
                   [stretchable-width true]
                   [alignment '(right center)]
                   [horiz-margin 10]))
        (set! voice-question-msg
              (new message%
                   [label ""]
                   [parent voice-question-panel]
                   [stretchable-width true]
                   [stretchable-height false]))
        (set! voice-question-text
              (new text%))
        (set! voice-question-canvas
              (new editor-canvas%
                   [parent voice-question-panel]
                   [editor voice-question-text]
                   [style '(no-hscroll no-vscroll)]
                   [stretchable-width true]
                   [stretchable-height false]
                   [min-width 0]
                   [line-count 1]))
        
        (send voice-label-msg show false)
        (send voice-question-canvas show false)
        (send voice-label/message/question-panel min-height (+ 8 (send voice-question-canvas min-height))))
      
      
      
      
      
      
      
      (define (make-voice-question-text-keymap cancel answer)
        (let ([question-text-keymap (make-object keymap:aug-keymap%)])
          (send question-text-keymap add-function "voice-question-text:cancel"
                (lambda (any event)
                  (cancel)))
          (send question-text-keymap add-function "voice-question-text:answer"
                (lambda (any event)
                  (answer)))
          (send question-text-keymap map-function "space" "voice-question-text:answer")
          (send question-text-keymap map-function "tab" "voice-question-text:answer")
          (send question-text-keymap map-function "enter" "voice-question-text:answer")
          (send question-text-keymap map-function "numpadenter" "voice-question-text:answer")
          (send question-text-keymap map-function "esc" "voice-question-text:cancel")
          (send question-text-keymap map-function "c:g" "voice-question-text:cancel")
          question-text-keymap))
      
      
      (define (voice-label-hide)
        (send voice-label-msg min-width 0)
        (send voice-label-msg show false))
      
      (define (voice-label-show)
        (send voice-label-msg min-width voice-label-width)
        (send voice-label-msg show true))
      
      (define (voice-label-shown?)
        (send voice-label-msg is-shown?))
      
      
      (define/public (voice-label label)
        (if label
            (begin
              (send voice-label-msg set-label
                    (substring label 0 (min voice-label-width (string-length label))))
              (voice-label-show))
            (begin
              (voice-label-hide))))
      
      
      
      (define (set-message-text text message . args)
        (let* ([message (apply format message args)]
               [short-message (substring message 0 (min 200 (string-length message)))])
          (send text min-width (string-length short-message))
          (send text set-label short-message)))
      
      ;; The function to show a message.
      (define/public (voice-message message . args)
        (apply set-message-text voice-message-msg message args))
      
      
      (define (voice-question-prompt message . args)
        (apply set-message-text voice-question-msg message args))
      
      
      ;; The function to show the question panel.
      (define voice-question-panel-show
        (lambda ()
          (send voice-question-panel show #t)
          (send voice-question-canvas min-width 180)
          (send voice-question-canvas show true)
          (send voice-question-canvas focus)))
      
      ;; The function to hide the question panel.
      (define voice-question-panel-hide
        (lambda ()
          (send voice-question-panel show #f)
          (send voice-question-canvas min-width 0)
          (send voice-question-canvas show false)))
      
      ;; The function to ask a question.
      ;; This is a callback function,
      ;; i.e. it is waiting for the user
      ;; and we cannot afford to wait for the user.
      ;; So when the user decided, we execute the associated function,
      ;; either no response, either an answer, and on function for each behavior.
      (define/public (voice-question question default cancel answer)
        (let* ([handle-voice-label (if (voice-label-shown?)
                                       (lambda () (voice-label-show))
                                       (lambda () ()))]
               [answer (lambda ()
                         (handle-voice-label)
                         (voice-question-panel-hide)
                         (voice-question-prompt "")
                         (answer (send voice-question-text get-text)))]
               [cancel (lambda ()
                         (handle-voice-label)
                         (voice-question-panel-hide)
                         (voice-question-prompt "")
                         (cancel))]
               [keymap (make-voice-question-text-keymap cancel answer)])
          (when (voice-label-shown?)
            (voice-label-hide))
          (voice-message "") ;; TODO: should we really clear voice-message here?
          (voice-question-prompt (format "~a: " question))
          (send voice-question-text set-keymap keymap)
          (send voice-question-text erase)
          (send voice-question-text insert default)
          (send voice-question-text set-position 0 (string-length default) false false 'local)
          (voice-question-panel-show)))
      
      (initialize))))
