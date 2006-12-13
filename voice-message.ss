(module voice-message mzscheme
  (require (lib "etc.ss")
           (lib "class.ss")
           (lib "framework.ss" "framework")
           (lib "mred.ss" "mred")
           "utilities.ss")

  ;;
  ;; THE MESSAGE PANEL
  ;; 
  
  ;; This is where the engine gives feedback to the user, errors and so on.
  ;; The message panel is divised in three subpanels:
  ;;  - one to indicate the current mode (voice, command, insert)
  ;;  - one for giving feedbacks to the user
  ;;  - one for answering questions
  ;; All These panels need a super panel which contains them.
  (provide voice-label/message/question-panel%)
  (define voice-label/message/question-panel%
    (class object%
      (init parent)
      (super-new)
      
      (define voice-label-width 200)
      
      (define voice-label/message/question-panel
        (new horizontal-panel%
             [parent parent]
             [stretchable-height false]
             [stretchable-width true]
             [alignment '(left center)]
             [horiz-margin 10]))
      
      ;; The panel showing the current mode.
      (define voice-label-msg
        (new message%
             [label ""]
             [parent voice-label/message/question-panel]
             [stretchable-width true]
             [stretchable-height false]))
      
      ;; The feedback panel.
      (define voice-message-msg
        (new message% 
             [label ""]
             [parent voice-label/message/question-panel]
             [stretchable-width true]
             [stretchable-height false]))
      
      
      (define voice-question-panel
        (new horizontal-panel%
             [parent voice-label/message/question-panel]
             [stretchable-height false]
             [stretchable-width true]
             [alignment '(right center)]
             [horiz-margin 10]))
      
      (define voice-question-msg
        (new message%
             [label ""]
             [parent voice-question-panel]
             [stretchable-width true]
             [stretchable-height false]))
      
      ;; The panel to answer questions need an object where the user can type.
      (define voice-question-text
        (new text%))
      
      ;; The answering panel.
      (define voice-question-canvas
        (new editor-canvas%
             [parent voice-question-panel]
             [editor voice-question-text]
             [style '(no-hscroll no-vscroll)]
             [stretchable-width true]
             [stretchable-height false]
             [min-width 0]
             [line-count 1]))
      
      (define make-voice-question-text-keymap
        (lambda (cancel answer)
          (let ([question-text-keymap (make-object keymap:aug-keymap%)])
            (send question-text-keymap add-function "voice-question-text:cancel"
                  (lambda (any event)
                    (cancel)))
            (send question-text-keymap add-function "voice-question-text:answer"
                  (lambda (any event)
                    (answer)))
            (send question-text-keymap map-function "space" "voice-question-text:answer")
            (send question-text-keymap map-function "tab"   "voice-question-text:answer")
            (send question-text-keymap map-function "enter" "voice-question-text:answer")
            (send question-text-keymap map-function "numpadenter" "voice-question-text:answer")
            (send question-text-keymap map-function "esc"   "voice-question-text:cancel")
            (send question-text-keymap map-function "c:g"   "voice-question-text:cancel")
            question-text-keymap)))
      
      ;; Configuration of these panels.
      (send voice-label-msg show false)
      (send voice-question-canvas show false)
      (send voice-label/message/question-panel min-height (+ 8 (send voice-question-canvas min-height)))
      
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
              (send voice-label-msg set-label (substring label 0 (min voice-label-width (string-length label))))
              (voice-label-show))
            (begin
              (voice-label-hide))))

      ;; The function to show a message.
      (define/public (voice-message message . args)
        (let* ([message (apply format message args)]
               [text (substring message 0 (min 200 (string-length message)))])
          (send voice-message-msg min-width (string-length text))
          (send voice-message-msg set-label text)))
      
      
      (define/public (voice-question-prompt message . args)
        (let* ([message (apply format message args)]
               [text (substring message 0 (min 200 (string-length message)))])
          (send voice-question-msg min-width (string-length text))
          (send voice-question-msg set-label text)))
      
      
      ;; The function to show the question panel.
      (define voice-question-panel-show
        (lambda ()
          (send voice-question-canvas min-width 180)
          (send voice-question-canvas show true)
          (send voice-question-canvas focus)))
      
      ;; The function to hide the question panel.
      (define voice-question-panel-hide
        (lambda ()
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
          (voice-message "")
          (voice-question-prompt (format "~a: " question))
          (send voice-question-text set-keymap keymap)
          (send voice-question-text erase)
          (send voice-question-text insert default)
          (send voice-question-text set-position 0 (string-length default) false false 'local)
          (voice-question-panel-show))))))