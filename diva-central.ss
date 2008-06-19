(module diva-central mzscheme
  
  ;; Central location for triggering global divascheme on-off behavior and other
  ;; events.
  
  (require (lib "class.ss")
           (lib "list.ss")
           (lib "mred.ss" "mred")
           (lib "string-constant.ss" "string-constants")
           "language.ss")
  
  (provide diva-central%
           make-diva-central-mixin
           (struct diva-switch-on-evt ())
           (struct diva-switch-off-evt ())
           (struct diva-keymap-changed-evt ())
           #;(struct diva-label-evt (label)))
  
  ;; The following global events can be triggered:
  ;;
  ;; diva-switch-on-evt: a request to turn DivaScheme on.
  ;; diva-switch-off-evt: a request to turn DivaScheme off.
  ;; diva-keymap-changed-evt: a request to refresh the keymap since something's changed.
  ;;
  ;; A shared diva-central% is passed to the toplevel components of DivaScheme, and
  ;; those components register themselves in to pay attention to announcements.
  
  (define-struct diva-switch-on-evt ())
  (define-struct diva-switch-off-evt ())  
  (define-struct diva-keymap-changed-evt ())
  
  ;; make-diva-central-mixin: diva-central% -> mixin
  (define (make-diva-central-mixin shared-diva-central)
    (lambda (super%)
      (class super%
        (super-new)
        (define/public (get-diva-central)
          shared-diva-central))))
  
  
  (define diva-central%
    (class object%      
      (define listeners empty)
      (define divascheme-is-on? #f)
      (super-new)
      
      (define/public (add-listener listener)
        (set! listeners (cons listener listeners)))
      
      (define/public (remove-listener listener)
        (set! listeners (remq listener listeners)))
      
      
      (define (notify event)
        (for-each (lambda (l) (l event))
                  listeners))
      
      ;; switch-toggle: -> void
      ;; Turns DivaScheme either on or off, depending on the current status.
      (define/public (switch-toggle)
        (cond
          [divascheme-is-on? (switch-off)]
          [else (switch-on)]))
      
      
      ;; diva-on?: -> boolean
      ;; Returns true if DivaScheme is on.  Otherwise, returns false.
      (define/public (diva-on?)
        divascheme-is-on?)

      ;; allow-enable?: boolean
      ;; Returns true if we're allowed to enable DivaScheme.
      (define (allow-enable?)
        (not (string=? (string-constant no-language-chosen)
             (get-language-name))))
      
      ;; switch-on: -> void 
      (define/public (switch-on)
        (cond
          [(allow-enable?)
           (notify (make-diva-switch-on-evt))
           (set! divascheme-is-on? #t)]
          [else
           (message-box
            "Error" "A language level should be selected first.")]))
      
      ;; switch-off: -> void 
      (define/public (switch-off)
        (notify (make-diva-switch-off-evt))
        (set! divascheme-is-on? #f))
      
      (define/public (keymap-changed)
        (notify (make-diva-keymap-changed-evt))))))