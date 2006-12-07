(module diva-preferences-gui mzscheme
  (require (lib "class.ss")
           (lib "list.ss")
           (lib "framework.ss" "framework")
           (lib "mred.ss" "mred"))
  
  
  (provide preference-panel-view%)
  
  ;; Some test code to take a look at the view
  (define (test:show-view)
    (define f (new frame% [label "test"]))
    (define v (new preference-panel-view%
                   [parent f]
                   [border 10]))
    (send f show #t)
    v)
  
  
  (define preference-panel-model%
    (class object%
      (super-new)
      (init diva-central)
      
      (define -diva-central diva-central)
      (define command-mode-text "")
      (define insert-mode-text "")
      (define globals-text "")
      (define preferred-keyboard-layout "")
      
      (define/public (set-command-mode-text text) (set! command-mode-text text))
      (define/public (set-insert-mode-text text) (set! insert-mode-text text))
      (define/public (set-globals-text text) (set! globals-text text))
      
      (define/public (get-command-mode-text) command-mode-text)
      (define/public (get-insert-mode-text) insert-mode-text)
      (define/public (get-globals-text) globals-text)
      
      (define (string->sexp text)
        (let ([ip (open-input-string (format "(~a)" text))])
          (first (read ip))))
      
      (define/public (save-to-preferences)
        ;; fill me in
        (preferences:set 'divascheme:preferred-keyboard-layout
                         (string->symbol preferred-keyboard-layout))
        (preferences:set 'divascheme:command-mode-bindings
                         (string->sexp command-mode-text))
        (preferences:set 'divascheme:insert-mode-bindings
                         (string->sexp insert-mode-text))
        (preferences:set 'divascheme:global-bindings
                         (string->sexp globals-text))
        (send -diva-central keymap-changed))
      
      (define/public (load-from-preferences)
        (void))
      ))
  
  
  
  (define preference-panel-view%
    (class vertical-panel%
      (super-new)
      
      
      (define/public (set-on-display-choice-changed-callback callback)
        (set! on-display-choice-changed callback))
      
      (define/public (set-on-keyboard-choice-changed-callback callback)
        (set! on-keyboard-choice-changed callback))
      
      
      ;; on-display-choice-changed: string -> void
      (define on-display-choice-changed
        (lambda (choice) (void)))
      
      
      ;; on-keyboard-choice-changed: string -> void
      (define on-keyboard-choice-changed
        (lambda (choice)
          (void)))
      
      
      (define display-choice
        (new choice% [label "View keybindings for "]
             [choices '("Command Mode" "Insert Mode" "Globals")]
             [parent this]
             [callback (lambda (c e)
                         (on-display-choice-changed (send c get-string-selection)))]))
      
      (define keybindings-text
        (new text-field% [label ""] [parent this] [style '(multiple)]))
      
      (define reset-panel
        (new horizontal-panel% [parent this] [stretchable-height #f]))
      
      (define keyboard-choice (new choice%
                                   [label "Set keybinding to built-in layout"]
                                   [choices '("Qwerty" "Dvorak" "orbiTouch")]
                                   [parent reset-panel]
                                   [callback
                                    (lambda (c e)
                                      (on-keyboard-choice-changed (send c get-string-selection)))]))
      
      (define choices-button
        (new button%
             [label "Set keybindings"]
             [parent reset-panel])))))