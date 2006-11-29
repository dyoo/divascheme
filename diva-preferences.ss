(module diva-preferences mzscheme
  (require (lib "plt-match.ss")
           (lib "class.ss")
           (lib "framework.ss" "framework")
           (lib "mred.ss" "mred")
           (lib "pretty.ss")
           "diva-central.ss")
  (provide install-diva-central-handler
           enable-on-startup?
           command-mode-bindings
           add-keymap-command-toggle
           add-preference-panel)
  
  ;; install-diva-central-handler: diva-central% -> void
  ;; Adds a diva-central listener that watches when
  ;; DivaScheme is turned on and off, and reacts by updating
  ;; the divascheme:on? preference.
  (define (install-diva-central-handler diva-central)
    (define (handler evt)
      (match evt
        [(struct diva-switch-on-evt ())
         (preferences:set 'divascheme:on? #t)]
        [(struct diva-switch-off-evt ())
         (preferences:set 'divascheme:on? #f)]
        [else (void)]))
    (send diva-central add-listener handler))
  
  
  ;; enable-on-startup?: -> boolean
  ;; Should return true if we should turn Divascheme on.
  (define (enable-on-startup?)
    (preferences:get 'divascheme:on?))
  
  (preferences:set-default 'divascheme:on? #f boolean?)
  
  
  
  ;; command-mode-bindings: -> (listof (cons string (cons string empty)))
  ;; Returns the key-to-function-name bindings as a list of string pairs.
  (define (command-mode-bindings)
    (preferences:get 'divascheme:command-mode-bindings))
  
  (define default-command-mode-bindings
    '(("return" "diva:enter")
      ("numpadenter" "diva:enter")
      ("tab" "diva:indent")
      ("h" "diva:before-this")
      ("semicolon" "diva:after-this")
      ("r" "diva:insert")
      ("k" "diva:down")
      ("i" "diva:up")
      ("s:k" "diva:out")
      ("j" "diva:backward")
      ("l" "diva:forward")
      ("n" "diva:next")
      ("p" "diva:previous")
      ("s" "diva:select")
      ("c" "diva:copy")
      ("x" "diva:cut")
      ("v" "diva:paste")
      ("u" "diva:undo")
      ("z" "diva:cancel")
      ("y" "diva:redo")
      ("d" "diva:delete")
      ("b" "diva:bring")
      ("s:b" "diva:push")
      ("s:x" "diva:exchange")
      ("m" "diva:mark")
      ("s:m" "diva:unmark")
      ("s:h" "diva:holder")
      ("t" "diva:transpose")
      ("." "diva:find-tag")
      ("o" "diva:join")
      ("[" "diva:open")
      ("(" "diva:open")
      ("{" "diva:open-square")
      ("]" "diva:close")
      (")" "diva:close")
      ("a" "diva:younger")
      ("e" "diva:older")
      ("space" "diva:extend-selection")
      ("w" "diva:edit-symbol")
      
      #|
("insert" "diva:backward")
("f3" "diva:before-this")
("f5" "diva:up")
("f7" "diva:after-this")
("f9" "diva:forward")
("=" "diva:down")

("8" "diva:out")
("0" "previous-page")
("6" "next-page")
("7" "forward-character")
("9" "backward-character")
|#
      
      ;; Keyboard shortcuts for use with the orbiTouch keyboard
      ("0" "diva:backward")
      ("9" "diva:before-this")
      ("8" "diva:up")
      ("7" "diva:after-this")
      ("6" "diva:forward")
      ("=" "diva:down")
      ("3" "diva:out")
      ("5" "previous-page")
      ("1" "next-page")
      ("2" "forward-character")
      ("4" "backward-character")))
  
  (preferences:set-default 'divascheme:command-mode-bindings
                           default-command-mode-bindings
                           list?)
  
  
  (define (add-keymap-command-toggle keymap diva-central)
    (send keymap map-function "f4" "diva:toggle"))
  
  
  ;; add-preference-panel: diva-central% -> void
  ;; Attaches a DivaScheme preference panel for user-editable preferences.
  (define (add-preference-panel diva-central)
    
    (define (sexp->string sexp)
      (let ([op (open-output-string)])
        (pretty-print sexp op)
        (get-output-string op)))
    
    (define (make-keymap-subpanel title property parent default-title default-bindings)
      (let* ([panel (new group-box-panel%
                         [parent parent]
                         [label title])]
             [current-text
              (lambda () (sexp->string (preferences:get property)))]
             [text-field
              (new text-field% [label ""] [parent panel] [style '(multiple)])]
             [update-keybindings-text
              (lambda (text)
                (send text-field set-value text)
                (send (send text-field get-editor) set-position 0))]
             [default-command-keybindings-panel
               (new horizontal-panel% [parent panel])]
             [reset-to-default-button
              (new button%
                   [label default-title]
                   [parent default-command-keybindings-panel]
                   [callback
                    (lambda (button event)
                      (update-keybindings-text (sexp->string default-bindings)))])])
        (preferences:add-callback property
                                  (lambda (p f)
                                    (update-keybindings-text (sexp->string f))
                                    (send diva-central keymap-changed)))
        (preferences:add-on-close-dialog-callback
         (lambda ()
           (when (not (string=? (current-text)
                                (sexp->string (send text-field get-value))))
             (let ([ip (open-input-string (send text-field get-value))])
               (preferences:set 'divascheme:command-mode-bindings (read ip))))))
        (update-keybindings-text (current-text))
        panel))
    
    
    (preferences:add-panel
     "DivaScheme"
     (lambda (p-frame)
       (let* ([parent (new vertical-panel% [parent p-frame])]
              [command-keybindings-panel
               (make-keymap-subpanel "Command Keybindings"
                                     'divascheme:command-mode-bindings
                                     parent
                                     "Reset to default command keybindings"
                                     default-command-mode-bindings)])
         parent)))))