(module diva-preferences mzscheme
  (require (lib "plt-match.ss")
           (lib "class.ss")
           (lib "framework.ss" "framework")
           (lib "mred.ss" "mred")
           (lib "pretty.ss")
           (lib "list.ss")
           (lib "plt-match.ss")
           (lib "contract.ss")
           "diva-central.ss"
           "utilities.ss")
  
  (provide install-diva-central-handler
           enable-on-startup?
           
           forcefully-reset-to-defaults
           
           get-global-bindings
           get-command-mode-bindings
           get-insert-mode-bindings
           
           set-global-bindings
           set-command-mode-bindings
           set-insert-mode-bindings
           
           ;; keymap stuff
           install-global-bindings
           install-command-mode-bindings
           install-insert-mode-bindings
           
           add-preference-panel)
  
  (define default-command-mode-bindings/common
    '(("return" "diva:enter")
      ("numpadenter" "diva:enter")
      ("tab" "diva:indent")
      ("h" "diva:before-this")
      ("semicolon" "diva:after-this")
      ("r" "diva:insert")
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
      
      ("c:[" "diva:open-square")

      ("[" "diva:open-square/contextual")
      ("(" "diva:open")
      ("{" "diva:open-square")
      ("]" "diva:close")
      (")" "diva:close")
      ("}" "diva:close")
      ("space" "diva:extend-selection")
      ("esc" "diva:stop-extend-selection")
      ("w" "diva:edit-symbol")
      ("home" "diva:first")
      ("end" "dive:end")))
  
  (define default-command-mode-bindings/qwerty
    (append
     '(("k" "diva:down")
       ("i" "diva:up")
       ("s:k" "diva:out")
       ("j" "diva:backward")
       ("l" "diva:forward")
       ("a" "diva:younger")
       ("e" "diva:older"))
     default-command-mode-bindings/common))
  
  (define default-command-mode-bindings/orbitouch
    (append
     '(("0" "diva:backward")
       ("9" "diva:before-this")
       ("8" "diva:up")
       ("7" "diva:after-this")
       ("6" "diva:forward")
       ("=" "diva:down")
       ("3" "diva:out")
       ("5" "previous-page")
       ("1" "next-page")
       ("2" "forward-character")
       ("4" "backward-character"))
     default-command-mode-bindings/qwerty))
  
  ;; These keybindings were contributed by David Cabana.
  (define default-command-mode-bindings/dvorak
    (append
     '(("j" "diva:down")
       ("e" "diva:up")
       ("s:e" "diva:out")
       ("q" "diva:backward")
       ("k" "diva:forward")
       ("`" "diva:younger")
       ("," "diva:older"))
     default-command-mode-bindings/common))
  
  (define default-global-bindings
    '(("f4" "diva:toggle")))
  
  (define default-insert-mode-bindings
    '(("esc" "diva:exit")
      ("c:g" "diva:cancel")
      ("c:c" "diva:cancel")
      
      ("space" "diva:space")
      
      ("(" "diva:open")
      (")" "diva:close")

      ("[" "diva:open-square/contextual")
      ("]" "diva:close-square")
      ("{" "diva:open-curly")
      ("}" "diva:close-curly")
      
      ("enter" "diva:enter")
      ("numpadenter" "diva:enter")
      
      ("backspace" "diva:delete-backward")
      ("delete" "diva:delete-forward")
      ("c:d" "diva:delete-forward")
      
      ("m:d" "diva:kill-word-forward")
      ("m:backspace" "diva:kill-word-backward")
      
      ("m:b" "diva:bring")
      ("tab" "diva:pass")
      ((alt/meta-prefix "/") "diva:magic")
      
      ("left" "diva:left")
      ("right" "diva:right")
      ("up" "diva:up")
      ("down" "diva:down")
      ("c:b" "diva:left")
      ("c:f" "diva:right")
      
      ("c:left" "diva:left*")
      ("c:right" "diva:right*")
      ("c:a" "diva:left*")
      ("c:e" "diva:right*")
      ("home" "diva:left*")
      ("end" "diva:right*")))
  
  
  ;; Sets up all the preferences to default values if they do not exist yet.
  (define (set-default-preferences)
    (preferences:set-default 'divascheme:on?
                             #f boolean?)
    (preferences:set-default 'divascheme:preferred-keyboard-layout
                             'qwerty symbol?)
    (preferences:set-default 'divascheme:global-bindings
                             default-global-bindings list?)
    (preferences:set-default 'divascheme:command-mode-bindings
                             default-command-mode-bindings/qwerty list?)
    (preferences:set-default 'divascheme:insert-mode-bindings
                             default-insert-mode-bindings list?))
  
  
  ;; forcefully-reset-to-defaults: -> void
  ;; This should only be called in emergencies.  I'll keep this undocumented,
  ;; and leave it as a reserve.
  (define (forcefully-reset-to-defaults)
    (preferences:set 'divascheme:on? #f)
    (preferences:set 'divascheme:preferred-keyboard-layout 'qwerty)
    (preferences:set 'divascheme:global-bindings default-global-bindings)
    (preferences:set
     'divascheme:command-mode-bindings default-command-mode-bindings/qwerty)
    (preferences:set
     'divascheme:insert-mode-bindings default-insert-mode-bindings))
  
  
  (define (get-global-bindings)
    (preferences:get 'divascheme:global-bindings))
  
  (define (get-command-mode-bindings)
    (preferences:get 'divascheme:command-mode-bindings))
  
  (define (get-insert-mode-bindings)
    (preferences:get 'divascheme:insert-mode-bindings))
  
  
  (define (set-global-bindings bindings)
    (preferences:set 'divascheme:global-bindings bindings))
  
  (define (set-command-mode-bindings bindings)
    (preferences:set 'divascheme:command-mode-bindings bindings))
  
  (define (set-insert-mode-bindings bindings)
    (preferences:set 'divascheme:insert-mode-bindings bindings))
  
  
  
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
  
  
  ;; install-keybindings: keymap (listof (list key function-name))
  (define (install-keybindings keymap bindings)
    (define (key->decorated-key key)
      (match key
        [(list 'alt/meta-prefix key)
         (alt/meta-prefix key)]
        [else key]))
    
    (for-each (lambda (key&function-name)
                (send keymap map-function
                      (key->decorated-key (first key&function-name))
                      (second key&function-name)))
              bindings))
  
  ;; install-global-keybindings: keymap% -> void
  (define (install-global-bindings keymap)
    (install-keybindings keymap
                         (preferences:get 'divascheme:global-bindings)))
  
  ;; install-command-mode-keybindings: keymap% -> void
  (define (install-command-mode-bindings keymap)
    (install-keybindings keymap
                         (preferences:get 'divascheme:command-mode-bindings)))
  
  ;; install-insert-mode-keybindings: keymap% -> void
  (define (install-insert-mode-bindings keymap)
    (install-keybindings keymap
                         (preferences:get 'divascheme:insert-mode-bindings)))
  
  
  
  
  
  
  ;; add-preference-panel: diva-central% -> void
  ;; Attaches a DivaScheme preference panel for user-editable preferences.
  (define (add-preference-panel diva-central)
    ;; read-string: string -> sexpr
    (define (read-string str)
      (let ([ip (open-input-string (format "(~a)" str))])
        (first (read ip))))
    
    ;; sexp->string: sexpr -> string
    (define (sexp->string sexp)
      (let ([op (open-output-string)]
            [sexp
             (map (match-lambda [(list a b) (list a (string->symbol b))])
                  sexp)])
        (pretty-print sexp op)
        (get-output-string op)))
    
    ;; string->sexp: string -> sexpr
    (define (string->sexp text)
      (map (match-lambda [(list a b) (list a (symbol->string b))])
           (read-string text)))
    
    (define (id->keyboard-symbol id)
      (case id
        [(0) 'qwerty]
        [(1) 'dvorak]
        [(2) 'orbiTouch]))
    
    (define (id->keymap id)
      (case id
        [(0) default-command-mode-bindings/qwerty]
        [(1) default-command-mode-bindings/dvorak]
        [(2) default-command-mode-bindings/orbitouch]))
    
    (define (keyboard-symbol->id s)
      (case s
        [(qwerty) 0]
        [(dvorak) 1]
        [(orbiTouch) 2]
	[else 0]))
    
    
    (define (keybinding-key? datum)
      (match datum
        [(? string?)
         #t]
        [(list 'alt/meta-prefix (? string?))
         #t]
        [else #f]))
    
    (define keybindings/c
      (listof (list/c keybinding-key? symbol?)))
    
    (preferences:add-panel
     "DivaScheme"
     (lambda (p-frame)
       
       (define (can-read? title text)
         (with-handlers
             ([void
               (lambda (exn)
                 (message-box "Custom key bindings"
                              (format "Cannot understand the key bindings specified for the ~a.~n~a" title
                                      (exn-message exn)))
                 #f)])
           (contract keybindings/c (read-string text) '|the text| '|the key bindings|)
           #t))
       
       
       
       (letrec ([parent (new vertical-panel% [parent p-frame] [border 10])]
                
                [display-choice (new choice% [label "View key bindings for "]
                                     [choices '("Command Mode" "Insert Mode" "Globals")]
                                     [parent parent]
                                     [callback
                                      (lambda (c e)
                                        (send text-pane active-child
                                              (case (send display-choice get-selection)
                                                [(0) command-keybindings-text]
                                                [(1) insert-keybindings-text]
                                                [(2) global-keybindings-text])))])]
                
                [text-pane (new panel:single% (parent parent))]
                
                [command-keybindings-text
                 (new text-field% [label ""] [parent text-pane] [style '(multiple)]
                      [init-value (sexp->string (preferences:get 'divascheme:command-mode-bindings))])]
                
                [insert-keybindings-text
                 (new text-field% [label ""] [parent text-pane] [style '(multiple)]
                      [init-value (sexp->string (preferences:get 'divascheme:insert-mode-bindings))])]
                
                [global-keybindings-text
                 (new text-field% [label ""] [parent text-pane] [style '(multiple)]
                      [init-value (sexp->string (preferences:get 'divascheme:global-bindings))])]
                
                [reset-panel (new horizontal-panel% [parent parent] [stretchable-height #f])]
                
                [preferred-keyboard-layout (keyboard-symbol->id (preferences:get 'divascheme:preferred-keyboard-layout))]
                
                [choice (new choice% [label "Set key binding to built-in layout "]
                             [choices '("Qwerty" "Dvorak" "orbiTouch")]
                             [selection preferred-keyboard-layout]
                             [parent reset-panel])]
                
                [set-to-default-keybindings
                 (lambda ()
                   (set! preferred-keyboard-layout (send choice get-selection))
                   (send command-keybindings-text set-value (sexp->string (id->keymap (send choice get-selection))))
                   (send insert-keybindings-text set-value (sexp->string default-insert-mode-bindings))
                   (send global-keybindings-text set-value (sexp->string default-global-bindings))
                   (send (send command-keybindings-text get-editor) set-position 0)
                   (send (send insert-keybindings-text get-editor) set-position 0)
                   (send (send global-keybindings-text get-editor) set-position 0))]
                
                [choices-button
                 (new button%
                      [label "Set key bindings"]
                      [parent reset-panel]
                      [callback (lambda (c e)
                                  (set-to-default-keybindings))])])
         
         (send text-pane active-child command-keybindings-text)
         (send (send command-keybindings-text get-editor) set-position 0)
         (send (send insert-keybindings-text get-editor) set-position 0)
         (send (send global-keybindings-text get-editor) set-position 0)
         (send insert-keybindings-text show #f)
         (send global-keybindings-text show #f)
         
         (preferences:add-can-close-dialog-callback
          (lambda ()
            (and (can-read? "Command Mode" (send command-keybindings-text get-value))
                 (can-read? "Insert Mode" (send insert-keybindings-text get-value))
                 (can-read? "Global Mode" (send global-keybindings-text get-value))
                 #t)))
         
         (preferences:add-on-close-dialog-callback
          (lambda ()
            (preferences:set 'divascheme:preferred-keyboard-layout (id->keyboard-symbol preferred-keyboard-layout))
            (preferences:set 'divascheme:command-mode-bindings (string->sexp (send command-keybindings-text get-value)))
            (preferences:set 'divascheme:insert-mode-bindings (string->sexp (send insert-keybindings-text get-value)))
            (preferences:set 'divascheme:global-bindings (string->sexp (send global-keybindings-text get-value)))
            (send diva-central keymap-changed)))
         
         
         parent))))
  
  
  (set-default-preferences))