(module diva-preferences mzscheme
  (require (lib "plt-match.ss")
           (lib "class.ss")
           (lib "framework.ss" "framework")
           "diva-central.ss")
  (provide install-diva-central-handler
           enable-on-startup?
           command-mode-bindings)
  
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
    '(("f4" "diva:f4-callback")
      ("return" "diva:enter")
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
  
  
  ;; DEBUG: at the moment, forcefully set the defaults all the time since Guillaume's
  ;; still refining what shortcuts to use for the orbitouch.
  (preferences:set 'divascheme:command-mode-bindings default-command-mode-bindings))