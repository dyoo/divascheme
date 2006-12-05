(module diva-preferences-gui mzscheme
  (require (lib "class.ss")
           (lib "mred.ss" "mred"))
  
  ;; Some test code to take a look at the view
  (define (test:show-view)
    (define f (new frame% [label "test"]))
    (define v (new preference-panel-view%
                   [parent f]
                   [border 10]))
    (send f show #t)
    v)
  
  (define preference-panel-view%
    (class vertical-panel%
      (super-new)
      (define display-choice
        (new choice% [label "View keybindings for "]
             [choices '("Command Mode" "Insert Mode" "Globals")]
             [parent this]))
      
      (define text-panel
        (new horizontal-panel% (parent this)))
      
      (define keybindings-text
        (new text-field% [label ""] [parent text-panel] [style '(multiple)]))
      
      (define reset-panel
        (new horizontal-panel% [parent this] [stretchable-height #f]))
      
      (define choice (new choice%
                          [label "Set keybinding to built-in layout"]
                          [choices '("Qwerty" "Dvorak" "orbiTouch")]
                          [parent reset-panel]))
      
      (define choices-button
        (new button%
             [label "Set keybindings"]
             [parent reset-panel])))))