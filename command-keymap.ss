(module command-keymap mzscheme
  (require (lib "etc.ss")
           (lib "class.ss")
           (lib "framework.ss" "framework")
           (lib "mred.ss" "mred")
           (lib "list.ss")
           "choose-paren.ss"
           "structures.ss"
           (prefix preferences: "diva-preferences.ss"))
  
  (provide make-command-keymap)
  
  
  ;; A grab-key-function that looks at unmapped keys that look like
  ;; capslock is on, and tries to remap to keys that aren't caps-locked.
  (define (ignores-caps-lock-grab-key-function str km editor event)
    (define (caps-lock-on?)
      (and (not str)
           (is-a? event key-event%)
           (char? (send event get-key-code))
           (char-upper-case? (send event get-key-code))
           (not (send event get-shift-down))))
    (define (copy-key/downcase)
      (let ([key-event
             (new key-event%
                  [key-code (char-downcase (send event get-key-code))]
                  [shift-down #f]
                  [control-down (send event get-control-down)]
                  [meta-down (send event get-meta-down)]
                  [alt-down (send event get-alt-down)]
                  [x (send event get-x)]
                  [y (send event get-y)]
                  [time-stamp (send event get-time-stamp)])])
        (send key-event set-other-altgr-key-code
              (send key-event get-other-altgr-key-code))
        (send key-event set-other-shift-altgr-key-code
              (send key-event get-other-shift-altgr-key-code))
        (send key-event set-other-shift-key-code
              (send key-event get-other-shift-key-code))
        key-event))
    (if (caps-lock-on?)
        (send km handle-key-event editor
              (copy-key/downcase))
        #f))
  
  
  
  (define make-command-keymap
    (lambda (window-text to-insert-mode to-insert-mode/cmd diva-message diva-question interpreter)
      (let ([command-keymap (make-object keymap:aug-keymap%)])
        
        (define (make-command-to-argument-mode command title)
          (let ([default ""])
            (lambda ()
              (diva-question title
                             default
                             argument-to-command-mode
                             (lambda (text)
                               (set! default text)
                               (interpreter (make-Verb (make-Command command)
                                                       false
                                                       (make-WhatN (make-Symbol-Noun
                                                                    (string->symbol text)))))
                               (argument-to-command-mode))))))
        
        (define (argument-to-command-mode)
          (let [(canvas (send window-text get-canvas))]
            (when canvas
              (send canvas focus))))
        
        (define (command command)
          (lambda (any event)
	    (interpreter (make-Verb (make-Command command) false false))))
        
        (define insert-before-ast 
          (make-Verb (make-Command 'Insert) (make-Loc (make-Before) false) false))
        (define insert-after-ast  
          (make-Verb (make-Command 'Insert) (make-Loc (make-After) false) false))
        
        
        (define (insert ast/false edit?)
          (lambda (any event)
            (when ast/false
		  (interpreter ast/false))
            (to-insert-mode edit?)))

        (define (insert/cmd cmd edit?)
          (lambda (any event)
            (to-insert-mode/cmd edit? cmd)))
        
        
        (define (insert-contextual-open/cmd cmd edit?)
          (lambda (editor event)
            (to-insert-mode/cmd edit? (get-contextual-open-cmd editor cmd))))
        
        
        (define (argument command title)
          (let ([command/default (make-command-to-argument-mode command title)])
            (lambda (any event)
              (command/default))))
        
        (add-text-keymap-functions command-keymap)
        
        (send command-keymap add-function "diva:enter" (command 'Enter))
        (send command-keymap add-function "diva:indent"    (command 'Indent))
        (send command-keymap add-function "diva:before-this" (insert insert-before-ast false))
        (send command-keymap add-function "diva:after-this" (insert insert-after-ast false))
        (send command-keymap add-function "diva:insert" (insert false false))
        (send command-keymap add-function "diva:up"        (command 'Up))
        (send command-keymap add-function "diva:down"        (command 'Down))
        (send command-keymap add-function "diva:out"       (command 'Out))
        (send command-keymap add-function "diva:backward"  (command 'Backward))
        (send command-keymap add-function "diva:forward"   (command 'Forward))
        (send command-keymap add-function "diva:next"      (command 'Next))
        (send command-keymap add-function "diva:previous"  (command 'Previous))
        (send command-keymap add-function "diva:select" (argument 'Select "select"))
        (send command-keymap add-function "diva:search-forward"  (argument 'Search-Forward  "search forward"))
        (send command-keymap add-function "diva:search-backward" (argument 'Search-Backward "search backward"))
        (send command-keymap add-function "diva:copy"      (command 'Copy))
        (send command-keymap add-function "diva:cut"       (command 'Cut))
        (send command-keymap add-function "diva:paste"     (command 'Paste))
        (send command-keymap add-function "diva:undo"      (command 'Undo))
        (send command-keymap add-function "diva:cancel"    (command 'Cancel))
        (send command-keymap add-function "diva:redo"      (command 'Redo))
        (send command-keymap add-function "diva:delete"    (command 'Delete))
        (send command-keymap add-function "diva:push"       (command 'Push))
        (send command-keymap add-function "diva:bring"     (command 'Bring))
        (send command-keymap add-function "diva:exchange"  (command 'Exchange))
        (send command-keymap add-function "diva:mark"      (command 'Mark))
        (send command-keymap add-function "diva:holder"    (command 'Holder))
        (send command-keymap add-function "diva:transpose" (command 'Transpose))
        (send command-keymap add-function "diva:find-tag" (argument 'Tag "Find tag"))
        (send command-keymap add-function "diva:magic"     (command 'Magic))
        (send command-keymap add-function "diva:join"      (command 'Join))
        (send command-keymap add-function "diva:unmark"    (command 'UnMark))
        
        (send command-keymap add-function "diva:open" (insert/cmd 'Open false))
        (send command-keymap add-function "diva:open-square/contextual" (insert-contextual-open/cmd 'Open-Square false))
        (send command-keymap add-function "diva:open-square" (insert/cmd 'Open-Square false))
        
        (send command-keymap add-function "diva:close"     (command 'Close))
        (send command-keymap add-function "diva:search-top" (argument 'Search-Top "search top" ))
        (send command-keymap add-function "diva:search-bottom" (argument 'Search-Bottom "search bottom"))
        (send command-keymap add-function "diva:definition" (argument 'Definition "definition"))
        (send command-keymap add-function "diva:usage" (argument 'Usage "usage"))
        (send command-keymap add-function "diva:younger"     (command 'Younger))
        (send command-keymap add-function "diva:older"      (command 'Older))
        (send command-keymap add-function "diva:first"     (command 'First))
        (send command-keymap add-function "diva:last"      (command 'Last))
        (send command-keymap add-function "diva:extend-selection" (command 'Extend-Selection))
        (send command-keymap add-function "diva:edit-symbol" (insert false true))
        (send command-keymap add-function "diva:disabled" void)
        (for-each
         (lambda (key) (send command-keymap map-function key "diva:disabled"))
         `("1" "2" "3" "4" "5" "6" "7" "8" "9" "0"
               "!" "@" "#" "$" "%" "^" "&" "*"
               "_" "-" "=" "+"
               "backspace" "delete" "|"
               "`" "\"" "," "'" "<" ">" "/" "\\"  "?"
               "insert" "colon"
               ,@(map (lambda (ch) (format "s:~a" ch))
                      (string->list "abcdefghijklmnopqrstuvwxyz"))
               ,@(map string
                      (string->list "abcdefghijklmnopqrstuvwxyz"))))
        
        
        ;; When caps lock is on, it appears that something screwy happens with 
        ;; key lookup.  I may want to refactor this out or ask on the PLT list
        ;; what the right thing to do here is.
        ;; The following tries to ignore caps lock.
        (send command-keymap set-grab-key-function
              ignores-caps-lock-grab-key-function)
        
        
        (preferences:install-command-mode-bindings command-keymap)
        
        
        command-keymap))))
