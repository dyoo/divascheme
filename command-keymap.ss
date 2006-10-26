(module command-keymap mzscheme
  (require (lib "etc.ss")
           (lib "class.ss")
           (lib "framework.ss" "framework")
	   "structures.ss")

  (provide make-command-keymap)
  

  (define make-command-keymap
    (lambda (window-text to-insert-mode to-insert-mode/cmd diva-message diva-question f4-callback interpreter)
      (let ([command-keymap (make-object keymap:aug-keymap%)])

        (define (make-command-to-argument-mode command title)
          (let ([default ""])
            (lambda ()
              (diva-question title
                             default
                             argument-to-command-mode
                             (lambda (text)
                               (set! default text)
                               (interpreter (make-Verb (make-Command command) false (make-WhatN (make-Symbol-Noun (string->symbol text)))))
                               (argument-to-command-mode))))))

        (define (argument-to-command-mode)
          (send (send window-text get-canvas) focus))
        
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

        (define (argument command title)
          (let ([command/default (make-command-to-argument-mode command title)])
            (lambda (any event)
              (command/default))))

        (send command-keymap add-function "diva:f4-callback" (lambda (any event) (f4-callback)))
        
        (send command-keymap map-function "f4"                "diva:f4-callback")
        
        (send command-keymap add-function "diva:enter"     (command 'Enter))
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
               "`" "\"" "," "'" "<" ">" "/" "?"
               "esc" "insert" "colon"
               ,@(map (lambda (ch) (format "s:~a" ch))
                      (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
               ,@(map string
                      (string->list "abcdefghijklmnopqrstuvwxyz"))))

        (send command-keymap map-function "return" "diva:enter")
        (send command-keymap map-function "numpadenter" "diva:enter")
        (send command-keymap map-function "tab" "diva:indent")
        (send command-keymap map-function "h" "diva:before-this")
        (send command-keymap map-function "semicolon" "diva:after-this")
        (send command-keymap map-function "r" "diva:insert")
        (send command-keymap map-function "k" "diva:down")
        (send command-keymap map-function "i" "diva:up")
        (send command-keymap map-function "s:k" "diva:out")
        (send command-keymap map-function "j" "diva:backward")
        (send command-keymap map-function "l" "diva:forward")
        (send command-keymap map-function "n" "diva:next")
        (send command-keymap map-function "p" "diva:previous")
        (send command-keymap map-function "s" "diva:select")
        (send command-keymap map-function "c" "diva:copy")
        (send command-keymap map-function "x" "diva:cut")
        (send command-keymap map-function "v" "diva:paste")
        (send command-keymap map-function "u" "diva:undo")
        (send command-keymap map-function "z" "diva:cancel")
        (send command-keymap map-function "y" "diva:redo")
        (send command-keymap map-function "d" "diva:delete")
        (send command-keymap map-function "b" "diva:bring")
        (send command-keymap map-function "s:b" "diva:push")
        (send command-keymap map-function "s:x" "diva:exchange")
        (send command-keymap map-function "m" "diva:mark")
        (send command-keymap map-function "s:m" "diva:unmark")
        (send command-keymap map-function "s:h" "diva:holder")
        (send command-keymap map-function "t" "diva:transpose")
        (send command-keymap map-function "." "diva:find-tag")
        (send command-keymap map-function "o" "diva:join")
        (send command-keymap map-function "[" "diva:open")
        (send command-keymap map-function "(" "diva:open")
        (send command-keymap map-function "{" "diva:open-square")
        (send command-keymap map-function "]" "diva:close")
        (send command-keymap map-function ")" "diva:close")
        (send command-keymap map-function "a" "diva:younger")
        (send command-keymap map-function "e" "diva:older")
        (send command-keymap map-function "space" "diva:extend-selection")
        (send command-keymap map-function "w" "diva:edit-symbol")

        command-keymap))))
