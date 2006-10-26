(module tag-gui mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "list.ss")
           (lib "list.ss")
           "tag-reader.ss"
           "tag-state.ss")

    
  ;; Adds the support necessary to load new tag files.
  ;; This adds a new menu to the File menu called "Load Navigation Tags".
  (provide tag-gui-unit:frame-mixin)
  (define (tag-gui-unit:frame-mixin super%)
    (class super%
      (override file-menu:between-print-and-close)
      (super-new)
      
      ;; Add a new menu option "Load Diva Tags" that goes
      ;; right after the print menu.
      (define (file-menu:between-print-and-close menu)
        (super file-menu:between-print-and-close menu)
        (new menu-item% 
             [label "Load Navigation Tags..."]
             [parent menu]
             [callback (lambda (menu-item control-event)
                         (ask-and-load-tags this))])
        (make-object separator-menu-item% menu))
      
      
      (define (ask-and-load-tags parent)
        (let ([filename
               (get-file "Choose a STAGS file.\nSTAGS files contain navigation hints,\nand are created by plt/bin/stags."
                         parent
                         #f
                         #f
                         "ss"
                         empty
                         '(("Any" "STAGS")))])
          (when filename
            (set-current-tag-library! (open-tag-library filename))

            #;(message-box "tags" (format "~a loaded" filename)))))))
  
  
  ;; Frame for showing search results from tag searching.
  #;(define search-frame%
    (class object%
      (super-new)

      (define null-choice (string-copy "---"))
      

      (define (tag->description tag)
        (format "~a (~a, line ~a)" 
                (tag-name tag)
                (tag-path tag)
                (tag-line tag)))

      (define-values (memorize-tag+description!
                      recall-tag
                      forget-tags+descriptions!)
        (let ([description-tag-map (make-hash-table 'equal)])
          (values
           (lambda (t)
             (hash-table-put! description-tag-map (tag->description t) t))

           (lambda (description)
             (hash-table-get description-tag-map description))

           (lambda ()
             (set! description-tag-map (make-hash-table 'equal))))))

      
      (define frame (new frame% [label "tags"]))
      
      (define h-panel
        (new horizontal-panel% [parent frame]))
      
      (define text-field 
        (new text-field% 
             [label "search:"] 
             [parent h-panel]
             [callback (lambda (text-field event)
                         (when (symbol=? 'text-field-enter
                                         (send event get-event-type))
                           (search-and-display-choices
                            (send text-field get-value))))]))
      
      (define choice-list 
        (new choice% 
             [label "Nothing to choose yet"]
             [choices (list null-choice)]
             [parent frame]
             [callback (lambda (c e)
                         (unless (= (send c get-selection) 0)
                           (open&jump
                            (recall-tag (send c get-string-selection)))))]))
      
      (define (open&jump tag)
        (let* ([frame (handler:edit-file (tag-path tag))]
               [editor (send frame get-editor)])
          (when (is-a? editor text%)
            (let ([pos (send editor paragraph-start-position (sub1 (tag-line tag)))])
              (send editor set-position (skip-to-paren editor pos))))
          (show #f)))


      (define (skip-to-paren text pos)
        (let loop ([pos pos])
          (if (>= pos (send text last-position))
              (send text last-position)
              (let ([char (send text get-character pos)])
                (cond
                 [(or (char=? #\) char)
                      (char=? #\( char)
                      (char=? #\] char)
                      (char=? #\[ char))
                  pos]
                 [else (loop (+ pos 1))])))))
      

      
      (define (choice-prompt-msg n)
        (cond [(= n 0) "No choices found."]
              [(= n 1) "One choice found:"]
              [else (format "~a choices found:" n)]))

            
      (define (search-and-display-choices query)
        (reset-choice-list)
        (let ([results (lookup query)])
          (send choice-list set-label (choice-prompt-msg (length results)))
          (for-each (lambda (t) 
                      (send choice-list append (tag->description t))
                      (memorize-tag+description! t)) 
                    results)))

      
      (define/public (reset-choice-list)
        (send choice-list clear)
        (send choice-list append null-choice)
        (forget-tags+descriptions!))



      (define/public (show b)
        (send frame show b)
        (when b
          (send text-field focus)))))



  ;; We'll keep a single search frame
  #;(define current-search-frame (new search-frame%))
  
  
  ;; Adds ESC;., m:., and a:. as keybindings to us.
  #;(provide register-tags-keybindings)
  #;(define (register-tags-keybindings keymap)
    (send keymap map-function "ESC;." "tags:-to-tag-mode")
    (send keymap map-function "m:." "tags:-to-tag-mode")
    (send keymap map-function "a:." "tags:-to-tag-mode")
    (send keymap map-function "d:." "tags:-to-tag-mode")
    (send keymap add-function "tags:-to-tag-mode"
          (lambda (editor event)
            (send current-search-frame show #t))))
)
  
  

  
