(module exercise-navigation mzscheme
  (require "dsyntax.ss"
           "parse-plt-scheme.ss"
           "simple-profile.ss"
           "dstx-text-mixin.ss"
           (lib "etc.ss")
           (lib "list.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred"))
  
  ;; not really a test, more an interactive exercise of the
  ;; focus commands from a graphical window.
  ;; This is almost a poor-man's DivaScheme.
  
  (define (my-key-handler editor event)
    (define (maybe-add-leading-space a-cursor)
      (cond [(space? (send a-cursor cursor-dstx))
             (void)]
            [else
             (send a-cursor insert-after! (new-space " "))]))
    
    
    (define (maybe-add-trailing-space a-cursor)
      (when (send a-cursor can-focus-older/no-snap?)
        (send a-cursor focus-older/no-snap!)
        (cond [(space? (send a-cursor cursor-dstx))
               (send a-cursor focus-younger/no-snap!)]
              [else
               (send a-cursor insert-before! (new-space " "))
               (send a-cursor focus-younger/no-snap!)])))
    
    (let ([a-cursor (send editor get-tree-cursor)])
      (cond [(send a-cursor can-focus-endpos? (send editor get-start-position))
             (send a-cursor focus-endpos! (send editor get-start-position))]
            [else
             (send a-cursor focus-container! (send editor get-start-position))])
      
      (case (send event get-key-code)
        [(#\j)
         (send a-cursor focus-predecessor!)
         (send editor show-focus)]
        [(#\l)
         (send a-cursor focus-successor!)
         (send editor show-focus)]
        [(#\k)
         (send a-cursor focus-out!)
         (send editor show-focus)]
        [(#\a)
         (send a-cursor focus-younger!)
         (send editor show-focus)]
        [(#\e)
         (send a-cursor focus-older!)
         (send editor show-focus)]
        [(#\()
         (maybe-add-leading-space a-cursor)
         (send a-cursor insert-after! (new-fusion "(" (list (new-atom "$expr$")) ")"))
         (maybe-add-trailing-space a-cursor)
         (send a-cursor focus-in!)
         (send editor show-focus)]
        [(#\[)
         (maybe-add-leading-space a-cursor)
         (send a-cursor insert-after! (new-fusion "[" (list (new-atom "$expr$")) "]"))
         (maybe-add-trailing-space a-cursor)
         (send a-cursor focus-in!)
         (send editor show-focus)]
        [(#\')
         (maybe-add-leading-space a-cursor)
         (send a-cursor insert-after! (new-fusion "'"
                                                  (list (new-atom "$expr$"))
                                                  ""))
         (maybe-add-trailing-space a-cursor)
         (send a-cursor focus-in!)
         (send editor show-focus)]
        [(#\,)
         (maybe-add-leading-space a-cursor)
         (send a-cursor insert-after! (new-fusion ","
                                                  (list (new-atom "$expr$"))
                                                  ""))
         (maybe-add-trailing-space a-cursor)
         (send a-cursor focus-in!)
         (send editor show-focus)]
        [(f4)
         (send editor toggle-dstx-parsing)])))
  
  
  
  (define my-text%
    (class (dstx-text-mixin text%)
      (inherit set-keymap get-keymap set-position scroll-to-position
               get-dstx-cursor
               dstx-parsing-enabled?
               enable-dstx-parsing
               disable-dstx-parsing)
      
      (super-new)
      (define tree-cursor (get-dstx-cursor))
      (set-keymap (new keymap%))
      (send (get-keymap) add-function
            "dsyntax:test-handler" my-key-handler)
      (send (get-keymap) remove-grab-key-function)
      (send (get-keymap) map-function "C:j" "dsyntax:test-handler")
      (send (get-keymap) map-function "C:k" "dsyntax:test-handler")
      (send (get-keymap) map-function "C:l" "dsyntax:test-handler")
      (send (get-keymap) map-function "(" "dsyntax:test-handler")
      (send (get-keymap) map-function "[" "dsyntax:test-handler")
      (send (get-keymap) map-function "f4" "dsyntax:test-handler")
      (send (get-keymap) map-function "'" "dsyntax:test-handler")
      (send (get-keymap) map-function "," "dsyntax:test-handler")
      
      (define/override (load-file filename)
        (super load-file filename)
        (set! tree-cursor (get-dstx-cursor)))
      
      
      (define/public (get-tree-cursor)
        tree-cursor)
      
      (define/augment (on-structured-insert-before f-cursor a-dstx)
        (inner (void) on-structured-insert-before f-cursor)
        (printf "structured insert seen: ~s~n" (cursor-dstx f-cursor)))
      
      (define/augment (on-structured-insert-after f-cursor a-dstx)
        (inner (void) on-structured-insert-after f-cursor)
        (printf "structured insert seen: ~s~n" (cursor-dstx f-cursor)))
      
      (define/augment (on-structured-delete f-cursor)
        (inner (void) on-structured-delete f-cursor)
        (printf "structured delete seen~n"))
      
      
      (define/public (show-focus)
        (set-position (send tree-cursor cursor-pos)
                      (send tree-cursor cursor-endpos)
                      #f
                      #f)
        (scroll-to-position (send tree-cursor cursor-pos)
                            #f
                            (send tree-cursor cursor-endpos)
                            'start))
      
      (define/public (toggle-dstx-parsing)
        (cond [(dstx-parsing-enabled?)
               (printf "parsing disabled~n")
               (disable-dstx-parsing)]
              [else
               (printf "parsing enabled~n")
               (enable-dstx-parsing)]))))
  
  
  (define (test)
    (open-file (expand-user-path "~/local/plt/collects/tex2page/tex2page-aux.ss")))
  
  
  (define (open-file filename)
    (local ((define f (make-object frame% "test-navigation" #f 400 500))
            (define t (make-object my-text%))
            (define c (make-object editor-canvas% f t '(no-hscroll))))
      (send t enable-dstx-parsing)
      (send t load-file filename)
      (send t auto-wrap #t)
      (send f show #t)
      t)))