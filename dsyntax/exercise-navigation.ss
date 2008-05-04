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
    
    (define (snap-to-container a-cursor)
      (send a-cursor focus-container! (send editor get-start-position)))
    
    (define (snap-to-insertion-point a-cursor)
      (cond [(send a-cursor try-focus-endpos! (send editor get-start-position))
             (void)]
            [(send a-cursor try-focus-container! (send editor get-start-position))
             (void)]
            [else
             (send a-cursor focus-toplevel!)
             (send a-cursor focus-oldest!)]))
    
    (let ([a-cursor (send editor get-tree-cursor)])
      (let ([start-pos (send editor get-start-position)]
            [end-pos (send editor get-end-position)])
        (case (send event get-key-code)
          [(#\j)
           (snap-to-container a-cursor)
           (send a-cursor try-focus-predecessor!)
           (send editor show-focus)]
          [(#\l)
           (snap-to-container a-cursor)
           (send a-cursor try-focus-successor!)
           (send editor show-focus)]
          [(#\k)
           (snap-to-container a-cursor)
           (send a-cursor try-focus-out!)
           (send editor show-focus)]
          [(#\a)
           (snap-to-container a-cursor)
           (send a-cursor try-focus-younger!)
           (send editor show-focus)]
          [(#\e)
           (snap-to-container a-cursor)
           (send a-cursor try-focus-older!)
           (send editor show-focus)]
          [(#\()
           (snap-to-insertion-point a-cursor)
           (maybe-add-leading-space a-cursor)
           (send a-cursor insert-after! (new-fusion "(" (list) ")"))
           (maybe-add-trailing-space a-cursor)
           (send a-cursor focus-in/no-snap!)
           (send editor show-focus)]
          [(#\[)
           (snap-to-insertion-point a-cursor)
           (maybe-add-leading-space a-cursor)
           (send a-cursor insert-after! (new-fusion "[" (list) "]"))
           (maybe-add-trailing-space a-cursor)
           (send a-cursor focus-in/no-snap!)
           (send editor show-focus)]
          [(#\] #\))
           (snap-to-insertion-point a-cursor)
           (send a-cursor try-focus-out!)
           (send editor set-position (send a-cursor cursor-endpos))
           (send editor show-focus)]
          [(#\')
           (snap-to-insertion-point a-cursor)
           (maybe-add-leading-space a-cursor)
           (send a-cursor insert-after! (new-fusion "'"
                                                    (list (new-atom "$expr$"))
                                                    ""))
           (maybe-add-trailing-space a-cursor)
           (send a-cursor focus-in!)
           (send editor show-focus)]
          [(#\,)
           (snap-to-insertion-point a-cursor)
           (maybe-add-leading-space a-cursor)
           (send a-cursor insert-after! (new-fusion ","
                                                    (list (new-atom "$expr$"))
                                                    ""))
           (maybe-add-trailing-space a-cursor)
           (send a-cursor focus-in!)
           (send editor show-focus)]
          [(#\return numpad-enter)
           (send editor set-position
                 (send editor get-end-position)
                 (send editor get-end-position))
           (send editor insert "\n")]
          [(f4)
           (send editor toggle-dstx-parsing)]))))
  
  
  
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
      (send (get-keymap) map-function "C:a" "dsyntax:test-handler")
      (send (get-keymap) map-function "C:e" "dsyntax:test-handler")
      (send (get-keymap) map-function "(" "dsyntax:test-handler")
      (send (get-keymap) map-function ")" "dsyntax:test-handler")
      (send (get-keymap) map-function "[" "dsyntax:test-handler")
      (send (get-keymap) map-function "]" "dsyntax:test-handler")
      (send (get-keymap) map-function "f4" "dsyntax:test-handler")
      (send (get-keymap) map-function "return" "dsyntax:test-handler")
      (send (get-keymap) map-function "enter" "dsyntax:test-handler")
      (send (get-keymap) map-function "numpadenter" "dsyntax:test-handler")
      (send (get-keymap) map-function "'" "dsyntax:test-handler")
      (send (get-keymap) map-function "," "dsyntax:test-handler")
      
      (define/override (load-file filename)
        (super load-file filename)
        (set! tree-cursor (get-dstx-cursor)))
      
      
      (define/public (get-tree-cursor)
        tree-cursor)
      
      (define/augment (on-structured-insert-before f-cursor a-dstx)
        (inner (void) on-structured-insert-before f-cursor))
      
      (define/augment (on-structured-insert-after f-cursor a-dstx)
        (inner (void) on-structured-insert-after f-cursor))
      
      (define/augment (on-structured-delete f-cursor)
        (inner (void) on-structured-delete f-cursor))
      
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
  
  
  ;; (test)
  ;; torture test.  As of this writing, the file tex2page-aux.ss is the largest
  ;; scheme source file.
  (define (test)
    (open-file (big-file-path-string)))
  
  (define (big-file-path-string)
    (path->string
     (build-path
      (collection-path "tex2page")
      "tex2page-aux.ss")))
  
  ;; Measures how much time it takes to focus-successor and focus-predecessor
  ;; across a file.
  (define (measure-time a-file)
    (let* ([a-text (open-file a-file)]
           [a-cursor (send a-text get-dstx-cursor)])
      (time
       (send a-cursor focus-toplevel!)
       (let loop ([i 0])
         (cond [(send a-cursor try-focus-successor!)
                (loop (add1 i))]
               [else
                (display i)
                (newline)]))
       (let loop ([i 0])
         (cond [(send a-cursor try-focus-predecessor!)
                (loop (add1 i))]
               [else
                (display i)
                (newline)])))))
  
  
  
  (define (open-file filename)
    (local ((define f (make-object frame% filename #f 400 500))
            (define t (make-object my-text%))
            (define c (make-object editor-canvas% f t '(no-hscroll))))
      (send t enable-dstx-parsing)
      (send t load-file filename)
      (send t auto-wrap #t)
      (send f show #t)
      t)))