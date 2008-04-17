(module exercise-navigation mzscheme
  (require "dsyntax.ss"
           "parse-plt-scheme.ss"
           "text-support.ss"
           "simple-profile.ss"
           "dstx-text-mixin.ss"
           (lib "etc.ss")
           (lib "list.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred"))
  
  ;; not really a test, more an interactive exercise of the
  ;; focus commands from a graphical window.
  
  (define (my-key-handler editor event)
    (local ((define a-cursor (send editor get-tree-cursor)))
      (send a-cursor resync)
      (send a-cursor focus-pos (send editor get-start-position))
      (case (send event get-key-code)
        [(#\j)
         (send a-cursor focus-predecessor)
         (send editor show-focus)]
        [(#\l)
         (send a-cursor focus-successor)
         (send editor show-focus)]
        [(#\k)
         (send a-cursor focus-out)
         (send editor show-focus)]
        [(#\a)
         (send a-cursor focus-younger)
         (send editor show-focus)]
        [(#\e)
         (send a-cursor focus-older)
         (send editor show-focus)])))
  
  
  (define my-text%
    (class (dstx-text-mixin text%)
      (inherit set-keymap get-keymap set-position scroll-to-position get-dstx-cursor)
      
      (super-new)
      (define tree-cursor (get-dstx-cursor))
      (set-keymap (new keymap%))
      (send (get-keymap) add-function
            "dsyntax:test-handler" my-key-handler)
      (send (get-keymap) remove-grab-key-function)
      (send (get-keymap) map-function "C:j" "dsyntax:test-handler")
      (send (get-keymap) map-function "C:k" "dsyntax:test-handler")
      (send (get-keymap) map-function "C:l" "dsyntax:test-handler")
      
      
      (define/override (load-file filename)
        (super load-file filename)
        (set! tree-cursor (get-dstx-cursor)))
      
      
      (define/public (get-tree-cursor)
        tree-cursor)
      
      (define/public (show-focus)
        (set-position (send tree-cursor cursor-pos)
                      (send tree-cursor cursor-endpos)
                      #f
                      #f)
        (scroll-to-position (send tree-cursor cursor-pos)
                            #f
                            (send tree-cursor cursor-endpos)
                            'start))))
  
  (define (test)
    (open-file (expand-user-path "~/local/plt/collects/tex2page/tex2page-aux.ss")))
  
  
  (define (open-file filename)
    (local ((define f (make-object frame% "test-navigation" #f 400 500))
            (define t (make-object my-text%))
            (define c (make-object editor-canvas% f t '(no-hscroll))))
      (send t load-file filename)
      (send t auto-wrap #t)
      (send f show #t)
      t)))