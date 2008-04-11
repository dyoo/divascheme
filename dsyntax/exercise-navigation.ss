(module exercise-navigation mzscheme
  (require "dsyntax.ss"
           "parse-plt-scheme.ss"
           "text-support.ss"
           "simple-profile.ss"
           (lib "etc.ss")
           (lib "list.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred"))
  
  ;; not really a test, more an interactive exercise of the
  ;; focus commands from a graphical window.
  
  (define (my-key-handler editor event)
    (local ((define a-cursor (send editor get-tree-cursor)))
      (case (send event get-key-code)
        [(#\j)
         (send editor set-tree-cursor (focus-predecessor a-cursor))]
        [(#\l)
         (send editor set-tree-cursor (focus-successor a-cursor))]
        [(#\K)
         (send editor set-tree-cursor (focus-out a-cursor))])))
  
  
  (define my-text%
    (class text%
      (super-new)
      (inherit set-keymap get-keymap set-position scroll-to-position)
      (set-keymap (new keymap%))
      (send (get-keymap) add-function
            "dsyntax:test-handler" my-key-handler)
      (send (get-keymap) remove-grab-key-function)
      (send (get-keymap) map-function "j" "dsyntax:test-handler")
      (send (get-keymap) map-function "K" "dsyntax:test-handler")
      (send (get-keymap) map-function "l" "dsyntax:test-handler")
      
      
      (define tree
        (call-with-text-input-port this parse-port))
      (define tree-cursor
        (make-toplevel-cursor tree))
      
      (define (refresh-tree)
        (prof 'refresh-tree
              (set! tree (call-with-text-input-port this parse-port))
              (set-tree-cursor (make-toplevel-cursor tree))))
      
      (define/override (load-file filename)
        (super load-file filename)
        (refresh-tree))
      
      (define/public (get-tree-cursor)
        tree-cursor)
      
      (define/public (set-tree-cursor a-cursor)
        (when a-cursor
          (printf "~a ~a~n"
                  (cursor-pos a-cursor)
                  (cursor-endpos a-cursor))
          (set! tree-cursor a-cursor)
          (set-position (cursor-pos a-cursor)
                        (cursor-endpos a-cursor)
                        #f
                        #f)
          (scroll-to-position (cursor-pos a-cursor)
                              #f
                              (cursor-endpos a-cursor)
                              'start)))))
  
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