(module mred-callback mzscheme
  (require (lib "etc.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           "utilities.ss"
           "long-prefix.ss"
           "rope.ss"
           "gui/text-rope-mixin.ss")
  
  ;; Here we are defining mixins for text% and canvas% classes
  ;; whose instanciations would be handled by a mred-state%.
  ;; These mixins are setting callbacks and variables.
  
  ;; There is a special one for the interaction because it is a special text:
  ;; there is some text to be removed, and some part to be read-only, etc..
  
  (provide voice-mred-text-callback-mixin
           voice-mred-interactions-text-callback-mixin
           )
  
  ;;
  ;; DEBUGGING STUFFS
  ;;
  
  ;; diva-debug : boolean
  (define diva-debug false)
  
  ;; This function prints something if and only if diva-debug is true.
  (define (diva-printf text . args)
    (when diva-debug
      (apply printf text args)))
  
  
  
  ;;
  ;; THE MRED TEXT CALLBACK MIXINS
  ;;
  
  ;; Here we are trying to solve problems about the content of the window
  ;; which is changing whereas DivaScheme is activated and these changes are not done through DivaScheme.
  ;; TODO : re-thought all these stuffs here; I'm sure all that can be better...
  
  ;; To be told that the cursor position changed is very difficult because there are a lot of ways to change it
  ;; and each has got its own processing method.
  ;;  * when clicking with the mouse: I don't remember
  ;;  * when moving the arrows of the keyboard: that's a C/C++ function; it is needed to put a hook on a kaymap about that
  ;;  * when editing text: no callback about the position; we know that probably the position changed after an edition
  ;;  * when calling the method set-position: callback with after-set-position

  (define (voice-mred-text-callback-mixin super%)
    (class super%
      (super-instantiate ())
      
      
      ;;
      ;; TEXT STUFFS
      ;;
      
      (inherit get-text can-insert? delete begin-edit-sequence end-edit-sequence
               highlight-range)
      
      (define/public (diva:-get-rope)
        (send this get-rope))
      
      ;; update-text: rope -> void
      ;; Given the content in to-text, we set the rope in mred-callback so that
      ;; the post-condition is that (get-rope) should be the same as to-text
      ;; (rope=? modulo specials).
      (define (update-text to-text)
        (let ([from-text (send this diva:-get-rope)])
          (unless (rope=? to-text from-text)
            (let*-values
                ([(start-length end-length)
                  (common-prefix&suffix-lengths (rope->vector from-text)
                                                (rope->vector to-text)
                                                vector-length
                                                vector-ref
                                                equal?)]
                  [(from-end)
                   (- (rope-length from-text) end-length)]
                  [(to-end)
                   (- (rope-length to-text) end-length)]
                  [(insert-text) (subrope to-text start-length to-end)])
               (cond
                 [(rope=? (subrope from-text start-length from-end)
                          insert-text)
                  (void)]
                 [(can-insert? start-length from-end)
                  (begin-edit-sequence)
                  (delete start-length from-end #f)
                  (send this set-position start-length 'same #f #f 'local)
                  (insert-rope-in-text this insert-text)
                  (end-edit-sequence)]
                 [else
                  (raise (make-voice-exn
                          "I cannot edit the text. Text is read-only."))])))))
      
      
      (define/public (diva:-update-text text)
        (dynamic-wind
         (lambda () (begin-edit-sequence))
         (lambda () (update-text text))
         (lambda () (end-edit-sequence))))
      
      
      ))
  
  
  
  ;; All the stuffs here is for removing the "annotations" at the beginning of the interactions window
  ;; which makes read-syntax unhappy: 
  ;;      Bienvenue dans DrScheme, version 299.405-svn9nov2005.
  ;;      Langage: Textuel (MzScheme).
  ;;      > 
  ;;
  ;; We do so by focusing diva:-get-text to text stuff after the unread-start-point; everything
  ;; before it will be hidden from DivaScheme.  At the moment, we do so by making the previous
  ;; characters into 'X's.  Not perfect, but it sorta works.
  (define (voice-mred-interactions-text-callback-mixin super%)
    (class (voice-mred-text-callback-mixin super%)
      (super-instantiate ())
      
      (inherit get-rope
               get-text
               get-unread-start-point)
      
      (define/override (diva:-update-text text)
        (super diva:-update-text text)
        ;; rehydrate-prompts-in-text: cleanup-text munges the following space
        ;; after a prompt symbol. We have to kludge those spaces back in. Ugh.
        #; (define (rehydrate-prompts-in-text text)
             (regexp-replace* "(^|\n)>($|\n)" text "\\1> \\2"))
        #; (super diva:-update-text (rehydrate-prompts-in-text text)))
      
      
      (define/override (diva:-get-rope)
        (define (all-but-last text)
          (substring text 0 (sub1 (string-length text))))
        
        (define (last-char text)
          (string-ref text (sub1 (string-length text))))
        
        (define (hide-annotations annotated-text)
          (cond
            [(and (> (string-length annotated-text) 0)
                  (char-whitespace? (last-char annotated-text)))
             (string-append
              (string-convert-non-control-chars
               (all-but-last annotated-text) #\X)
              (string (last-char annotated-text)))]
            [else
             (string-convert-non-control-chars annotated-text #\X)]))
        (let ([a-rope (get-rope)]
              [text (get-text)])
          (rope-append
           (string->rope
            (hide-annotations (substring text 0 (get-unread-start-point))))
           (subrope a-rope (get-unread-start-point)))))))
  
  )
