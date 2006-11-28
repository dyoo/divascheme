(module mred-callback mzscheme
  (require (lib "etc.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred")
	   "utilities.ss"
           "long-prefix.ss")

  ;; Here we are defining mixins for text% and canvas% classes
  ;; whose instanciations would be handled by a mred-state%.
  ;; These mixins are setting callbacks and variables.

  ;; There is a special one for the interaction because it is a special text:
  ;; there is some text to be removed, and some part to be read-only, etc..

  (provide voice-mred-text-callback-mixin
           voice-mred-interactions-text-callback-mixin)
  
  ;;
  ;; DEBUGGING STUFFS
  ;;
  
  ;; diva-debug : boolean
  (define diva-debug false)
  
  ;; This function prints something if and only if diva-debug is true.
  (define (diva-printf text . args)
    (when diva-debug
      (apply printf text args)))
  
  (define ((a title) a)
    (diva-printf "A: ~a: ~a~n" title a)
    a)
  
  (define (b b)
    (diva-printf "B: `~a' ~n" b)
    b)
  
  
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
      (super-instantiate ()) ; compulsory if we want it to work

      ;;
      ;; INSERTION MODE CALLBACKS
      ;;

      ;; When the position changes, the insertion should be exited.
      (define insertion-after-set-position-callback-default (lambda () ()))
      (define insertion-after-set-position-callback insertion-after-set-position-callback-default)
      (define/public (diva:-insertion-after-set-position-callback-set callback)
	(set! insertion-after-set-position-callback callback))
      (define/public (diva:-insertion-after-set-position-callback-reset)
	(diva:-insertion-after-set-position-callback-set insertion-after-set-position-callback-default))

      
      ;;
      ;; CALLBACKS CALLS
      ;;

      (define/augment (after-set-position)
	(diva-printf "AFTER-SET-POSITION called.~n")
	(insertion-after-set-position-callback))

      (define/augment (after-insert start len) 
        (diva-printf "AFTER INSERT called with start: ~a len: ~a~n" start len)
        (inner void after-insert start len)
	(reset-mark))

      (define/augment (after-delete start len)
        (diva-printf "AFTER DELETE called with start: ~a len: ~a~n" start len)
        (inner void after-delete start len)
	(reset-mark))

      

      ;;
      ;; TEXT STUFFS
      ;;

      (inherit get-text can-insert? can-delete? delete insert freeze-colorer thaw-colorer
               begin-edit-sequence end-edit-sequence)
      
      (define/public (diva:-get-text)
        (get-text))


      
      (define (update-text to-text)
        (let ([from-text (send this diva:-get-text)])
          (unless (string=? to-text from-text)
            (let*-values
                ([(start-length end-length)
                  (common-prefix&suffix-lengths
                   from-text to-text string-length string-ref char=?)]
                 [(from-end)
                  (- (string-length from-text) end-length)]
                 [(to-end)
                  (- (string-length to-text) end-length)]
                 [(insert-text) (substring to-text start-length to-end)])
              (cond
                [(string=? (substring from-text start-length from-end)
                           insert-text)
                 (void)]
                [(can-insert? start-length from-end)
                 (begin-edit-sequence)
                 (insert insert-text start-length from-end false)
                 (end-edit-sequence)]
                [else
                 (raise (make-voice-exn "I cannot edit the text. Text is read-only."))])))))
      
      
      (define/public (diva:-update-text text)
	(dynamic-wind
         (lambda () (begin-edit-sequence))
         (lambda () (update-text text))
         (lambda () (end-edit-sequence))))
      
      
      ;;
      ;; MARK STUFFS
      ;;
      
      ;; We are supplying the same API as for the selection:
      ;;  * start and end position, no length
      ;;  * the same style of name
      ;;  * etc.
      
      (define mark-start-position 0)
      (define mark-end-position 0)

      (define/public (diva:-get-mark-start-position)
	mark-start-position)

      (define/public (diva:-get-mark-end-position)
	mark-end-position)

      (define/public (diva:-set-mark start-pos end-pos)
	(hide-mark)
	(set! mark-start-position start-pos)
	(set! mark-end-position   end-pos)
	(show-mark))

      (inherit highlight-range)

      (define mark-color (send the-color-database find-color "Orange"))

      (define (show-mark)
	(unless (= mark-start-position mark-end-position)
	   (set! hide-mark (highlight-range mark-start-position mark-end-position mark-color false false 'low))))
      
      (define hide-mark (lambda () ()))

      (define (reset-mark)
	(hide-mark)
	(set! mark-end-position mark-start-position))))


  ;; All the stuffs here is for removing the "annotations" at the beginning of the interactions window
  ;; which makes read-syntax unhappy: 
  ;;      Bienvenue dans DrScheme, version 299.405-svn9nov2005.
  ;;      Langage: Textuel (MzScheme).
  ;;      > 
  ;; NB: work only for interactions window because elsewhere the function initialize-console does not exist.
  (define (voice-mred-interactions-text-callback-mixin super%)
    (class (voice-mred-text-callback-mixin super%)
      (super-instantiate ())

      (inherit get-text)
      
      (define annotations-length 3)
      
      (define (get-annotations-length)
        annotations-length)

      (define/override (initialize-console)
        (super initialize-console)
        (set! annotations-length (string-length (get-text))))
      
      (define/override (diva:-update-text text)
        ;; rehydrate-prompts-in-text: cleanup-text munges the following space
        ;; after a prompt symbol.  We have to kludge those spaces back in.  Ugh.
        (define (rehydrate-prompts-in-text text)
          (regexp-replace* "(^|\n)>($|\n)" text "\\1> \\2"))
        (super diva:-update-text (rehydrate-prompts-in-text text)))
      
      
      (define/override (diva:-get-text)
        (define hide-character #\X)
        (define (hide-annotations text)
          (let ([annotations-length (get-annotations-length)])
            (format "~a~n~n~n~a"
                    (make-string (- annotations-length 3) hide-character)
                    (substring text annotations-length (string-length text)))))
        ;; hide-structures: string -> string
        ;; obscure what look like structures from being badly read.
        ;; FIXME: This is a kludge: we really should be paying attention to snips, and not
        ;; assume that everything is a string.
        (define (hide-structures text)
          (regexp-replace* "#<([^>]*)>" text "<<\\1>"))
        (let ([text (get-text)])
          (cond
            [(= 0 (string-length text)) text]
            [else
             (hide-structures
              (hide-annotations text))]))))))
