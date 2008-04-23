(module diva-link mzscheme
  (require (lib "etc.ss")
	   (lib "list.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "struct.ss")
           (lib "plt-match.ss")
           (lib "errortrace-lib.ss" "errortrace")
           "interpreter.ss"
           "dot-processing.ss"
           "operations.ss"
           "mred-state.ss"
           "mred-callback.ss"
           "command-keymap.ss"
           "insert-keymap.ss"
           "structures.ss"
           "utilities.ss"
           "diva-central.ss" 
           "rope.ss"
           "dsyntax/dsyntax.ss"
           "gui/clipboard.ss"
           (lib "async-channel.ss")
           "imperative-operations.ss"
           (prefix preferences: "diva-preferences.ss"))
  
  (provide diva-link:frame-mixin)
  (provide diva-link:text-mixin)
  
  
  
  ;; This file is the file which links every parts of DivaScheme into one:
  ;;  - the infrastructure
  ;;  - the different ways of input
  ;;  - the interpreter
  ;;  - and MrEd
  
  
  
  
  
  ;; This is the central location where we handle turning on and off DivaScheme
  ;; for an individual frame.
  (define (diva-link:frame-mixin super%)
    (class super%
      (inherit get-diva-central
               get-definitions-text
               #;get-interactions-text)
      (define started? #f)
      (super-new)
      
      (define (initialize)
        (send (get-diva-central) add-listener handle-diva-central-evt)
        (queue-callback
         (lambda ()
           (when (and (send (get-diva-central) diva-on?)
                      (not started?))
             (startup)))))
      
      (define (startup)
        (send this diva-panel-show)
        (send (get-definitions-text) to-command-mode)
        (send (get-definitions-text) enable-dstx-parsing)
        (set! started? #t))
      
      (define (shutdown)
        (send this diva-panel-hide)
        (send (get-definitions-text) to-normal-mode)
        (send (get-definitions-text) disable-dstx-parsing)
        (set! started? #f))
      
      (define (refresh-keymaps)
        (send (get-definitions-text) refresh-keymaps))
      
      (define/augment (on-tab-change from-tab to-tab)
        (inner (void) on-tab-change from-tab to-tab)
        (when started?
          (send (send from-tab get-defs) diva:-on-loss-focus)
          (send (send to-tab get-defs) diva:-on-loss-focus)
          (send (send from-tab get-defs) to-normal-mode)
          (send (send to-tab get-defs) to-command-mode)))
      
      (define (handle-diva-central-evt evt)
        (match evt
          [(struct diva-switch-on-evt ()) (startup)]
          [(struct diva-switch-off-evt ()) (shutdown)]
          [(struct diva-keymap-changed-evt ()) (refresh-keymaps)]
          [else (void)]))
      
      (define/augment (on-close)
        (inner (void) on-close)
        (send (get-diva-central) remove-listener handle-diva-central-evt))
      
      (initialize)))
  
  
  ;; apply-callback-mixins: text% -> diva-text%
  ;; Given a text, adds callback attachment methods.
  ;; We do this because diva-link was starting to get way too heavy.
  (define (apply-callback-mixins super%)
    (set-position/preserving-marks-callback-mixin
     (insert-and-delete-callback-mixin
      (focus-callback-mixin
       super%))))
  
  
  
  ;;
  ;; THE TEXT
  ;; 
  
  ;;
  ;; OVERLOADING THE WORKING WINDOW
  ;;
  
  ;; Here we are overloading the text window in order to modify the normal mode and add the 3 new modes.
  
  ;; What's would be cool:
  ;;  * we are waiting for an input, i.e. one input to give us a function that give us a function which modify the current state
  ;;  * the buffer is freezed, a World is made, the function run onto this World, which returns a new World or an exception
  ;;  * then, the buffer is updated according to the returned value, the buffer is unfreezed.
  ;; May be better: in fact we have a pipe and while the pipe is not empty, the functions are run and the buffer unfreezes only when the pipe is empty.
  
  ;; Problem: when the object is created, `get-top-level-window' returns false,
  ;; that's why we should wait for the first hit on F4 on to set all the things.
  
  (define (diva-link:text-mixin super%)
    (class (apply-callback-mixins super%)
      (inherit get-top-level-window
               get-keymap
               
               get-rope ;; We assume we'll get this text-rope-mixin
               
               begin-edit-sequence
               end-edit-sequence
               
               diva:-set-on-loss-focus
               diva:-on-loss-focus
               diva:-set-after-insert-callback
               diva:-set-after-delete-callback
               
               get-diva-central
               set-position)
      
      (super-instantiate ())
      
      
      (define -in-insert-mode? #f)
      
      (define/public (set-in-insert-mode b)
        (set! -in-insert-mode? b))
      
      (define/public (in-insert-mode?)
        -in-insert-mode?)
      
      
      ;; decorate-new-dstx: dstx -> dstx
      ;; When we see new dstxs being created, mark them with the 'from-insert-mode
      ;; property if we're currently in insert-mode.
      (define/augment (decorate-new-dstx a-dstx)
        (let ([new-dstx
               (cond
                 [(in-insert-mode?)
                  (dstx-deepmap (lambda (a-dstx)
                                  (dstx-property-set a-dstx 'from-insert-mode #t))
                                a-dstx)]
                 [else
                  a-dstx])])
          (inner new-dstx decorate-new-dstx new-dstx)))
      
      
      
      
      
      ;; When the user changes editor modes (such as from Scheme to text mode),
      ;; we have to reinstall our keymap, because that editor mode has its own
      ;; keymap.  Concretely, switching between text and editor modes screws
      ;; with the binding for '['.
      (define/override (set-surrogate surrogate)
        (cond
          [(send (get-diva-central) diva-on?)
           (diva:-on-loss-focus)
           (uninstall-command-keymap)
           (super set-surrogate surrogate)
           (cond
             [(is-a? surrogate scheme:text-mode%)
              (diva-message "")
              (install-command-keymap)]
             [else
              (diva-message "Disabled: not in scheme mode")])]
          [else
           (super set-surrogate surrogate)]))
      
      
      
      ;; Messaging
      
      ;; diva-label: string -> void
      ;; Displays a label.
      (define (diva-label label) 
        (when (get-top-level-window)
          (send (get-top-level-window) diva-label label)))
      
      
      ;; diva-message: string -> void
      ;; Displays a message to the top-level frame window.
      (define/public (diva-message msg)
        (when (get-top-level-window)
          (send (get-top-level-window) diva-message msg)))
      
      
      ;; error-exn: exn -> void
      ;; Report an exception message.
      (define (error-exn exn)
        (let ([err-msg (format "DivaScheme Error: ~a" exn)])
          (print-error-trace (current-error-port) exn)
          (diva-message err-msg)))
      
      
      ;; error-message: string -> void
      ;; Report a non-critical error to the user.
      (define (error-message str)
        (and str (diva-message str)))
      
      
      ;; diva-question: string string (-> void) (string -> void) -> void
      ;; Asks a question.  If cancelled, calls the cancel callback.  Otherwise,
      ;; calls the answer callback with the provided string.
      (define (diva-question question default cancel answer)
        (send (get-top-level-window) diva-question question default cancel answer))
      
      
      
      
      
      ;;
      ;; COMMAND CENTER STUFFS
      ;;
      
      
      ;; STATE STUFFS
      
      (define current-mred
        (make-object MrEd-state% this))
      
      (define current-world (send current-mred pull-world (make-fresh-world)))
      
      
      
      ;; get-current-world: -> World
      ;; Returns the current world state.
      (define/public (get-current-world)
        current-world)
      
      (define (set-current-world! a-world)
        (set! current-world a-world))
      
      
      
      
      ;; When loading a file, we don't want to add onto the undo
      ;; chain.
      (define/augment (after-load-file success?)
        (set! last-action-load? true)
        (inner void after-load-file success?))
      
      (define last-action-load? false)
      
      
      
      ;; STUFFS about the interpretation of the abstract syntax tree.
      
      ;; This function can be called from other threads,
      ;; and so we are not sure of the value of current-eventspace (if exists).
      (define (push-callback callback)
	(parameterize ([current-eventspace (send (get-top-level-window) get-eventspace)])
          (queue-callback callback)))
      
      
      
      
      
      
      
      
      
      
      ;; INTERPRETATION
      
      
      ;; queue-for-interpretation: ast -> void
      ;; Add a command ast to be interpreted.  In the meantime, we return immediately.
      ;; The command will be eventually run in the main gui thread.
      (define/public (queue-for-interpretation! an-ast)
        (async-channel-put command-mailbox an-ast))
      
      ;; Whenever new events happen, we'll send an operation message to the central world
      ;; mailbox for processing.
      (define command-mailbox (make-async-channel))
      
      ;; A thread will run to process new operations
      (thread (lambda ()
                (let loop ()
                  (let ([new-ast (async-channel-get command-mailbox)])
                    (push-callback
                     (lambda ()
                       (diva-ast-put/wait+world (pull-from-mred) new-ast))))
                  (loop))))
      
      
      ;; pull-from-mred: -> world
      ;; Pulls a new world from mred into us.
      (define (pull-from-mred)
        (print-mem
         'get-mred
         (lambda ()
           (let ([new-world
                  (success-message (send current-mred pull-world (get-current-world))
                                   "")])
             (cond
               [(rope=? (World-rope new-world)
                        (World-rope (get-current-world)))
                new-world]
               [last-action-load?
                (set! last-action-load? false)
                (copy-struct World new-world
                             [World-undo #f])]
               [else
                (copy-struct World new-world
                             [World-undo (get-current-world)])])))))
      
      
      ;; push-into-mred: world -> void
      ;; pushes our world into mred.
      ;; WARNING: this also does things to our world, based on our kludgy implementation
      ;; of world-imperative-actions!  This seems like a design flaw.
      (define (push-into-mred world)
        (unless (World? world)
          (error 'push-into-mred))
        (with-handlers ([voice-exn?
                         (lambda (exn)
                           (printf "~a~n" exn)
                           (error-message (voice-exn-message exn)))]
                        [(lambda args true)
                         (lambda (exn)
                           (printf "~a~n" exn)
                           (error-exn exn))])
          (dynamic-wind
           (lambda ()
             (begin-edit-sequence))
           (lambda ()
             ;; This pushes through the initial changes in the world's content
             ;; into our window text.
             (send current-mred push-world world)
             
             ;; We then apply the other imperative actions:
             (let
                 ([new-world
                   (foldl
                    (lambda (op world)
                      (with-divascheme-handlers
                       world
                       (lambda ()
                         (apply-imperative-op
                          op
                          world this
                          (lambda (w)
                            (send current-mred pull-world w))
                          (lambda (w)
                            (send current-mred push-world w))))))
                    (copy-struct World world [World-imperative-operations empty])
                    (reverse (World-imperative-operations world)))])
               (set-current-world! (copy-struct World new-world
                                                [World-imperative-operations empty]))))
           (lambda ()
             (end-edit-sequence)))))
      
      
      
      ;; with-divascheme-handlers: world (-> world) -> world
      ;; Calls the thunk and returns a world.  If something bad happens, returns the
      ;; default world.
      (define (with-divascheme-handlers default-world-on-exn thunk)
        (dynamic-wind
         (lambda ()
           (begin-edit-sequence))
         (lambda () 
           (with-handlers ([voice-exn?
                            (lambda (exn)
                              (printf "exception: ~s~n" exn)
                              (error-message (voice-exn-message exn))
                              default-world-on-exn)]
                           [voice-exn/world?
                            (lambda (exn)
                              (printf "exception: ~s~n" exn)
                              (error-message (voice-exn/world-message exn))
                              (voice-exn/world-world exn))]
                           [(lambda args true)
                            (lambda (exn)
                              (printf "exception: ~s~n" exn)
                              (error-exn exn)
                              default-world-on-exn)])
             (thunk)))
         (lambda ()
           (end-edit-sequence))))
      
      
      
      ;; interpreter/imperative: ast world -> world
      ;; Evaluate the given ast and the world, and returns the new state of the world.
      (define (interpreter/imperative ast world)
        (let ([new-world (interpreter ast world)])
          (match new-world
            ;; The command may refer to another file path, in which
            ;; case we have to do some tab/frame stuff.
            [(struct SwitchWorld (path inner-ast))
             (let ([frame (handler:edit-file path)])
               (when (eq? this (send frame get-editor))
                 (push-into-mred (pull-from-mred)))
               (send (send frame get-editor) queue-for-interpretation! inner-ast))
             (pull-from-mred)]
            [new-world
             new-world])))
      
      
      
      ;; diva-ast-put/wait+world: world ast -> void
      ;; Applies the ast on the given world.
      ;; This assumes that we are currently in the gui thread.  Do not call
      ;; this function directly unless we know we're in the gui event thread.
      ;; TODO: add defensive check for this condition.
      (define (diva-ast-put/wait+world world ast)
        (push-into-mred
         (with-divascheme-handlers
          world
          (lambda ()
            (interpreter/imperative ast world)))))
      
      
      ;;
      ;; MODE STUFFS
      ;;
      
      ;; check-good-syntax: -> void
      ;; Checking to see if we have a good syntax. If we don't, this should
      ;; raise an exception.
      (define (check-good-syntax)
        (cond
          [(and (get-current-world)
                (rope=? (get-rope)
                        (World-rope (get-current-world))))
           ;; Local optimization: if our rope is equal
           ;; to the one in the current-world, just reuse that.
           (void (World-syntax-list (get-current-world)))]
          [else
           (void (rope-parse-syntax (get-rope)))]))
      
      
      ;; Insertion Mode
      (define to-insert-mode
        (case-lambda 
          [(edit? on-entry on-exit)
           (to-insert-mode edit? on-entry on-exit #f)]
          [(edit? on-entry on-exit cmd)
           (with-divascheme-handlers
            (pull-from-mred)
            (lambda ()
              (on-entry)
              ;; Currently, we're ignoring the return value of make-insert-mode.
              ;; TODO: take advantage of having a value for this, so we don't have to pass
              ;; so many silly callback functions.
              (void
               (make-insert-mode this ;; window
                                 (lambda (msg) (diva-message msg)) ;; diva-message
                                 (lambda () (pull-from-mred)) ;; get-world
                                 (lambda (world) (push-into-mred world)) ;; set-world
                                 ;; set-on-loss-focus
                                 (lambda (callback)
                                   (diva:-set-on-loss-focus callback))
                                 
                                 ;; set-after-insert-callback
                                 (lambda (callback)
                                   (diva:-set-after-insert-callback callback))
                                 
                                 ;; set-after-delete-callback
                                 (lambda (callback)
                                   (diva:-set-after-delete-callback callback))
                                 
                                 ;; interpreter
                                 (lambda (world ast)
                                   (diva-ast-put/wait+world world ast))
                                 
                                 on-exit ;; post-exit-hook
                                 cmd ;; cmd
                                 edit? ;; edit?
                                 ))))]))
      
      
      ;; Command Mode
      (define (new-command-keymap)
        (local (;; get-check-syntax-button: -> (union button% #f)
                (define (get-check-syntax-button)
                  ;; major kludgery: see if there does exist such a button
                  (cond
                    [(and (send this get-tab)
                          (send (send this get-tab) get-frame)
                          (method-in-interface?
                           'syncheck:get-button
                           (object-interface (send (send this get-tab) get-frame))))
                     (send (send (send this get-tab) get-frame)
                           syncheck:get-button)]
                    [else #f]))
                
                (define was-button-enabled? #t)
                
                (define (on-entry)
                  (diva-label "DivaScheme: insertion mode")
                  (diva-message "")
                  
                  (check-good-syntax)
                  
                  (when (get-check-syntax-button)
                    (set! was-button-enabled? (send (get-check-syntax-button) is-enabled?))
                    (send (get-check-syntax-button) enable #f)))
                
                (define (on-exit)
                  (diva-label "DivaScheme: command mode")
                  (when (get-check-syntax-button)
                    (send (get-check-syntax-button) enable was-button-enabled?))))
          (make-command-keymap this
                               (lambda (edit?)
                                 (to-insert-mode edit? on-entry on-exit))
                               (lambda (edit? command)
                                 (to-insert-mode edit? on-entry on-exit command))
                               (lambda (msg)
                                 (diva-message msg))
                               diva-question
                               (lambda (ast)
                                 (diva-ast-put/wait+world (pull-from-mred) ast)))))
      
      (define command-keymap (new-command-keymap))
      
      
      (define (install-command-keymap)
        (send (get-keymap) chain-to-keymap command-keymap #t))
      
      (define (uninstall-command-keymap)
        (send (get-keymap) remove-chained-keymap command-keymap))
      
      (define/public (to-command-mode)
        (install-command-keymap)
        (with-divascheme-handlers
         (pull-from-mred)
         (lambda ()
           (check-good-syntax))))
      
      (define/public (to-normal-mode)
        (diva:-on-loss-focus)
        (uninstall-command-keymap)
        (diva-label false))
      
      
      (define (new-f4-keymap)
        (define f4-keymap (new keymap:aug-keymap%))
        (send f4-keymap add-function "diva:toggle"
              (lambda (any event)
                (send (get-diva-central) switch-toggle)))
        (preferences:install-global-bindings f4-keymap)
        f4-keymap)
      
      
      (define (uninstall-f4-keymap)
        (send (get-keymap) remove-chained-keymap f4-keymap))
      
      (define (install-f4-keymap)
        (send (get-keymap) chain-to-keymap f4-keymap #t))
      
      (define f4-keymap (new-f4-keymap))
      (install-f4-keymap)
      
      
      (define/public (refresh-keymaps)
        (uninstall-f4-keymap)
        (set! f4-keymap (new-f4-keymap))
        (install-f4-keymap)
        (cond
          [(send (get-diva-central) diva-on?)
           (diva:-on-loss-focus)
           (uninstall-command-keymap)
           (set! command-keymap (new-command-keymap))
           (install-command-keymap)]
          [else
           (set! command-keymap (new-command-keymap))]))
      
      
      ;; diva:set-selection-position: number number -> void
      ;; Does most of the ugly details on setting the selection.
      ;; In particular, we don't want to munge the clipboard.
      (define/public diva:set-selection-position
        (case-lambda
          [(start end)
           (set-position start end #f #t 'local)]
          [(start)
           (set-position start 'same #f #t 'local)])))))