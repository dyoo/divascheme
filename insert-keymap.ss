(module insert-keymap mzscheme
  (require (lib "etc.ss")
           (lib "list.ss")
           (lib "class.ss")
           (only (lib "1.ss" "srfi") circular-list)
           (lib "framework.ss" "framework")
           "rigid-keymap.ss"
           "traversal.ss"
           "utilities.ss"
           "structures.ss"
           "gui/choose-paren.ss"
           "in-something.ss"
           "rope.ss"
           "gui/text-rope-mixin.ss"
           (prefix action: "actions.ss")
           (prefix preferences: "diva-preferences.ss"))
  
  
  (provide make-insert-mode)
  
  
  ;;
  ;; The Insertion Mode
  ;;
  
  
  ;; FIXME: if this takes so many parameters, there is a structural problem.
  (define (make-insert-mode editor diva-message get-world set-world set-on-focus-lost
                            set-after-insert-callback set-after-delete-callback
                            interpret! post-exit-hook cmd edit?)
    
    (define world-at-beginning-of-insert (get-world))
    (define magic-options-lst false)
    (define magic-option-base false)
    
    (define left-edge-of-insert (send editor get-start-position))
    (define right-edge-of-insert (send editor get-start-position))
    (define original-selected-dstx #f)
    (define need-space-before #f)
    (define need-space-after #f)
    
    
    (define clear-highlight (lambda () (void)))
    (define insert-keymap #f)
    
    
    (define (initialize!)
      ;; Keymap stuff.
      (set! insert-keymap (make-insert-keymap))
      (send (send editor get-keymap) chain-to-keymap insert-keymap #t)
      
      (focus-initial-world-selection-on-atom!)
      ;; Hooking up the other callbacks
      (set-on-focus-lost consume&exit)
      (unset-insert&delete-callbacks)
      (if edit?
          (begin-symbol-edit)
          (begin-symbol-insertion))
      (when cmd (eval-cmd cmd)))
    
    
    ;; do-interpretation: world Protocol-Syntax-tree -> void
    (define (do-interpretation world ast)
      (with-unstructured-decoration
       (lambda ()
         (restore-editor-to-pre-state!)))
      (with-structured-decoration
       (lambda ()
         (interpret! world ast))))
    
    
    ;; delete-insertion-point!: -> void
    ;; Deletes the insertion point.
    (define (delete-insertion-point!)      
      (with-unstructured-decoration
       (lambda ()
         (let ([left left-edge-of-insert]
               [right right-edge-of-insert])
           (unset-insert&delete-callbacks)
           (send editor delete left right)
           (when need-space-after
             (send editor delete left (add1 left)))
           (when need-space-before
             (send editor delete (sub1 left) left))))))
    
    
    ;; Before interpreting a command, we'd like to restore the
    ;; editor state before coming into insert mode, so that it
    ;; syncs up with what's in the World.
    (define (restore-editor-to-pre-state!)
      (with-unstructured-decoration
       (lambda ()
         (delete-insertion-point!)
         (when original-selected-dstx
           (let ([a-cursor (send editor get-dstx-cursor)])
             (send a-cursor focus-endpos! (send editor get-start-position))
             (send a-cursor insert-after! original-selected-dstx)
             ;; Restore the structured nature of the dstx.
             (send a-cursor property-remove! 'from-unstructured-editing)))
         (let ([index (pos->index (World-cursor-position world-at-beginning-of-insert))])
           (send editor diva:set-selection-position
                 index
                 (+ index (World-selection-length world-at-beginning-of-insert))))
         ;; for debugging
         (unless (rope=? (World-rope world-at-beginning-of-insert)
                         (send editor get-rope))
           (error 'restore-editor-to-pre-state!
                  "rope mismatch!  World has~n~s~nEditor has ~n~s~n"
                   (rope->string (World-rope world-at-beginning-of-insert))
                   (rope->string (send editor get-rope)))))))
    
    
    
    ;; consume-text: World Pending rope -> void
    ;; Send the new text to be filled into the buffer.
    ;; The templating system forces us to consider if the insertion
    ;; is based on the sequence (Open X) or just regular X.
    (define (consume-text world a-rope)
      (do-interpretation world
                         (make-Verb (make-InsertRope-Cmd a-rope)
                                    false
                                    false))
      (set! world-at-beginning-of-insert (get-world)))
    
    
    ;; consume-cmd: World symbol -> void
    ;; Evaluates a symbol as a command.
    (define (consume-cmd world symbol)
      (do-interpretation world (make-Verb (make-Command symbol) false false))
      (set! world-at-beginning-of-insert (get-world)))
    
    
    ;; insert-color: -> color%
    ;; Returns the color we use to highlight the insertion point.
    (define (insert-color)
      (preferences:get 'framework:paren-match-color))
    
    
    ;; get-insertion-point-text: -> string
    ;; Returns the string contents between the left and right edge of the insertion point.
    (define (get-insertion-point-text)
      (send editor get-text left-edge-of-insert right-edge-of-insert))
    
    
    ;; get-insertion-point-rope: -> rope
    ;; Returns the contents between the left and right edge of the
    ;; insertion point.
    (define (get-insertion-point-rope)
      (read-subrope-in-text editor left-edge-of-insert
                            (- right-edge-of-insert left-edge-of-insert)))
    
    
    
    (define (with-unstructured-decoration f)
      (let ([old-val (send editor in-unstructured-editing?)])
        (dynamic-wind (lambda () (send editor set-in-unstructured-editing? #t))
                      f
                      (lambda () (send editor set-in-unstructured-editing? old-val)))))
    
    (define (with-structured-decoration f)
      (let ([old-val (send editor in-unstructured-editing?)])
        (dynamic-wind (lambda () (send editor set-in-unstructured-editing? #f))
                      f
                      (lambda () (send editor set-in-unstructured-editing? old-val)))))
    
    
    ;; set-text: string -> void
    ;; Assign to the selection point text.
    (define (set-text text)
      (with-unstructured-decoration
       (lambda ()
         (send editor insert text left-edge-of-insert right-edge-of-insert true))))
    
    
    ;; set-insert&delete-callbacks: -> void
    ;; Attach callbacks to the editor, so we can detect insertions and deletions
    ;; and readjust our insertion point accordingly.
    (define (set-insert&delete-callbacks)
      (set-after-insert-callback on-insert)
      (set-after-delete-callback on-delete))
    
    ;; unset-insert&delete-callbacks
    ;; Detach our callbacks.
    (define (unset-insert&delete-callbacks)
      (set-after-insert-callback void)
      (set-after-delete-callback void))
    
    
    ;; focus-world-selection-on-atom!: -> void
    ;; Given the current world, focus it on the structure we're on, or otherwise
    ;; don't affect the world.
    ;; This code is kludgy, but there are a lot of edge cases here.
    (define (focus-initial-world-selection-on-atom!)
      (let* ([stx/false (find-pos-near (World-cursor-position world-at-beginning-of-insert)
                                       (World-syntax-list world-at-beginning-of-insert))])
        (cond
          ;; Editing a symbol
          [edit?
           (let ([atom-stx/false (and stx/false
                                      (first (append
                                              (find-all atomic/stx?
                                                        (list stx/false))
                                              (list #f))))])
             (cond
               [(and atom-stx/false
                     (in-syntax?
                      (World-cursor-position world-at-beginning-of-insert)
                      atom-stx/false))
                (set! world-at-beginning-of-insert
                      (action:select/stx world-at-beginning-of-insert atom-stx/false))
                (set-world world-at-beginning-of-insert)]
               [else
                (void)]))]
          [else
           
           ;; Inserting a new symbol: the selection must be completely surrounding a
           ;; syntax.
           ;; Otherwise, we should push the selection off to the side of
           ;; the atom,to avoid screwing up the atom by unstructured edit.
           (cond
             [stx/false
              (cond
                [;; If we overlap, snap to it for replacement.
                 (overlap? (World-cursor-position world-at-beginning-of-insert)
                           (+ (World-selection-length world-at-beginning-of-insert)
                              (World-cursor-position world-at-beginning-of-insert))
                           (syntax-position stx/false)
                           (+ (syntax-span stx/false)
                              (syntax-position stx/false)))
                 (set! world-at-beginning-of-insert
                       (action:select/stx world-at-beginning-of-insert stx/false))
                 (set-world world-at-beginning-of-insert)]
                [else
                 ;; Otherwise, we're not overlapping and we can just leave things be.
                 (void)])]
             [else
              (void)])])))
    
    
    ;; overlap?: number number number number -> boolean
    ;; Returns true if the half-open intervals [start-1, end-1) [start-2, end-2)
    ;; overlap.
    (define (overlap? start-1 end-1 start-2 end-2)
      (and (< start-1 end-2)
           (< start-2 end-1)))
    
    
    
    ;; save-original-selected-dstx!: -> void
    ;; Saves the dstx at the selection.
    (define (save-original-selected-dstx!)
      (let ([a-cursor (send editor get-dstx-cursor)])
        (cond
          [(send a-cursor can-focus-pos? (send editor get-start-position))
           (send a-cursor focus-pos! (send editor get-start-position))
           ;; Temporarily mark the selected dstx as unstructured.  When we
           ;; reinstate it, we'll restore it.
           (send a-cursor property-set! 'from-unstructured-editing #t)
           (set! original-selected-dstx (send a-cursor cursor-dstx))]
          [else
           (set! original-selected-dstx #f)])))
    
    
    ;; begin-symbol-edit: -> void
    ;; Set up the insertion point around the currently focused atom. 
    (define (begin-symbol-edit)
      (with-unstructured-decoration
       (lambda ()
         (let* ([stx/false (find-pos-near (World-cursor-position world-at-beginning-of-insert)
                                          (World-syntax-list world-at-beginning-of-insert))]
                [stx/false (and stx/false
                                (first (append
                                        (find-all atomic/stx?
                                                  (list stx/false))
                                        (list #f))))])
           (cond
             [stx/false
              (let ([original-pos (send editor get-end-position)])
                (let ([selected-world (action:select/stx world-at-beginning-of-insert stx/false)])
                  (set-world selected-world)
                  (set! world-at-beginning-of-insert selected-world))
                (set! need-space-before #f)
                (set! need-space-after #f)
                (let* ([start-pos (send editor get-start-position)]
                       [end-pos (send editor get-end-position)]
                       [selection-rope-before-insert
                        (read-subrope-in-text editor start-pos (- end-pos start-pos))])
                  (save-original-selected-dstx!)
                  (begin-symbol start-pos end-pos)
                  (send editor delete)
                  (insert-rope-in-text editor selection-rope-before-insert)
                  (send editor diva:set-selection-position
                        (clamp original-pos start-pos end-pos))
                  (set-insert&delete-callbacks)))]
             [else
              (begin-symbol-insertion)])))))
    
    
    ;; begin-symbol-insertion: -> void
    ;; Set up the insertion point, replacing the current selection. 
    (define (begin-symbol-insertion)
      (with-unstructured-decoration
       (lambda ()
         (let ([left-point (send editor get-start-position)]
               [right-point (send editor get-end-position)])
           
           (define (prepare-insertion-point!)
             (if need-space-before
                 (begin-symbol (add1 left-point) (add1 left-point))
                 (begin-symbol left-point left-point))
             (unset-insert&delete-callbacks)
             (cond [(empty-selection?)
                    (set! original-selected-dstx #f)]
                   [else
                    (save-original-selected-dstx!)
                    (send editor delete)])
             (when need-space-before
               (send editor insert " "))
             (when need-space-after
               (send editor insert " ")
               (send editor diva:set-selection-position
                     (max (sub1 (send editor get-end-position)) 0)))
             (set-insert&delete-callbacks))
           
           (begin
             (set! need-space-before
                   (and (not (= 0 left-point))
                        (not (char-whitespace?
                              (send editor get-character (sub1 left-point))))))
             
             (set! need-space-after
                   (and (not (= (send editor last-position) right-point))
                        (not (char-whitespace?
                              (send editor get-character right-point)))))
             (prepare-insertion-point!)
             (fill-highlight!))))))
    
    
    
    
    (define (begin-symbol start-position end-position)
      (diva-message "")
      (set! left-edge-of-insert start-position)
      (set! right-edge-of-insert end-position)
      (fill-highlight!)
      (set! magic-options-lst false)
      (set! magic-option-base false))
    
    
    (define (crop n)
      (clamp n 0 (string-length (send editor get-text))))
    
    
    ;; empty-selection?: -> boolean
    ;; returns true if the selection is empty.
    (define (empty-selection?)
      (define start (box 0))
      (define end (box 0))
      (send editor get-position start end)
      (= (unbox start) (unbox end)))
    
    
    ;; clamp: number number number -> number
    ;; clamp x between low and high.
    (define (clamp x low high)
      (min (max x low) high))
    
    
    (define (snap-to-edges)
      (let ([snapped-pos
             (clamp (send editor get-start-position)
                    left-edge-of-insert right-edge-of-insert)])
        (unless (= snapped-pos (send editor get-start-position))
          (send editor diva:set-selection-position snapped-pos))))
    
    (define (move-up)
      (send editor move-position 'up)
      (snap-to-edges))
    
    (define (move-down)
      (send editor move-position 'down)
      (snap-to-edges))
    
    (define (move-left)
      (send editor move-position 'left)
      (snap-to-edges))
    
    (define (move-right)
      (send editor move-position 'right)
      (snap-to-edges))
    
    (define (move-left*)
      (send editor diva:set-selection-position left-edge-of-insert))
    
    (define (move-right*)
      (send editor diva:set-selection-position right-edge-of-insert))
    
    
    (define (delete-backward)
      (cond
        [(= left-edge-of-insert (send editor get-start-position))
         (void)
         ;; not quite working yet
         #;(eval-text&cmd 'Younger)
         ]
        [(< left-edge-of-insert
            (send editor get-start-position))
         (with-unstructured-decoration
          (lambda () (send editor delete)))]))
    
    
    (define (delete-forward)
      (when (< (send editor get-start-position)
               right-edge-of-insert)
        
        (with-unstructured-decoration
         (lambda ()
           (send editor delete
                 (send editor get-start-position)
                 (add1 (send editor get-start-position)))))))
    
    
    ;; copy-and-paste from framework/private/keymap.ss.
    (define (kill-word-forward)
      (let ([sel-start (send editor get-start-position)]
            [sel-end (send editor get-end-position)])
        (let ([end-box (box sel-end)])
          (send editor find-wordbreak #f end-box 'caret)
          (with-unstructured-decoration
           (lambda ()
             (send editor kill
                   0
                   sel-start
                   (min right-edge-of-insert (unbox end-box))))))))
    
    
    (define (kill-word-backward)
      (let ([sel-start (send editor get-start-position)]
            [sel-end (send editor get-end-position)])
        (let ([start-box (box sel-start)])
          (send editor find-wordbreak start-box #f 'caret)
          (with-unstructured-decoration
           (lambda ()
             (send editor kill
                   0
                   (max left-edge-of-insert (unbox start-box))
                   sel-end))))))
    
    
    (define (fill-highlight!)
      (clear-highlight)
      (local ((define left-
                (cond
                  [(= (send editor
                            position-line left-edge-of-insert)
                      (send editor
                            position-line (crop (sub1 left-edge-of-insert))))
                   (crop (sub1 left-edge-of-insert))]
                  [else
                   left-edge-of-insert])))
        (set! clear-highlight
              (send editor highlight-range
                    left-
                    (crop (add1 right-edge-of-insert))
                    (insert-color)))))
    
    
    ;; on-insert: number number -> void
    ;; When new text is inserted into the window in insert mode, we
    ;; want to extend the insert point's highlighted region.
    (define (on-insert start length)
      (set! right-edge-of-insert (+ right-edge-of-insert length))
      (fill-highlight!))
    
    
    (define (on-delete start length)
      (set! right-edge-of-insert (- right-edge-of-insert length))
      (fill-highlight!))
    
    
    (define (eval-text)
      (local
        ((define txt (get-insertion-point-text))
         (define closer (in-something? txt))
         (define a-rope (get-insertion-point-rope)))
        (unless (blank-string? txt)
          (local ((define closed-rope
                    (cond [closer
                           (rope-append a-rope (string->rope closer))]
                          [else a-rope])))
            (let ([spaced-rope
                   (rope-append
                    (if need-space-before
                        (string->rope " ")
                        (string->rope ""))
                    (rope-append closed-rope
                                 (if need-space-after
                                     (string->rope " ")
                                     (string->rope ""))))])
              (consume-text world-at-beginning-of-insert spaced-rope)
              (begin-symbol-insertion))))))
    
    
    (define (eval-cmd symbol)
      (consume-cmd world-at-beginning-of-insert symbol)
      (begin-symbol-insertion))
    
    
    (define (eval-text&cmd symbol)
      ;; FIXME: ugly kludge ahead.  The original architecture of this makes
      ;; the right fix too hard to implement.  I need to think a little more before
      ;; implementing it.  I think that the template system should be more
      ;; involved at this point, rather than the insertion keymap.
      ;; The issue is if we enter the sequence "#(...", we want the templating
      ;; system to introduce "#($expr$)", but not execute the opening command, or else
      ;; we end up getting the silly expression "#(($expr$))".
      (cond [(text-already-introduces-open? (get-insertion-point-text))
             (eval-text)]
            [else
             (eval-text)
             (eval-cmd symbol)]))
    
    (define (text-already-introduces-open? txt)
      (or (string=? txt "#")
          (string=? txt "#s")))
    
    
    
    
    
    ;; restore-editor-to-pre-state!: -> void
    ;; Revert the buffer to its pre-state, and exit.
    (define (revert&exit)
      (restore-editor-to-pre-state!)
      (set-world world-at-beginning-of-insert)
      (exit))
    
    
    ;; consume&exit: -> void
    ;; Evaluate the text and then exit.
    (define (consume&exit)
      (if (blank-string? (get-insertion-point-text))
          (revert&exit)
          (begin
            (eval-text)
            ;; Subtle: we need to remove the new insertion point, so we call
            ;; restore-editor-to-pre-state! right after evaluation.
            (restore-editor-to-pre-state!)
            (set-world world-at-beginning-of-insert)
            (exit))))
    
    
    
    ;; exit: -> void
    ;; Detach our keymap, clear our state.
    (define (exit)
      (send (send editor get-keymap) remove-chained-keymap insert-keymap)
      (clear-highlight)
      (set-on-focus-lost (lambda () (void)))
      (unset-insert&delete-callbacks)
      (post-exit-hook))
    
    
    ;; Returns a new event handler that can handle failure.
    (define-syntax (wrap-up stx)
      (syntax-case stx ()
        [(wrap-up fun ...)
         (syntax/loc stx
           (wrap-up* (lambda () fun) ...))]))
    
    
    (define (wrap-up* . thunks)
      (lambda (any event)
        (dynamic-wind
         (lambda () (send editor begin-edit-sequence))
         (lambda ()
           (with-handlers ([voice-exn? (lambda (exn)
                                         (diva-message (voice-exn-message exn))
                                         (exit))])
             (for-each (lambda (t) (t)) thunks)))
         (lambda () (send editor end-edit-sequence)))))
    
    
    ;; Slightly special command here, since we need to communicate with
    ;; the editor.
    (define ((open-paren/contextual literal default-cmd) editor evt)
      ((wrap-up (maybe-literal
                 literal
                 (eval-text&cmd (get-contextual-open-cmd editor default-cmd))))
       editor evt))
    
    
    
    ;; get-text-to-cursor: -> string
    ;; Gets the text from the left-edge-of-insert up to the current cursor position.
    (define (get-text-to-cursor)
      (send editor get-text left-edge-of-insert (send editor get-start-position)))
    
    ;; maybe-literal*: character (-> void) -> void
    ;; Possibly insert the character c, depending on the content of the insertion point
    ;; and the cursor position.  Otherwise, evaluate the thunks.
    (define (maybe-literal* c thunk)
      (cond [(in-something? (get-text-to-cursor))
             (send editor insert c)]
            [else
             (thunk)]))
    
    ;; Syntax to automatically wrap the thunk around.
    (define-syntax (maybe-literal stx)
      (syntax-case stx ()
        [(_ c e)
         (syntax/loc stx
           (maybe-literal* c (lambda () e)))]))
    
    
    
    ;; magic-or-pass: -> void
    ;; Either evaluate the magic expansion, or do a Pass-Wrap.
    (define (magic-or-pass)
      (if (= (string-length (get-insertion-point-text)) 0)
          (eval-text&cmd 'Pass-Wrap)
          (magic-expand-insertion-text)))
    
    ;; magic-expand-insertion-text: -> void
    ;; Do autocompletion.
    (define (magic-expand-insertion-text)
      (define quote-prefix "^([\"#'`,@]*)")
      
      (define (get-unmagic-prefix)
        (second (regexp-match quote-prefix (get-insertion-point-text))))
      
      (define (get-magic-text)
        (regexp-replace quote-prefix (get-insertion-point-text) ""))
      
      (define (consume-magic)
        (set-text (string-append (get-unmagic-prefix)
                                 (first magic-options-lst)))
        (diva-message "")
        (set! magic-option-base (first magic-options-lst))
        (set! magic-options-lst (rest magic-options-lst)))
      
      (cond
        [(and magic-option-base
              (string=? magic-option-base (get-magic-text)))
         (consume-magic)]
        [else
         (let* ([options ;; options guaranteed not-empty: contains at least (get-text).
                 (action:magic-options
                  world-at-beginning-of-insert
                  left-edge-of-insert
                  (string->symbol (get-magic-text)))])
           (set! magic-options-lst (rest (apply circular-list options)))
           (cond
             [(empty? (rest options))
              (diva-message
               (format "no completion for ~a" (get-magic-text)))]
             [else
              (consume-magic)]))]))
    
    
    
    ;; make-insert-keymap: -> keymap
    ;; Creates a new keymap that we intend to chain up an existing keymap.
    (define (make-insert-keymap)
      (define insert-keymap (make-object keymap:aug-keymap%))
      (install-rigid-keymap-bindings! insert-keymap)
      
      (send insert-keymap add-function "diva:exit" (wrap-up (consume&exit)))
      (send insert-keymap add-function "diva:cancel" (wrap-up (revert&exit)))
      
      (send insert-keymap add-function "diva:delete-backward" (wrap-up (delete-backward)))
      (send insert-keymap add-function "diva:delete-forward" (wrap-up (delete-forward)))
      
      (send insert-keymap add-function "diva:kill-word-forward" (wrap-up (kill-word-forward)))
      (send insert-keymap add-function "diva:kill-word-backward" (wrap-up (kill-word-backward)))
      
      (send insert-keymap add-function "diva:space" (wrap-up (maybe-literal #\space (eval-text))))
      
      (send insert-keymap add-function "diva:close" (wrap-up (maybe-literal #\) (eval-text&cmd 'Close))))
      (send insert-keymap add-function "diva:close-square" (wrap-up (maybe-literal #\] (eval-text&cmd 'Close))))
      (send insert-keymap add-function "diva:close-curly" (wrap-up (maybe-literal #\} (eval-text&cmd 'Close))))
      
      (send insert-keymap add-function "diva:open" (wrap-up (maybe-literal #\( (eval-text&cmd 'Open))))
      (send insert-keymap add-function "diva:open-square" (wrap-up (maybe-literal #\[ (eval-text&cmd 'Open))))
      (send insert-keymap add-function "diva:open-square/contextual" (open-paren/contextual #\[ 'Open))
      (send insert-keymap add-function "diva:open-curly" (wrap-up (maybe-literal #\{ (eval-text&cmd 'Open-Square))))
      
      (send insert-keymap add-function "diva:enter" (wrap-up (maybe-literal #\newline (eval-text&cmd 'Enter))))
      
      (send insert-keymap add-function "diva:indent" (wrap-up (eval-text&cmd 'Indent)))
      (send insert-keymap add-function "diva:magic" (wrap-up (magic-expand-insertion-text)))
      (send insert-keymap add-function "diva:pass" (wrap-up (magic-or-pass)))
      (send insert-keymap add-function "diva:bring" (wrap-up (eval-text&cmd 'Bring)))
      
      (send insert-keymap add-function "diva:up" (wrap-up (move-up)))
      (send insert-keymap add-function "diva:down" (wrap-up (move-down)))
      (send insert-keymap add-function "diva:left" (wrap-up (move-left)))
      (send insert-keymap add-function "diva:right" (wrap-up (move-right)))
      
      (send insert-keymap add-function "diva:left*" (wrap-up (move-left*)))
      (send insert-keymap add-function "diva:right*" (wrap-up (move-right*)))
      
      (preferences:install-insert-mode-bindings insert-keymap)
      
      insert-keymap)
    
    
    (initialize!)))
