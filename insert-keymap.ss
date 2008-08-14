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
  
  ;; A Pending is a (make-Pending a-world a-sym)
  ;; where a-World is a world, and a-sym is a member of '(Open Open-Square).
  ;; If the user starts typing "(...)", this is a signal that there might be
  ;; possible expansion by templates triggered by parens.
  ;; FIXME: this design is slightly convoluted. What would be a better way?
  (define-struct Pending (world symbol))
  
  
  ;; FIXME: if this takes so many parameters, there is a structural problem.
  (define (make-insert-mode editor diva-message get-world set-world set-on-focus-lost
                            set-after-insert-callback set-after-delete-callback
                            interpret! post-exit-hook cmd edit?)
    
    (define world-at-beginning-of-insert #f)
    (define pending-open false)
    (define magic-options-lst false)
    (define magic-option-base false)
    
    (define left-edge-of-insert (send editor get-start-position))
    (define right-edge-of-insert (send editor get-start-position))
    
    (define clear-highlight (lambda () (void)))
    (define insert-keymap #f)
    
    
    (define (initialize!)
      ;; Keymap stuff.
      (set! insert-keymap (make-insert-keymap))
      (send (send editor get-keymap) chain-to-keymap insert-keymap #t)
      
      ;; Hooking up the other callbacks
      (set-on-focus-lost consume&exit)
      (unset-insert&delete-callbacks)
      (if edit? (begin-symbol-edit) (begin-symbol-insertion))
      (when cmd (eval-cmd cmd)))
    
    
    
    ;; consume-text: World Pending rope -> void
    ;; Send the new text to be filled into the buffer.
    ;; The templating system forces us to consider if the insertion
    ;; is based on the sequence (Open X) or just regular X.
    (define (consume-text world pending-open a-rope)
      (if pending-open
          ;; possible templating with open parens
          (interpret! (Pending-world pending-open)
                      (make-Verb (make-Command (Pending-symbol pending-open))
                                 false
                                 (make-WhatN
                                  (make-Rope-Noun a-rope))))
          
          ;; possible templating without open parens
          (interpret! world
                      (make-Verb (make-InsertRope-Cmd a-rope)
                                 false
                                 false))))
    
    
    (define (consume-cmd world symbol)
      (interpret! world (make-Verb (make-Command symbol) false false)))
    
    (define (insert-color)
      (preferences:get 'framework:paren-match-color))
    
    
    (define (get-text)
      (send editor get-text left-edge-of-insert right-edge-of-insert))
    
    ;; get-rope: -> rope
    ;; Returns the contents between the left and right edge of the
    ;; insertion point.
    (define (get-rope)
      (read-subrope-in-text editor left-edge-of-insert
                            (- right-edge-of-insert left-edge-of-insert)))
    
    (define (get-text-to-cursor)
      (send editor get-text left-edge-of-insert (send editor get-start-position)))
    
    (define (set-text text)
      (send editor insert text left-edge-of-insert (send editor get-start-position) true))
    
    
    (define (set-insert&delete-callbacks)
      (set-after-insert-callback on-insert)
      (set-after-delete-callback on-delete))
    
    (define (unset-insert&delete-callbacks)
      (set-after-insert-callback void)
      (set-after-delete-callback void))
    
    
    (define (begin-symbol-insertion/nothing-pending)
      (set! pending-open false)
      (begin-symbol-insertion))
    
    
    (define (begin-symbol-edit)
      (let* ([world (get-world)]
             [stx/false (find-pos-near (World-cursor-position world)
                                       (World-syntax-list world))]
             [stx/false (and stx/false
                             (first (append
                                     (find-all atomic/stx?
                                               (list stx/false))
                                     (list #f))))])
        (cond
          [stx/false
           (let ([original-pos (send editor get-end-position)])
             (set-world (action:select/stx world stx/false))
             (begin-symbol (send editor get-start-position)
                           (send editor get-end-position))
             (send editor diva:set-selection-position
                   (clamp original-pos
                          (send editor get-start-position)
                          (send editor get-end-position)))
             (set-insert&delete-callbacks))]
          [else
           (begin-symbol-insertion)])))
    
    
    
    
    (define (begin-symbol-insertion)
      (define left-point (send editor get-start-position))
      
      (define need-space-before
        (and (not (= 0 left-point))
             (not (eq? #\space
                       (send editor get-character (sub1 left-point))))))
      
      (define need-space-after
        (or (= (string-length (send editor get-text))
               left-point)
            (not (eq? #\space
                      (send editor get-character (add1 left-point))))))
      
      (define (prepare-insertion-point!)
        (if need-space-before
            (begin-symbol (add1 left-point) (add1 left-point))
            (begin-symbol left-point left-point))
        (unset-insert&delete-callbacks)
        (unless (empty-selection?)
          (send editor delete))
        (when need-space-before
          (send editor insert " "))
        (when need-space-after
          (send editor insert " ")
          (send editor diva:set-selection-position
                (max (sub1 (send editor get-end-position)) 0)))
        (set-insert&delete-callbacks))
      
      (prepare-insertion-point!)
      (fill-highlight!))
    
    
    (define (begin-symbol start-position end-position)
      (diva-message "")
      (set! left-edge-of-insert start-position)
      (set! right-edge-of-insert end-position)
      (fill-highlight!)
      (set! world-at-beginning-of-insert (get-world))
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
         (send editor delete)]))
    
    
    (define (delete-forward)
      (when (< (send editor get-start-position)
               right-edge-of-insert)
        (send editor delete
              (send editor get-start-position)
              (add1 (send editor get-start-position)))))
    
    
    ;; copy-and-paste from framework/private/keymap.ss.
    (define (kill-word-forward)
      (let ([sel-start (send editor get-start-position)]
            [sel-end (send editor get-end-position)])
        (let ([end-box (box sel-end)])
          (send editor find-wordbreak #f end-box 'caret)
          (send editor kill
                0
                sel-start
                (min right-edge-of-insert (unbox end-box))))))
    
    
    (define (kill-word-backward)
      (let ([sel-start (send editor get-start-position)]
            [sel-end (send editor get-end-position)])
        (let ([start-box (box sel-start)])
          (send editor find-wordbreak start-box #f 'caret)
          (send editor kill
                0
                (max left-edge-of-insert (unbox start-box))
                sel-end))))
    
    
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
                    (clamp (add1 right-edge-of-insert)
                           left-
                           (send editor last-position))
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
    
    
    (define (invalid-insert? text)
      (with-handlers
          ([exn:fail? (lambda (exn) (exn-message exn))])
        (read (open-input-string text))
        #f))
    
    (define (eval-text)
      (local
        ((define txt (get-text))
         (define closer (in-something? txt))
         (define a-rope (get-rope)))
        (unless (blank-string? txt)
          (local ((define closed-rope
                    (cond [closer
                           (rope-append a-rope (string->rope closer))]
                          [else a-rope])))
            (let ([txt (if closer
                           (format "~a~a" txt closer)
                           txt)])
              (consume-text world-at-beginning-of-insert
                            pending-open closed-rope)
              (begin-symbol-insertion/nothing-pending))))))
    
    
    (define (eval-cmd symbol)
      (consume-cmd world-at-beginning-of-insert symbol)
      
      (if (or (eq? symbol 'Open)
              (eq? symbol 'Open-Square))
          (begin
            (set! pending-open (make-Pending world-at-beginning-of-insert symbol))
            (begin-symbol-insertion))
          (begin-symbol-insertion/nothing-pending)))
    
    (define (eval-text&cmd symbol)
      ;; FIXME: ugly kludge ahead.  The original architecture of this makes
      ;; the right fix too hard to implement.  I need to think a little more before
      ;; implementing it.  I think that the template system should be more
      ;; involved at this point, rather than the insertion keymap.
      ;; The issue is if we enter the sequence "#(...", we want the templating
      ;; system to introduce "#($expr$)", but not execute the opening command, or else
      ;; we end up getting the silly expression "#(($expr$))".
      (cond [(text-already-introduces-open? (get-text))
             (eval-text)]
            [else
             (eval-text)
             (eval-cmd symbol)]))
    
    (define (text-already-introduces-open? txt)
      (or (string=? txt "#")
          (string=? txt "#s")))
    
    (define (magic-expand-insertion-text)
      (define quote-prefix "^([\"#'`,@]*)")
      
      (define (get-unmagic-prefix)
        (second (regexp-match quote-prefix (get-text))))
      
      (define (get-magic-text)
        (regexp-replace quote-prefix (get-text) ""))
      
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
    
    
    (define (revert&exit)
      (set-world world-at-beginning-of-insert)
      (exit))
    
    (define (consume&exit)
      (if (blank-string? (get-text))
          (revert&exit)
          (begin
            (eval-text)
            (set-world world-at-beginning-of-insert)
            (exit))))
    
    
    
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
    
    
    (define-syntax (maybe-literal stx)
      (syntax-case stx ()
        [(_ c e ...)
         (syntax/loc stx
           (maybe-literal* c (lambda () e) ...))]))
    
    (define (maybe-literal* c . thunks)
      (if (in-something? (get-text-to-cursor))
          (send editor insert c)
          (for-each (lambda (t) (t)) thunks)))
    
    (define (magic-or-pass)
      (if (= (string-length (get-text)) 0)
          (eval-text&cmd 'Pass-Wrap)
          (magic-expand-insertion-text)))
    
    
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
