(module insert-keymap mzscheme
  (require (lib "etc.ss")
	   (lib "list.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "pregexp.ss")
           (only (lib "1.ss" "srfi") circular-list)
           (lib "framework.ss" "framework")
           "traversal.ss"
           "long-prefix.ss"
           "utilities.ss"
           "structures.ss"
           (prefix preferences: "diva-preferences.ss"))
  
  
  (provide make-insert-mode)
  
  ;;
  ;; DEBUG STUFFS
  ;;
  
  ;; diva-debug : boolean
  (define diva-debug false)
  
  ;; This function prints something if and only if diva-debug is true.
  (define (diva-printf text . args)
    (when diva-debug
      (apply printf text args)))
  
  (define (alt/meta-prefix str)
    (format "~a~a" (case (system-type)
		     [(macosx macos) "d:"]
		     [(windows)      "m:"]
		     [(unix)         "m:"]
		     [else           "m:"]) str))
  
  ;; Returns a true value if the str appears to be part of an imcomplete literal.
  ;;
  ;; (in-something "\"hello") should return \#".
  (define (in-something? str)
    (let loop ([i 0]
               [in false])
      (define (is? c) (eq? c (string-ref str i)))
      (define (consume c)
        (cond [(eq? c in) (loop (add1 i) false)]
              [(not in) (loop (add1 i) c)]
              [else (loop (add1 i) in)]))
      (cond
        [(>= i (string-length str)) in]
        [(is? #\\) (loop (+ 2 i) in)]
        [(is? #\") (consume #\")]
        [(is? #\|)  (consume #\|)]
        [else (loop (add1 i) in)])))
  
  
  
  
  ;; even it was forgotten in the documentation, key identifiers "back" and "backspace" are the same for map-function.
  ;; 
  (define (make-rigid-keymap keymap fun)
    (send keymap add-function "diva:rigid-rid-off" fun)
    
    (send keymap map-function "up"      "diva:rigid-rid-off")
    (send keymap map-function "down"    "diva:rigid-rid-off")
    
    (send keymap map-function "home"     "diva:rigid-rid-off")
    (send keymap map-function "end"      "diva:rigid-rid-off")
    (send keymap map-function "pageup"   "diva:rigid-rid-off")
    (send keymap map-function "pagedown" "diva:rigid-rid-off")
    
    (send keymap map-function "delete"   "diva:rigid-rid-off")
    (send keymap map-function "insert"   "diva:rigid-rid-off")
    
    (send keymap map-function "leftbutton"   "diva:rigid-rid-off")
    (send keymap map-function "rightbutton"  "diva:rigid-rid-off")
    (send keymap map-function "middlebutton" "diva:rigid-rid-off")
    
    (send keymap set-grab-key-function
	  (lambda (str km editor event)
            (define whitespace
              (list #\nul #\rubout #\backspace #\tab #\return #\space
                    #\linefeed #\newline #\null #\page #\vtab))
            (diva-printf "GRAB KEY FUNCTION WAS CALLED for TEXT: str:~a km: editor: event:%~a%'~n" str (send event get-key-code))
            (if str
                (if ((prefix/string? "diva:") str) false true)
                (let ([key-code (send event get-key-code)])
                  (cond
                    [(and (char? key-code)
                          (not (member key-code whitespace)))
                     (send editor insert key-code)
                     true]
                    [else
                     false]))))))
  
  
  
  (define (blank-string? text)
    (pregexp-match "^\\s*$" text))
  
  
  ;;
  ;; The Insertion Mode
  ;;
  (define-struct Pending (world symbol))
  
  (define make-insert-mode
    (lambda (window actions diva-message get-world set-world set-on-focus-lost
                    set-after-insert-callback set-after-delete-callback
                    interpreter post-exit-hook cmd edit?)
      
      (define (consume-text world pending-open text)
        (if pending-open
            (interpreter (Pending-world pending-open)
                         (make-Verb (make-Command (Pending-symbol pending-open))
                                    false
                                    (make-WhatN (make-Symbol-Noun (string->symbol text)))))
            
            (interpreter world (make-Verb (make-Symbol-Cmd (string->symbol text)) false false))))
      
      
      (define (consume-cmd world symbol)
        (interpreter world (make-Verb (make-Command symbol) false false)))
      
      (let ()
        
        (define world-at-beginning-of-insert #f)
        (define pending-open false)
        (define magic-options-lst false)
        (define magic-option-base false)
        
        (define left-edge-of-insert (send window get-start-position))
        (define right-edge-of-insert (send window get-start-position))
        
        (define (insert-color)
          (preferences:get ' framework:paren-match-color))
        
        (define clear-highlight (lambda () (void)))
        
        (define (get-text)
          (send window get-text left-edge-of-insert right-edge-of-insert))
        
        (define (get-text-to-cursor)
          (send window get-text left-edge-of-insert (send window get-start-position)))
        
        (define (set-text text)
          (send window insert text left-edge-of-insert (send window get-start-position) true))
        
        
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
               (let ([original-pos (send window get-end-position)])
                 (set-world (send actions select/stx world stx/false))
                 (begin-symbol (send window get-start-position)
                               (send window get-end-position))
                 (send window set-position
                       (clamp original-pos
                              (send window get-start-position)
                              (send window get-end-position)))
                 (set-insert&delete-callbacks))]
              [else
               (begin-symbol-insertion)])))
        
        
        
        
        (define (begin-symbol-insertion)
          (define left-point (send window get-start-position))
          
          (define need-space-before
            (and (not (= 0 left-point))
                 (not (eq? #\space
                           (send window get-character (sub1 left-point))))))
          
          (define need-space-after
            (or (= (string-length (send window get-text))
                   left-point)
                (not (eq? #\space
                          (send window get-character (add1 left-point))))))

          (define (prepare-insertion-point!)
            (if need-space-before
                (begin-symbol (add1 left-point) (add1 left-point))
                (begin-symbol left-point left-point))
            (unset-insert&delete-callbacks)
            (unless (empty-selection?)
              (send window delete))
            (when need-space-before
              (send window insert " "))
            (when need-space-after
              (send window insert " ")
              (send window set-position
                    (sub1 (send window get-end-position))))
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
          (clamp n 0 (string-length (send window get-text))))

        
        ;; empty-selection?: -> boolean
        ;; returns true if the selection is empty.
        (define (empty-selection?)
          (define start (box 0))
          (define end (box 0))
          (send window get-position start end)
          (= (unbox start) (unbox end)))
  
        
        ;; clamp: number number number -> number
        ;; clamp x between low and high.
        (define (clamp x low high)
          (min (max x low) high))
        
        
        (define (move-cursor delta)
          (send window set-position 
                (clamp (+ delta (send window get-start-position))
                       left-edge-of-insert right-edge-of-insert)))
        
        (define (move-left) (move-cursor -1))

        (define (move-right) (move-cursor +1))

        (define (move-left*)
          (send window set-position left-edge-of-insert))
        
        (define (move-right*)
          (send window set-position right-edge-of-insert))

        
        (define (delete-backward)
          (cond
            [(= left-edge-of-insert (send window get-start-position))
             (void)
             ;; not quite working yet
             #;(eval-text&cmd 'Younger)
               ]
            [(< left-edge-of-insert
                (send window get-start-position))
             (send window delete)]))
        
        
        (define (delete-forward)
          (when (< (send window get-start-position)
                   right-edge-of-insert)
            (send window delete
                  (send window get-start-position)
                  (add1 (send window get-start-position)))))
        
        
        ;; copy-and-paste from framework/private/keymap.ss. 
        (define (kill-word-forward)
          (let ([sel-start (send window get-start-position)]
                [sel-end (send window get-end-position)])
            (let ([end-box (box sel-end)])
              (send window find-wordbreak #f end-box 'caret)
              (send window kill
                    0
                    sel-start
                    (min right-edge-of-insert (unbox end-box))))))
        
        
        (define (kill-word-backward)
          (let ([sel-start (send window get-start-position)]
                [sel-end (send window get-end-position)])
            (let ([start-box (box sel-start)])
              (send window find-wordbreak start-box #f 'caret)
              (send window kill
                    0
                    (max left-edge-of-insert (unbox start-box))
                    sel-end))))

        
        (define (fill-highlight!)
          (clear-highlight)
          (set! clear-highlight
                (send window highlight-range 
                      (crop (sub1 left-edge-of-insert))
                      (crop (add1 right-edge-of-insert))
                      (insert-color))))
        
        
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
          (let ([txt (get-text)])
            (unless (blank-string? txt)
              (let ([txt (if (in-something? txt)
                             (format "~a~a" txt (in-something? txt))
                             txt)])
                (cond
                  #;[(invalid-insert? txt) => diva-message]
                    [else
                     (consume-text world-at-beginning-of-insert pending-open txt)
                     (begin-symbol-insertion/nothing-pending)])))))
        
        
        (define (eval-cmd symbol)
          (consume-cmd world-at-beginning-of-insert symbol)
          
          (if (or (eq? symbol 'Open)
                  (eq? symbol 'Open-Square))
              (begin
                (set! pending-open (make-Pending world-at-beginning-of-insert symbol))
                (begin-symbol-insertion))
              (begin-symbol-insertion/nothing-pending)))
        
        (define (eval-text&cmd symbol)
          (let ([start (box 0)]
                [end (box 0)])
            (eval-text)
            (eval-cmd symbol)))
        
        (define (make-circular lst) (apply circular-list lst))
        
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
                     (send actions magic-options world-at-beginning-of-insert
                           left-edge-of-insert
                           (string->symbol (get-magic-text)))]
                    [gcd (common-long-prefix-ci options)])
               (set! magic-options-lst (rest (make-circular options)))
               (cond 
                 [(empty? (rest options))
                  (diva-message (format "no completion for ~a" (get-magic-text)))]
                 [(string-ci=? gcd (get-magic-text))
                  (consume-magic)]
                 [else (set-text gcd)]))]))

        
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
        
        
        (define this-insert-mode-exited? false)
        (define (exit)
          (send (send window get-keymap) remove-chained-keymap insert-keymap)
          (clear-highlight)
          (set-on-focus-lost (lambda () (void)))
          (unset-insert&delete-callbacks)
          (post-exit-hook)
          (set! this-insert-mode-exited? true))
        
        
        ;; Returns a new event handler that can handle failure.
        (define-syntax (wrap-up stx)
          (syntax-case stx ()
            [(wrap-up fun ...)
             (syntax/loc stx
               (wrap-up* (lambda () fun) ...))]))
        
        (define (wrap-up* . thunks)
          (lambda (any event)
            (dynamic-wind
             (lambda () (send window begin-edit-sequence))
             (lambda () 
               (with-handlers ([voice-exn? (lambda (exn)
                                             (diva-message (voice-exn-message exn))
                                             (exit))])
                 (for-each (lambda (t) (t)) thunks)))
             (lambda () (send window end-edit-sequence)))))
        
        
        (define (maybe-literal* c  . thunks)
          (if (in-something? (get-text-to-cursor))
              (send window insert c)
              (for-each (lambda (t) (t)) thunks)))
        
        (define-syntax (maybe-literal stx)
          (syntax-case stx ()
            [(_ c e ...)
             (syntax/loc stx
               (maybe-literal* c (lambda () e) ...))]))
        
        (define (magic-or-pass)
          (if (= (string-length (get-text)) 0)
              (eval-text&cmd 'Pass-Wrap)
              (magic-expand-insertion-text)))
        
        (define insert-keymap (make-object keymap:aug-keymap%))
        
        ;; setting up callbacks
        (make-rigid-keymap insert-keymap void)
        
        (send insert-keymap add-function "diva:exit"        (wrap-up (consume&exit)))
        (send insert-keymap add-function "diva:cancel"      (wrap-up (revert&exit)))
        
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
        (send insert-keymap add-function "diva:open-square/contextual" (wrap-up (maybe-literal #\[ (eval-text&cmd 'Open))))
        (send insert-keymap add-function "diva:open-curly" (wrap-up (maybe-literal #\{ (eval-text&cmd 'Open-Square))))
        
        (send insert-keymap add-function "diva:enter" (wrap-up (maybe-literal #\newline (eval-text&cmd 'Enter))))
        
        (send insert-keymap add-function "diva:indent" (wrap-up (eval-text&cmd 'Indent)))
        (send insert-keymap add-function "diva:magic"       (wrap-up (magic-expand-insertion-text)))
        (send insert-keymap add-function "diva:pass" (wrap-up (magic-or-pass)))
        (send insert-keymap add-function "diva:bring" (wrap-up (eval-text&cmd 'Bring)))
        
        (send insert-keymap add-function "diva:left"        (wrap-up (move-left)))
        (send insert-keymap add-function "diva:right"        (wrap-up (move-right)))

        (send insert-keymap add-function "diva:left*"        (wrap-up (move-left*)))
        (send insert-keymap add-function "diva:right*"        (wrap-up (move-right*)))
        
        (preferences:install-insert-mode-bindings insert-keymap)
        
        (when this-insert-mode-exited?
          (error 'insert-keymap "Insert keymap used after it exited."))
        
        
        (set-on-focus-lost consume&exit)
        
        (unset-insert&delete-callbacks)
        
        (send (send window get-keymap) chain-to-keymap insert-keymap #t)
        
        (if edit? (begin-symbol-edit) (begin-symbol-insertion))
        (when cmd (eval-cmd cmd))))))
