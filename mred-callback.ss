(module mred-callback mzscheme
  (require (lib "etc.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred"))
  
  ;; Here we are defining mixins for text% and canvas% classes
  ;; whose instanciations would be handled by a mred-state%.
  ;; These mixins are setting callbacks and variables.
  
  (provide focus-callback-mixin
           insert-and-delete-callback-mixin
           set-position/preserving-marks-callback-mixin)
  
  
  
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
  
  
  
  ;;
  ;; FOCUS STUFF.
  ;;
  ;; Adds hooks for when we lose focus.
  ;; Main usage pattern: trigger the exit from insertion mode
  ;; when we lose window focus.
  ;;
  ;; Adds a diva:-set-on-loss-focus method to the superclass.
  (define (focus-callback-mixin super%)
    (class super%
      (define/override (on-focus on?)
        (super on-focus on?)
        (unless on?
          (diva:-on-loss-focus)))
      
      (define on-loss-focus (lambda () ()))
      
      (define/public (diva:-set-on-loss-focus fun)
        (set! on-loss-focus fun))
      
      (define/public (diva:-on-loss-focus)
        (on-loss-focus))
      
      (super-new)))
  
  
  
  ;; INSERT/DELETE STUFF
  ;; Creates a class with two exposed methods:
  ;;
  ;; diva:-set-after-insert-callback
  ;; diva:-set-after-delete-callback
  (define (insert-and-delete-callback-mixin super%)
    (class super%
      (define after-insert-callback (lambda (start end) (void)))
      (define after-delete-callback (lambda (start end) (void)))
      
      (define/public (diva:-set-after-insert-callback fun)
        (set! after-insert-callback fun))
      
      (define/augment (after-insert start end)
        (after-insert-callback start end)
        (inner void after-insert start end))
      
      (define/public (diva:-set-after-delete-callback fun)
        (set! after-delete-callback fun))
      
      (define/augment (after-delete start end)
        (after-delete-callback start end)
        (inner void after-delete start end))
      
      (super-new)))
  
  
  
  ;;
  ;; INSERTION MODE CALLBACKS
  ;;
  ;;
  ;; diva:-insertoin-after-set-position-callback-set
  ;; diva:-insertion-after-set-position-callback-reset
  
  (define (set-position/preserving-marks-callback-mixin super%)
    (class super%
      (inherit highlight-range get-text)
      
      ;; When the position changes, the insertion should be exited.
      (define insertion-after-set-position-callback-old
        (lambda () ()))
      (define insertion-after-set-position-callback
        insertion-after-set-position-callback-old)
      
      (define/public (diva:-insertion-after-set-position-callback-set callback)
        (set! insertion-after-set-position-callback-old
              insertion-after-set-position-callback)
        (set! insertion-after-set-position-callback callback))
      
      (define/public (diva:-insertion-after-set-position-callback-reset)
        (set! insertion-after-set-position-callback
              insertion-after-set-position-callback-old))
      ;;
      ;; CALLBACKS CALLS
      ;;
      (define/augment (after-set-position)
        (insertion-after-set-position-callback))
      
      (define/augment (after-insert start len)
        (inner void after-insert start len)
        (preserve-mark start len))
      
      (define/augment (after-delete start len)
        (inner void after-delete start len)
        (preserve-mark start (- len)))
      
      
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
        (set! mark-end-position end-pos)
        (show-mark))
      
      (define mark-color (send the-color-database find-color "Orange"))
      
      (define (show-mark)
        (unless (= mark-start-position mark-end-position)
          (set! hide-mark
                (highlight-range mark-start-position
                                 mark-end-position
                                 mark-color
                                 false
                                 false
                                 'low))))
      
      (define hide-mark (lambda () ()))
      
      (define (preserve-mark start delta)
        (hide-mark)
        (when (<= start mark-start-position)
          (set! mark-start-position (+ mark-start-position delta))
          (set! mark-end-position (+ mark-end-position delta)))
        (unless (and (legal-mark-pos? mark-start-position)
                     (legal-mark-pos? mark-end-position))
          (set! mark-start-position 0)
          (set! mark-end-position 0))
        (show-mark))
      
      (define (legal-mark-pos? pos)
        (<= 0 pos (string-length (get-text))))
      (super-new))))
