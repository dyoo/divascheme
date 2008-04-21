(module mred-state mzscheme
  (require (lib "class.ss")
           (lib "struct.ss")
           "utilities.ss"
           "structures.ss"
           "rope.ss"
           "long-prefix.ss"
           "gui/text-rope-mixin.ss")
  
  (provide MrEd-state% MrEd-state<%>)
  
  ;; Here is defined the class of a MrEd-state object.
  ;; Such an object is an interface between DivaScheme and DrScheme.
  ;; More precisely, one DivaScheme instance works on one text instance.
  ;; A MrEd-state with the interpreter is the processing part of a DivaScheme instance.
  ;; However, in the function make-interpreter there is command code and not processing code.
  ;; A MrEd-state supplies to the command side of DivaScheme an interface to speak with a text object:
  ;;  * lock the text
  ;;  * make an image which stands for the content of the text object (a World object)
  ;;  * update the text object from an image (a World object)
  ;;  * unlock the text.
  ;; Such an object is stateless modulo the text object of course 
  ;; (because of the field and a function like get-text returns something according to the text).

  
  ;; Here's our interface.
  (define MrEd-state<%>
    (interface ()
      
      ;; pull-world: world -> world
      ;; Pull a world from what's on screen.
      pull-world
      
      ;; push-world: world -> world
      ;; Force the screen's content to that of the given world.
      push-world))
  
  
  ;;
  ;; MrEd-state%
  ;;
  
  (define MrEd-state%
    (class* object% (MrEd-state<%>)
      
      (super-instantiate ())
      
      ;;
      ;; INIT STUFFS
      ;;
      
      ;; These are the arguments to be given to a MrEd-State when it is built,
      ;; these are the input/ouput objects.
      ;;  * diva-message-init : a function to send messages to the user
      ;;  * window-text-init  : the text object of the window that MrEd-State should take care
      
      
      (init window-text-init)
      (define window-text window-text-init)
      
      
      
      
      
      ;;
      ;; TEXT & SYNTAX STUFFS
      ;;
      
      ;; get-rope: -> rope
      ;; Returns the rope content of the window.
      (define (get-rope)
        (send window-text get-rope))
      
      
      
      ;; STOP COLORING:          (send window-text freeze-colorer)
      ;; CONTINUE HARD COLORING: (send window-text thaw-colorer true true)
      ;; CONTINUE SOFT COLORING: (send window-text thaw-colorer false false)
      
      
      ;;
      ;; CURSOR STUFFS
      ;;
      
      ;; get-cursor-position: -> number
      ;; Returns the cursor position.
      (define (get-cursor-position)
        (index->pos (send window-text get-start-position)))
      
      ;; set-cursor-position: number -> void
      ;; Sets the cursor position.
      (define (set-cursor-position pos)
        (send window-text diva:set-selection-position (pos->index pos)))
      
      
      ;;
      ;; SELECTION STUFFS
      ;;
      ;; set-selection: number number -> void
      ;; Sets the window selection.
      (define (set-selection pos len)
        (if (<= 0 len)
            (begin (send window-text set-position
                         (pos->index pos)
                         (+ len (pos->index pos))
                         #f #f 'local)
                   (send window-text scroll-to-position
                         (pos->index pos)
                         #f
                         (+ len (pos->index pos))
                         'start))
            (set-selection (+ pos len) (- len))))
      
      ;; get-selection-len: -> number
      ;; Returns the selection length.
      (define (get-selection-len)
        (let ([start-pos (send window-text get-start-position)]
              [end-pos   (send window-text get-end-position)])
          (- end-pos start-pos)))
      
      
      ;;
      ;; MARK STUFFS
      ;;
      
      (define (get-mark-position)
        (index->pos (send window-text diva:-get-mark-start-position)))
      
      (define (get-mark-length)
        (let ([mark-start-pos (send window-text diva:-get-mark-start-position)]
              [mark-end-pos   (send window-text diva:-get-mark-end-position)])
	  (- mark-end-pos mark-start-pos)))

      (define (set-mark pos len)
        (if (>= len 0)
	    (send window-text diva:-set-mark (pos->index pos) (+ (pos->index pos) len))
	    (set-mark (+ pos len) (- len))))

      ;;
      ;; TEXT 2 WORLD STUFFS
      ;;
      
      ;; pull-world : World -> World
      ;; Takes the state on screen on our text% and constructs
      ;; a World that reflects it.
      ;; We take an original-world as an optimization: if the text hasn't
      ;; changed, we reuse the parse tree of the original world.
      (define/public (pull-world original-world)
        (update-world-path
         (update-world-mark 
          (update-world-select 
           ;; update-world-text should be first so that if the content of the buffer
           ;; is not parsable, nothing is changed.
           (update-world-text original-world)))))
      
      
      ;; update-world-path: World -> World
      ;; Creates a new world whose World-path now reflects what
      ;; we see in mred.
      (define (update-world-path original-world)
        (copy-struct World original-world
                     [World-path (send window-text get-filename)]))
      
      ;; update-world-text : World -> World
      ;; Takes the rope state in the mred, and returns a new world reflecting
      ;; what's on screen.
      (define (update-world-text original-world)
        (cond
          [(rope=? (World-rope original-world) (get-rope))
           ;; we save the new rope for faster eq? checking next time, but
           ;; reuse the World-syntax-list/lazy structure.
           (copy-struct World original-world
                        [World-rope (get-rope)]
                        [World-syntax-list/lazy
                         (World-syntax-list/lazy original-world)])]
          [else
           (copy-struct World original-world
                        [World-rope (get-rope)]
                        [World-syntax-list/lazy #f])]))
      
      
      ;; update-world-select : World -> World
      ;; Returns the new world whose selection matches what we see
      ;; from the mred side.
      (define (update-world-select original-world)
        (let*-values
            ([(p l) (values (get-cursor-position) (get-selection-len))]
             [(stop-extending)
              (or clear-extension
                  (not (and
                        (= p (World-cursor-position original-world))
                        (= l (World-selection-length original-world)))))])
          (copy-struct World original-world
                       [World-cursor-position (get-cursor-position)]
                       [World-selection-length (get-selection-len)]
                       [World-extension (if stop-extending #f (World-extension original-world))])))
      
      ;; update-world-mark : World -> World
      (define (update-world-mark original-world)
        (if (World-extension original-world)
            original-world
            (copy-struct World original-world
                         [World-mark-position     (get-mark-position)]
                         [World-mark-length (get-mark-length)])))
      
      
      (define clear-extension #f)
      ;;
      ;; WORLD 2 TEXT STUFFS
      ;;
      ;; Pushes changes from the world back into the stateful text%.
      ;; push-world: world -> void
      (define/public (push-world world)
        (with-edit-sequence
         (lambda ()
           (send window-text set-rope (World-rope world))))
        (set-selection (World-cursor-position world) (World-selection-length world))
        (cond [(World-extension world)
               (let ([e (World-extension world)])
                 (set-mark (extension-puck e)
                           (extension-puck-length e))
                 (send window-text scroll-to-position
                       (extension-puck e)
                       #f
                       (+ (extension-puck e) (extension-puck-length e))
                       'none)
                 (set! clear-extension #f)
                 (send window-text diva:-insertion-after-set-position-callback-set
                       (lambda ()
                         (send window-text diva-message "")
                         (set! clear-extension #t)
                         (set-mark 1 0))))]
              [else
               (set-mark (World-mark-position world) (World-mark-length world))
               (send window-text diva:-insertion-after-set-position-callback-set
                     (lambda () (void)))])
        
        (send window-text diva-message (World-success-message world)))
      
      
      (define (with-edit-sequence f)
        (dynamic-wind
         (lambda ()
           (send window-text begin-edit-sequence))
         f
         (lambda ()
           (send window-text end-edit-sequence)))))))