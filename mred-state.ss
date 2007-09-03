(module mred-state mzscheme
  (require (lib "class.ss")
           (lib "etc.ss")
           (lib "struct.ss")
           (lib "mred.ss" "mred")
           (lib "errortrace-lib.ss" "errortrace")
           "dot-processing.ss"
           "utilities.ss"
           "structures.ss"
           "rope.ss")
  
  (provide MrEd-state%)
  
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
    (diva-printf "B: ~a ~n" b)
    b)


  ;;
  ;; MrEd-state%
  ;;

  (define MrEd-state%
    (class object%

      (super-instantiate ())

      ;;
      ;; INIT STUFFS
      ;;
      
      ;; These are the arguments to be given to a MrEd-State when it is built,
      ;; these are the input/ouput objects.
      ;;  * diva-message-init : a function to send messages to the user
      ;;  * window-text-init  : the text object of the window that MrEd-State should take care

      (init diva-message-init window-text-init)
      
      (define diva-message- diva-message-init)
      (define window-text   window-text-init)

      (define (diva-message text . args)
        ;;(apply diva-printf text args)
        ;; (printf "diva-message: ~a ~a~n" text args)
        (apply diva-message- text args))
      
      
      ;;
      ;; MESSAGE STUFFS
      ;;
      
      (define/public (critical-error exn)
        (let ([err-msg (format "DivaScheme Error: ~a" exn)])
          (print-error-trace (current-error-port) exn)
          (diva-message err-msg))
        (raise exn))
      
      
      (define/public (error-message str)
        (and str (diva-message str)))
      
      (define/public (message str)
        (diva-message str))
      
      
      ;;
      ;; TEXT & SYNTAX STUFFS
      ;;
      
      (define/public (get-rope)
        (send window-text diva:-get-rope))
      
      (define/public (get-syntax-list)
        (parse-syntax (open-input-rope (get-rope))))
      
      (define/public (update-text rope)
        (send window-text diva:-update-text rope))
      
      
      ;; STOP COLORING:          (send window-text freeze-colorer)
      ;; CONTINUE HARD COLORING: (send window-text thaw-colorer true true)
      ;; CONTINUE SOFT COLORING: (send window-text thaw-colorer false false)
      
      
      ;;
      ;; CURSOR STUFFS
      ;;
      
      (define/public (get-cursor-position)
        (index->pos (send window-text get-start-position)))
      
      (define/public (set-cursor-position pos)
        (send window-text diva:set-selection-position (pos->index pos)))

      
      ;;
      ;; SELECTION STUFFS
      ;;

      (define/public (set-selection pos len)
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

      (define/public (get-selection-len)
        (let ([start-pos (send window-text get-start-position)]
              [end-pos   (send window-text get-end-position)])
          (- end-pos start-pos)))


      ;;
      ;; MARK STUFFS
      ;;
      
      (define/public (get-mark-position)  
	(index->pos (send window-text diva:-get-mark-start-position)))

      (define/public (get-mark-length)
	(let ([mark-start-pos (send window-text diva:-get-mark-start-position)]
	      [mark-end-pos   (send window-text diva:-get-mark-end-position)])
	  (- mark-end-pos mark-start-pos)))

      (define/public (set-mark pos len)
	(if (>= len 0)
	    (send window-text diva:-set-mark (pos->index pos) (+ (pos->index pos) len))
	    (set-mark (+ pos len) (- len))))


      
      
      ;;
      ;; TEXT 2 WORLD STUFFS
      ;;

      ;; update-world : World -> World
      ;; Takes the state on screen on our text% and constructs a World that reflects
      ;; it.
      (define/public (update-world world)
        (update-world-path
         (update-world-mark 
          (update-world-select 
           ;; update-world-text should be first so that if the content of the buffer
           ;; is not parsable, nothing is changed.
           (update-world-text world)))))
      
      
      ;; update-world-path: World -> World
      (define (update-world-path world)
        (copy-struct World world
                     [World-path (send window-text get-filename)]))
      
      ;; update-world-text : World -> World
      ;; Takes the rope state in the mred, and returns a new world reflecting
      ;; what's on screen.
      (define/public (update-world-text world)
        (cond
             [(rope=? (World-rope world) (get-rope))
              world]
             [else
              (copy-struct World world
                           [World-rope (get-rope)]
                           [World-syntax-list (get-syntax-list)])]))
      
      
      ;; update-world-select : World -> World
      (define (update-world-select world)
        (copy-struct World world
                     [World-cursor-position   (get-cursor-position)]
                     [World-selection-length  (get-selection-len)]))
      
      ;; update-world-mark : World -> World
      (define (update-world-mark world)
        (copy-struct World world
                     [World-mark-position     (get-mark-position)]
                     [World-mark-length       (get-mark-length)]))
      
      
      
      ;;
      ;; WORLD 2 TEXT STUFFS
      ;;
      ;; Pushes changes from the world back into the stateful text%.
      
      ;; update-mred : World -> World
      ;; We are updating the mark before the selection so that the selection is not hidden by the mark.
      (define/public (update-mred world)
        (local
            (;; update-mred-text : World -> World
             (define (update-mred-text world)
               (unless (rope=? (World-rope world) (get-rope))
                 (update-text (World-rope world)))
               world)
             
             ;; select-mred : World -> World
             (define (select-mred world)
               (set-selection (World-cursor-position world) (World-selection-length world))
               world)
             
             ;; mark-mred : World -> World
             (define (mark-mred world)
               (set-mark (World-mark-position world) (World-mark-length world))
               world))
          (select-mred
           (mark-mred
            (update-mred-text world))))))))