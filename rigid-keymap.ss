(module rigid-keymap mzscheme
  (require (lib "class.ss")
           "utilities.ss")
  
  
  (provide install-rigid-keymap-bindings!)
  
  
  
  ;; diva-debug : boolean
  ;; Small logger debugging.
  (define current-rigid-keymap-debug (make-parameter #f))
  (define (diva-printf text . args)
    (when (current-rigid-keymap-debug)
      (apply printf text args)))
  
  
  ;; even it was forgotten in the documentation, key identifiers "back" and "backspace" are the same for map-function.
  ;;
  ;; Modifies a keymap and redirects many of the control characters to things that don't do anything.
  (define (install-rigid-keymap-bindings! keymap)
    (send keymap add-function "diva:rigid-rid-off" void)
    
    (send keymap map-function "up" "diva:rigid-rid-off")
    (send keymap map-function "down" "diva:rigid-rid-off")
    
    (send keymap map-function "home" "diva:rigid-rid-off")
    (send keymap map-function "end" "diva:rigid-rid-off")
    (send keymap map-function "pageup" "diva:rigid-rid-off")
    (send keymap map-function "pagedown" "diva:rigid-rid-off")
    
    (send keymap map-function "delete" "diva:rigid-rid-off")
    (send keymap map-function "insert" "diva:rigid-rid-off")
    
    (send keymap map-function "leftbutton" "diva:rigid-rid-off")
    (send keymap map-function "rightbutton" "diva:rigid-rid-off")
    (send keymap map-function "middlebutton" "diva:rigid-rid-off")
    
    (send keymap set-grab-key-function rigid-grab-key))
  
  
  ;; rigid-grab-key: string-or-false keymap text% event
  ;; Our grab-key-function watches key events and only lets ones through that
  ;; either are regular printable key events, or calls to a function that
  ;; have the "diva:" prefix to them.  Other keystrokes are ignored.
  (define (rigid-grab-key callback-name/false km editor event)
    (define no-further-dispatch-needed #t)
    (define more-dispatch-needed #f)
    (diva-printf "GRAB KEY FUNCTION WAS CALLED for TEXT: str:~a km: editor: event:%~a%'~n" callback-name/false (send event get-key-code))
    
    (cond
      [(not callback-name/false)
       (let ([key-code (send event get-key-code)]
             [meta-down? (send event get-meta-down)]
             [control-down? (send event get-control-down)])
         (cond
           [(or meta-down? control-down?)
            no-further-dispatch-needed]
           [(printable-char? key-code)
            (dynamic-wind (lambda () 
                            (send editor set-in-unstructured-editing? #t)) 
                          (lambda () 
                            (send editor insert key-code)) 
                          (lambda () 
                            (send editor set-in-unstructured-editing? #f)))
            no-further-dispatch-needed]
           [else
            more-dispatch-needed]))]
      [(diva-prefixed-string? callback-name/false)
       more-dispatch-needed]
      [else
       no-further-dispatch-needed]))
  
  
  ;; printable-char?: maybe-char -> boolean
  ;; Returns true if the input is a character, and is considered
  ;; a normal printable character.
  (define (printable-char? a-keycode)
    (and (char? a-keycode)
         (not (member a-keycode whitespace))))
  
  
  ;; Our list of whitespace is a little less permissive because
  ;; we have to handle the control characters too.
  (define whitespace
    (list #\nul #\rubout #\backspace #\tab #\return #\space
          #\linefeed #\newline #\null #\page #\vtab))
  
  
  ;; diva-prefixed-string?: string -> boolean
  ;; Returns true if the string starts with the word "diva:".
  (define diva-prefixed-string? (prefix/string? "diva:")))