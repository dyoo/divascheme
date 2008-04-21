(module imperative-operations mzscheme
  (require (lib "contract.ss")
           (lib "plt-match.ss")
           (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "struct.ss")
           "utilities.ss"
           "traversal.ss"
           "structures.ss")
  
  ;; We may want to put this in mred-state...
  
  
  ;; apply-imperative-op: imperative-op world text% update-world-fn update-mred-fn
  ;;
  ;; fixme: do we really need update-world-fn and update-mred-fn?
  (define (apply-imperative-op an-op a-world a-text update-world-fn update-mred-fn)
    (match an-op
      [(struct imperative-op:indent-range (mark len))
       (indent-range mark len a-world a-text update-world-fn update-mred-fn)]
      [(struct imperative-op:flash-last-sexp ())
       (flash-last-sexp a-world a-text update-world-fn update-mred-fn)]
      [(struct imperative-op:move-cursor-position (direction))
       (move-cursor-position direction a-world a-text update-world-fn update-mred-fn)]
      [(struct imperative-op:transpose (original-world))
       (transpose original-world a-world a-text update-world-fn update-mred-fn)]))
  
  
  
  
  
  ;; indent-range: symbol number world text% (World -> World) (World -> World) -> World
  (define (indent-range mark len a-world a-text update-world-fn update-mred-fn)
    (let* ([new-pos (max 0 (sub1 (world-marker-position a-world mark)))]
           [new-end-pos (min (string-length (send a-text get-text))
                             (+ new-pos len))])
      (send a-text tabify-selection new-pos new-end-pos)
      (world-clear-marker (update-world-fn a-world) mark)))
  
  
  ;; indent-range: world text (World -> World) (World -> World) -> World
  (define (flash-last-sexp world window update-world-fn update-mred-fn)
    (let ([pos (send window get-backward-sexp (send window get-start-position))])
      ;; We queue-callback this up since the highlighting fights with
      ;; insertion mode otherwise, plus this is a low-priority item.
      (queue-callback
       (lambda ()
         (when (and pos
                    (send window get-forward-sexp pos)
                    (open-paren?
                     (string-ref (send window get-text pos (add1 pos)) 0)))
           (send window flash-on pos (send window get-forward-sexp pos))))
       #f)
      world))
  
  
  ;; open-paren?: char -> boolean
  ;; Returns true if achar is a character that looks like an open parenthesis.
  (define (open-paren? achar)
    (and (char? achar)
         (or (char=? achar #\()
             (char=? achar #\[)
             (char=? achar #\{))))
  
  
  
  ;; move-cursor-position: direction world text% (World -> World) (World -> World) -> World
  (define (move-cursor-position direction world window
                                update-world-fn update-mred-fn)
    (define (callback world)
      (send window diva:-insertion-after-set-position-callback-set (lambda () ()))
      (send window set-position (pos->index (World-cursor-position world))
            'same #f #f 'default) ;; this may be the puck or the selection
      (send window move-position direction)
      (send window diva:-insertion-after-set-position-callback-reset)
      (let ([b (box 0)])
        (send window get-position b)
        (copy-struct World world
                     [World-cursor-position (index->syntax-pos (unbox b))]
                     [World-selection-length 0])))
    (let ([w (if (World-extension world)
                 (with-selection-extension world callback)
                 (callback world))])
      (update-mred-fn w)
      w))
  
  
  ;; transpose: world text% (World -> World) (World -> World) -> World
  (define (transpose original-world world window update-world-fn update-mred-fn)
    (send window transpose-sexp (pos->index (World-cursor-position world)))
    (copy-struct World (update-world-fn world)
                 [World-cancel original-world]
                 [World-undo original-world]))
  
  
  
  (provide/contract
   [apply-imperative-op (imperative-op?
                         World?
                         (is-a?/c text%)
                         (World? . -> . World?) ;; pull-from-mred
                         (World? . -> . any) ;; push-into-mred
                         . -> . World?)]))