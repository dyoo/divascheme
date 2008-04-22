(module imperative-operations mzscheme
  (require (lib "contract.ss")
           (lib "plt-match.ss")
           (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "struct.ss")
           (lib "list.ss")
           "dsyntax/dsyntax.ss"
           "cleanup-whitespace.ss"
           "rope.ss"
           "utilities.ss"
           "traversal.ss"
           "structures.ss"
           "gui/text-rope-mixin.ss")
  
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
       (move-cursor-position
        direction a-world a-text update-world-fn update-mred-fn)]
      
      [(struct imperative-op:transpose (original-world))
       (transpose original-world a-world a-text update-world-fn update-mred-fn)]
      
      [(struct imperative-op:cleanup/pos (pos))
       (cleanup/pos pos a-world a-text update-world-fn update-mred-fn)]
      
      [(struct imperative-op:delete-range (start-pos end-pos))
       (delete-range start-pos end-pos a-world a-text
                     update-world-fn update-mred-fn)]
      
      [(struct imperative-op:insert-rope (rope pos))
       (do-insert-rope rope pos a-world a-text update-world-fn update-mred-fn)]))
  
  
  
  
  
  ;; indent-range: symbol number world text% (World -> World) (World -> void) -> World
  (define (indent-range mark len a-world a-text update-world-fn update-mred-fn)
    (let* ([new-pos (max 0 (sub1 (world-marker-position a-world mark)))]
           [new-end-pos (min (string-length (send a-text get-text))
                             (+ new-pos len))])
      (send a-text tabify-selection new-pos new-end-pos)
      (world-clear-marker (update-world-fn a-world) mark)))
  
  
  ;; indent-range: world text (World -> World) (World -> void) -> World
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
  
  
  
  ;; move-cursor-position: direction world text% (World -> World) (World -> void) -> World
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
  
  
  ;; transpose: world text% (World -> World) (World -> void) -> World
  (define (transpose original-world world window update-world-fn update-mred-fn)
    (send window transpose-sexp (pos->index (World-cursor-position world)))
    (copy-struct World (update-world-fn world)
                 [World-cancel original-world]
                 [World-undo original-world]))
  
  
  ;; delete-range: number number world text% (World->World) (World -> void) -> World
  (define (delete-range start-pos end-pos world a-text update-world-fn update-mred-fn)
    (send a-text delete start-pos end-pos #f)
    world)
  
  
  ;; insert-rope: rope number World text% (World -> World) (World -> void) -> World
  (define (do-insert-rope a-rope a-pos world a-text update-world-fn update-mred-fn)
    (send a-text set-position a-pos 'same #f #f 'local)
    (insert-rope-in-text a-text a-rope)
    world)
  
  
  
  ;; cleanup-pos: number world text% (World -> World) (World -> void) -> World
  ;; Cleanup in the neighborhood of syntax near this position.
  (define (cleanup/pos start-pos world a-text update-world-fn update-mred-fn)
    (let ([cursor (send (send a-text get-dstx-cursor) get-functional-cursor)])
      (cond
        [(not (focus-container cursor start-pos))
         world]
        [else
         (let* ([container (focus-container cursor start-pos)]
                [outer (focus-out container)]
                [before (focus-younger container)]
                [after (focus-older container)])
           (cond
             [outer
              (cleanup/between (cursor-pos outer)
                               (cursor-endpos outer)
                               world a-text update-world-fn update-mred-fn)]
             [(and before after)
              (cleanup/between (cursor-pos before)
                               (cursor-endpos after)
                               world a-text update-world-fn update-mred-fn)]
             [before
              (cleanup/between (cursor-pos before)
                               (cursor-endpos container)
                               world a-text update-world-fn update-mred-fn)]
             [after
              (cleanup/between (cursor-pos container)
                               (cursor-endpos after)
                               world a-text update-world-fn update-mred-fn)]
             [else
              world]))])))
  
  
  
  ;; cleanup/between: number number world text% (World -> World) (World -> void) -> World
  (define (cleanup/between start-index end-index world a-text update-world-fn update-mred-fn)
    (let* ([line (subrope (World-rope world) start-index end-index)]
           [len (rope-length line)])
      (let-values ([(clean-line lst)
                    (cleanup-whitespace line start-index
                                        (list (World-selection-index world)
                                              (World-selection-end-index world)
                                              (World-mark-index world)
                                              (World-mark-end-index world)))])
        (let* ([new-world
                (world-replace-rope world start-index clean-line len)]
               [new-world
                (mark/pos+len (select/pos+len new-world
                                              (index->pos (first lst))
                                              (- (second lst) (first lst)))
                              (index->pos (third lst))
                              (- (fourth lst) (third lst)))])
          
          ;; fixme: don't use update-mred-fn, but rather do the minimal whitespace
          ;; changes we need.
          (send a-text set-rope/minimal-edits (World-rope new-world))
          (send a-text tabify-selection start-index (+ start-index len))
          (let ([world-after-tabify (update-world-fn new-world)])
            (update-mred-fn world-after-tabify)
            world-after-tabify)))))
  
  
  
  ;; fixme: eliminate copy-and-paste from actions.ss!
  
  ;; mark/pos+len : World pos non-negative-integer -> World
  (define (mark/pos+len world pos len)
    (mark/pos (mark/len world len) pos))
  
  ;; mark/len : World non-negative-integer -> World
  (define (mark/len world len)
    (copy-struct World world
                 [World-mark-length len]))
  
  ;; mark/pos : World pos -> World
  (define (mark/pos world pos)
    (copy-struct World world
                 [World-mark-position pos]))
  
  ;; select/pos+len : World pos non-negative-integer -> World
  (define (select/pos+len world pos len)
    (select/pos (select/len world len) pos))
  
  ;; select/pos : World pos -> World
  (define (select/pos world pos)
    (copy-struct World world
                 [World-cursor-position pos]))
  
  ;; select/len : World non-negative-integer -> World
  (define (select/len world len)
    (copy-struct World world
                 [World-selection-length len]))
  
  
  
  
  (provide/contract
   [apply-imperative-op (imperative-op?
                         World?
                         (is-a?/c text%)
                         (World? . -> . World?) ;; pull-from-mred
                         (World? . -> . any) ;; push-into-mred
                         . -> . World?)]))