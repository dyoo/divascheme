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
           "gui/text-rope-mixin.ss"
           "marker.ss")
  
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
      
      [(struct imperative-op:cleanup ())
       (cleanup-everything a-world a-text update-world-fn update-mred-fn)]
      
      [(struct imperative-op:cleanup-range (start-mark end-mark))
       (cleanup-range start-mark end-mark a-world a-text update-world-fn update-mred-fn)]
      
      [(struct imperative-op:delete-range (start-pos end-pos))
       (delete-range start-pos end-pos a-world a-text
                     update-world-fn update-mred-fn)]
      
      [(struct imperative-op:insert-rope (rope pos))
       (do-insert-rope rope pos a-world a-text update-world-fn update-mred-fn)]
      
      [(struct imperative-op:select-range (start-pos end-pos))
       (select-range start-pos end-pos a-world a-text update-world-fn update-mred-fn)]))
  
  
  ;; imperative-op-changes-text?: imperative-op -> boolean
  ;; Returns true if the operations has the potential for changing the text.
  (define (imperative-op-changes-text? an-op)
    (match an-op
      [(struct imperative-op:indent-range (mark len))
       #t]
      
      [(struct imperative-op:flash-last-sexp ())
       #f]
      
      [(struct imperative-op:move-cursor-position (direction))
       #f]
      
      [(struct imperative-op:transpose (original-world))
       #t]
      
      [(struct imperative-op:cleanup ())
       #t]
      
      [(struct imperative-op:cleanup-range (start-mark end-mark))
       #t]
      
      [(struct imperative-op:delete-range (start-pos end-pos))
       #t]
      
      [(struct imperative-op:insert-rope (rope pos))
       #t]
      
      [(struct imperative-op:select-range (start-pos end-pos))
       #f]))
  
  
  ;; imperative-op-cleans-whitespace?: imperative-op -> boolean
  ;; Returns true if the imperative operation cleans whitespace up.
  (define (imperative-op-cleans-whitespace? an-op)
    (match an-op
      [(struct imperative-op:indent-range (mark len))
       #t]
      
      [(struct imperative-op:flash-last-sexp ())
       #f]
      
      [(struct imperative-op:move-cursor-position (direction))
       #f]
      
      [(struct imperative-op:transpose (original-world))
       #f]
      
      [(struct imperative-op:cleanup ())
       #t]
      
      [(struct imperative-op:cleanup-range (start-mark end-mark))
       #t]
      
      [(struct imperative-op:delete-range (start-pos end-pos))
       #f]
      
      [(struct imperative-op:insert-rope (rope pos))
       #f]
      
      [(struct imperative-op:select-range (start-pos end-pos))
       #f]))
  
  
  
  
  ;; preserve-selection-and-mark: World diva-text% -> World
  ;; Given some function f that does imperative stuff with the text,
  ;; we try to preserve the position of the mark and the selection.
  ;;
  ;; Remember: "Marker" is functional, "marker" is imperative.
  (define (preserve-selection-and-mark a-world a-text some-text-touching-mutator!)
    (let ([selection-start
           (send a-text add-marker! (World-selection-index a-world))]
          [selection-end
           (send a-text add-marker! (World-selection-end-index a-world))]
          [mark-start
           (send a-text add-marker! (World-mark-index a-world))]
          [mark-end
           (send a-text add-marker! (World-mark-end-index a-world))]
          [other-markers
           (World-markers->imperative-marker-list a-world a-text)])
      (some-text-touching-mutator!)
      (let ([new-selection-start (marker-pos selection-start)]
            [new-selection-end (marker-pos selection-end)]
            [new-mark-start (marker-pos mark-start)]
            [new-mark-end (marker-pos mark-end)])
        (copy-struct World a-world
                     [World-cursor-position (index->pos new-selection-start)]
                     [World-selection-length (- new-selection-end new-selection-start)]
                     [World-mark-position (index->pos new-mark-start)]
                     [World-mark-length (- new-mark-end new-mark-start)]
                     [World-markers (imperative-marker-list->world-markers other-markers)]))))
  
  
  ;; World-markers->marker-list: World -> (listof (list symbol marker))
  ;; Given a world, we pull out all the functional markers and construct
  ;; imperative ones that we'll track.
  (define (World-markers->imperative-marker-list a-world a-text)
    (map (lambda (a-marker)
           (list (Marker-name a-marker)
                 (send a-text add-marker! (Marker-index a-marker))))
         (World-markers a-world)))
  
  
  ;; imperative-marker-list->world-markers: (listof (list symbol marker)) -> (listof Marker)
  ;; Turn the imperative markers back into normal marks that we can install into the world.
  (define (imperative-marker-list->world-markers marker-list)
    (map (lambda (symbol&marker)
           (make-Marker
            (first symbol&marker)
            (marker-pos (second symbol&marker))))
         marker-list))
  
  
  
  
  ;; indent-range: symbol number world text% (World -> World) (World -> void) -> World
  (define (indent-range mark len a-world a-text update-world-fn update-mred-fn)
    (let* ([new-pos (max 0 (sub1 (world-marker-position a-world mark)))]
           [new-end-pos (min (string-length (send a-text get-text))
                             (+ new-pos len))])
      (let ([indented-world
             (preserve-selection-and-mark
              (world-clear-marker (update-world-fn a-world) mark)
              a-text
              (lambda ()
                (send a-text tabify-selection new-pos new-end-pos)))])
        
        (let ([final-world (update-world-fn indented-world)])
          (update-mred-fn final-world)
          final-world))))
  
  
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
    (let ([new-world (copy-struct World (update-world-fn world)
                                  [World-cancel original-world])])
      (update-mred-fn new-world)
      new-world))
  
  
  ;; delete-range: number number world text% (World->World) (World -> void) -> World
  (define (delete-range start-pos end-pos world a-text update-world-fn update-mred-fn)
    (send a-text delete start-pos end-pos #f)
    (let ([new-world (update-world-fn world)])
      (update-mred-fn new-world)
      new-world))
  
  
  ;; insert-rope: rope number World text% (World -> World) (World -> void) -> World
  (define (do-insert-rope a-rope a-pos world a-text update-world-fn update-mred-fn)
    (let ([cursor (send a-text get-dstx-cursor)])
      (cond
        ;; All insertions should be structured.
        [(send cursor can-focus-endpos? a-pos)
         (send cursor focus-endpos! a-pos)
         (let ([new-dstxs (send a-text dstx-parse-port (open-input-rope a-rope))])
           (for-each (lambda (a-dstx)
                       (send cursor insert-after! a-dstx))
                     new-dstxs))
         (let ([new-world (update-world-fn world)])
           (update-mred-fn new-world)
           new-world)]
        ;; In the very strange case that they aren't, we have a back-hatch
        ;; to insert unstructurally.  This shouldn't be the normal case, though.
        [else
         (printf "do-insert-rope: unstructured insert of ~s~n" (rope->string a-rope))
         (send a-text set-position a-pos 'same #f #f 'local)
         (insert-rope-in-text a-text a-rope)
         (let ([new-world (update-world-fn world)])
           (update-mred-fn new-world)
           new-world)])))
  
  
  
  ;; cleanup-everything: world text% (World -> World) (World -> void) -> World
  ;; Cleanup everything we can see.
  (define (cleanup-everything world a-text update-world-fn update-mred-fn)
    (cleanup/between 0 (send a-text last-position) world a-text update-world-fn update-mred-fn))
  
  
  ;; cleanup-range: symbol symbol World text% (World -> World) (World -> void) -> World
  ;; Cleanup between the non-whitespace syntax near the start-mark and end-marks.
  (define (cleanup-range start-mark end-mark a-world a-text update-world-fn update-mred-fn)
    (let ([start-pos (max 0 (world-marker-position a-world start-mark))]
          [end-pos (min (send a-text last-position)
                        (world-marker-position a-world end-mark))]
          [world-without-marks
           (world-clear-marker (world-clear-marker a-world start-mark)
                               end-mark)])
      (let ([cursor (send (send a-text get-dstx-cursor) get-functional-cursor)])
        (let ([start-pos (cond [(not (focus-container cursor start-pos))
                                0]
                               [(focus-younger (focus-container cursor start-pos))
                                (cursor-pos (focus-younger (focus-container cursor start-pos)))]
                               [(focus-out (focus-container cursor start-pos))
                                (cursor-pos (focus-out (focus-container cursor start-pos)))]
                               [else
                                (cursor-pos (focus-container cursor start-pos))])]
              [end-pos (cond [(not (focus-container cursor end-pos))
                              (send a-text last-position)]
                             [(focus-older (focus-container cursor end-pos))
                              (cursor-endpos (focus-older (focus-container cursor end-pos)))]
                             [(focus-out (focus-container cursor end-pos))
                              (cursor-endpos (focus-out (focus-container cursor end-pos)))]
                             [else
                              (cursor-endpos (focus-container cursor end-pos))])])
          (cleanup/between start-pos end-pos world-without-marks a-text update-world-fn update-mred-fn)))))
  
  
  
  ;; cleanup/between: number number world text% (World -> World) (World -> void) -> World
  ;; Cleans up between the start and end positions.
  (define (cleanup/between start-index end-index world a-text update-world-fn update-mred-fn)
    (let* ([line (subrope (World-rope world) start-index end-index)]
           [len (rope-length line)]
           [deletions (cleanup-whitespace-operations line)])
      (let* ([clean-world
              (preserve-selection-and-mark
               world
               a-text
               (lambda ()
                 (for-each (lambda (a-del)
                             (let ([pos (+ start-index (deletion-offset a-del))])
                               (send a-text delete pos (+ pos (deletion-len a-del)))))
                           deletions)))]
             [clean-and-tabbed-world
              (preserve-selection-and-mark clean-world
                                           a-text
                                           (lambda ()
                                             (send a-text tabify-selection
                                                   start-index (+ start-index len))))])
        (let ([final-world (update-world-fn clean-and-tabbed-world)])
          (update-mred-fn final-world)
          final-world))))
  
  
  ;; select-range: number number world text% (World -> World) (World -> void) -> World
  (define (select-range start-pos end-pos world a-text update-world-fn update-mred-fn)
    (send a-text diva:set-selection-position start-pos end-pos)
    (let ([new-world (update-world-fn world)])
      (update-mred-fn new-world)
      new-world))
  
  
  
  
  (provide/contract
   [apply-imperative-op (imperative-op?
                         World?
                         (is-a?/c text%)
                         (World? . -> . World?) ;; pull-from-mred
                         (World? . -> . any) ;; push-into-mred
                         . -> . World?)]
   [imperative-op-changes-text? (imperative-op? . -> . boolean?)]
   [imperative-op-cleans-whitespace? (imperative-op? . -> . boolean?)]))