(module actions mzscheme
  (require (lib "class.ss")
           (lib "struct.ss")
           (lib "etc.ss")
           (lib "list.ss")
           (lib "plt-match.ss")
           (lib "mred.ss" "mred")
           "traversal.ss"
           "structures.ss"
           "utilities.ss"
           "templates.ss"
           "tag-state.ss"
           "tag-reader.ss"
           "dot-processing.ss"
           "text-rope-mixin.ss"
           "rope.ss"
           (lib "errortrace-lib.ss" "errortrace")
           (lib "framework.ss" "framework"))
  
  (define voice-debug false)
  (define (voice-printf . args)
    (when voice-debug
      (apply printf args)))
  
  
  
  (define (open-paren? achar)
    (and (char? achar)
         (or (char=? achar #\()
             (char=? achar #\[)
             (char=? achar #\{))))
  
  (define (close-paren? achar)
    (and (char? achar)
         (or (char=? achar #\))
             (char=? achar #\])
             (char=? achar #\}))))
  
  (define (pipe? achar)
    (and (char? achar)
         (char=? achar #\|)))
  
  (define (double-quote? achar)
    (and (char? achar)
         (char=? achar #\")))
  
  
  (provide actions%)
  
  (define actions%
    (class object%
      
      ;; public functions for interpreter
      (public select/stx
              select/pos+end
              set-cursor-position
              exchange
              mark/stx
              unmark
              selection->mark
              first/selection
              last/selection
              delete
              close
              dedouble-ellipsis
              open
              symbol
              holder
              bring
              push
              copy
              cut
              paste
              enter/selection
              join/selection
              transpose
              indent/selection
              magic-options
              magic/expand
              magic-bash)
      
      ;; public functions for tests
      (public select/pos+len              
              cursor-at-selection-end
              recompute-selection/insert
              recompute-selection/delete
              recompute-selection/replace
              mark/pos+len
              set-mark-position
              mark-at-start
              mark-at-end
              recompute-mark/insert
              recompute-mark/delete
              recompute-mark/replace
              cleanup-text/pos+len
              cleanup-text/selection
              insert
              delete/pos+len
              delete/selection
              delete/mark
              replace/selection
              enter/pos+len
              join/pos+len
              magic/completion)
      
      
      (super-instantiate ())

      ;; select/stx : World syntax -> World
      (define (select/stx world stx)
        (select/pos+len world (syntax-position stx) (syntax-span stx)))
      
      ;; select/pos+len : World pos non-negative-integer -> World
      (define (select/pos+len world pos len)
        (select/pos (select/len world len) pos))
        
      ;; select/pos : World pos -> World
      (define (select/pos world pos)
        (copy-struct World world
                     [World-cursor-position  pos]))

      ;; select/len : World non-negative-integer -> World
      (define (select/len world len)
        (copy-struct World world
                     [World-selection-length len]))
      
      ;; set-cursor-position : World pos -> World
      (define (set-cursor-position world pos)
        (select/pos+len world pos 0))

     
      (define (select/pos+end world start end)
        (select/pos+len world start (- end start)))
      
      ;; cursor-at-selection-end : World -> World
      (define (cursor-at-selection-end world)
        (select/pos+len world (World-selection-end-position world) 0))

      ;; recompute-selection/insert : World pos non-negative-integer -> World
      (define (recompute-selection/insert world pos len)
        (let-values ([(new-cursor-index new-selection-length)
                      (compute-new-selection/insert (World-cursor-index world) (World-selection-length world) (pos->index pos) len)])
          (select/pos+len world (index->pos new-cursor-index) new-selection-length)))

      ;; recompute-selection/delete : World pos non-negative-integer -> World
      (define (recompute-selection/delete world pos len)
        (let-values ([(new-cursor-index new-selection-length)
                      (compute-new-selection/delete (World-cursor-index world) (World-selection-length world) (pos->index pos) len)])
          (select/pos+len world (index->pos new-cursor-index) new-selection-length)))

      ;; recompute-selection/replace : World pos non-negative-integer non-negative-integer -> World
      (define (recompute-selection/replace world pos b-len a-len)
        (let-values ([(new-cursor-index new-selection-length)
                      (compute-new-selection/replace (World-cursor-index world) (World-selection-length world) (pos->index pos) b-len a-len)])
          (select/pos+len world (index->pos new-cursor-index) new-selection-length)))

      ;; shrink-selection : World -> World
      ;; very specific to fill and command.
      (define (shrink-selection world)
        (let ([new-world
               (select/pos+len world
                               (add1 (World-cursor-position world))
                               (sub1 (World-selection-length world)))])
          new-world))

      ;; mark/stx : World syntax -> World
      (define (mark/stx world stx)
        (mark/pos+len world (syntax-position stx) (syntax-span stx)))
      
      ;; mark/pos+len : World pos non-negative-integer -> World
      (define (mark/pos+len world pos len)
        (mark/pos (mark/len world len) pos))
        
      ;; mark/pos : World pos -> World
      (define (mark/pos world pos)
        (copy-struct World world
                     [World-mark-position pos]))

      ;; mark/len : World non-negative-integer -> World
      (define (mark/len world len)
        (copy-struct World world
                     [World-mark-length len]))
      
      ;; set-mark-position : World pos -> World
      (define (set-mark-position world pos)
        (mark/pos+len world pos 0))
      
      ;; mark-at-start : World -> World
      (define (mark-at-start world)
        (mark/len world 0))
      
      ;; mark-at-end : World -> World
      (define (mark-at-end world)
        (set-mark-position world (World-mark-end-position world)))
      
      ;; unmark : World -> World
      (define (unmark world)
        (mark-at-start world))
     
      ;; exchange : World -> World
      (define (exchange world)
        (mark/pos+len
         (select/pos+len world (World-mark-position world) (World-mark-length world))
         (World-cursor-position world)
         (World-selection-length world)))
      
      ;; The selection is marked.
      ;; After no selection, only a cursor position.
      ;; selection->mark : World -> World
      (define (selection->mark world)
        (mark/pos+len (select/len world 0)
                      (World-cursor-position world)
                      (World-selection-length world)))
      
      ;; recompute-mark/insert : World pos non-negative-integer -> World
      (define (recompute-mark/insert world pos len)
        (let-values ([(new-mark-index new-mark-length)
                      (compute-new-selection/insert (World-mark-index world) (World-mark-length world) (pos->index pos) len)])
          (mark/pos+len world (index->pos new-mark-index) new-mark-length)))

      ;; recompute-mark/delete : World pos non-negative-integer -> World
      (define (recompute-mark/delete world pos len)
        (let-values ([(new-mark-index new-mark-length)
                      (compute-new-selection/delete (World-mark-index world) (World-mark-length world) (pos->index pos) len)])
          (mark/pos+len world (index->pos new-mark-index) new-mark-length)))

      ;; recompute-mark/replace : World pos non-negative-integer non-negative-integer -> World
      (define (recompute-mark/replace world pos b-len a-len)
        (let-values ([(new-mark-index new-mark-length)
                      (compute-new-selection/replace (World-mark-index world) (World-mark-length world) (pos->index pos) b-len a-len)])
          (mark/pos+len world (index->pos new-mark-index) new-mark-length)))
      
      
      ;; indent/pos+len: World pos non-negative-integer -> World
      (define (indent/pos+len world pos len)
        (when (< len 0)
          (error 'indent/pos+len))
        
        (let-values ([(world mark) (world-new-marker world pos)])
          (queue-imperative-action
           world
           (lambda (world window)
             (let* ([new-pos (max 0 (sub1 (world-marker-position world mark)))]
                    [new-end-pos (min (string-length (send window get-text))
                                      (+ new-pos len))])
               (send window tabify-selection new-pos new-end-pos)
               (world-clear-marker world mark))))))
      
      ;; indent/selection : World -> World
      (define (indent/selection world)
        (indent/pos+len world
                        (World-cursor-position world)
                        (World-selection-length world)))
      
      
      ;; cleanup-text/pos+len
      (define (cleanup-text/pos+len world pos len)
        #; (indent/pos+len
            world
            pos
            len)
        (local
            (
             ;; line-map : (World pos -> World) -> World pos len -> World
             (define ((line-map fun/pos) world pos len)
               ;; aux : pos non-negative-integer -> World
               (define (aux world pos len)
                 (if (> 0 len)
                     world
                     (let* ([old-len (rope-length (World-rope world))]
                            [world (fun/pos world pos)]
                            [new-len (rope-length (World-rope world))]
                            [diff (- new-len old-len)]
                            [-pos (add1 (line-end-pos (World-rope world) pos))]
                            [-len (- (+ len diff) (- -pos pos))])
                       (if (<= (pos->index -pos) new-len)
                           (aux world -pos -len)
                           world))))
               (let* ([-pos (line-pos (World-rope world) pos)]
                      [-len (+ len (- pos -pos))])
                 (aux world -pos -len)))
             
             
             ;; cleanup-text/pos : World pos -> World
             ;; This function eats all the extra white-space on the line of pos
             ;; and reindents it.
             (define (cleanup-text/pos world pos)
               (print-mem*
                'cleanup-text/pos
                (let* ([start-index (pos->index (line-pos (World-rope world) pos))]
                       [line (line-rope/pos (World-rope world) pos)]
                       [len (rope-length line)])
                  (let-values ([(clean-line lst)
                                (cleanup-whitespace start-index line
                                                    (list (World-selection-index world)
                                                          (World-selection-end-index world)
                                                          (World-mark-index world)
                                                          (World-mark-end-index world)))])
                    
                    (let ([new-world (world-replace-rope world start-index clean-line len)])
                      (mark/pos+len (select/pos+len new-world
                                                    (index->pos (first lst))
                                                    (- (second lst) (first lst)))
                                    (index->pos (third lst))
                                    (- (fourth lst) (third lst)))))))))
          
          (let* ([start-pos (line-pos (World-rope world) pos)]
                 [new-world ((line-map (lambda (world pos) (cleanup-text/pos world pos)))
                             world pos len)])
            (indent/pos+len
             new-world
             start-pos
             (+ len (- pos start-pos))))))
      
      
      
      ;; cleanup-text/selection : World -> World
      ;; This function eats all the extra white-space on the line of the cursor-position,
      ;; and reindents it.
      (define (cleanup-text/selection world)
        (cleanup-text/pos+len world (World-cursor-position world) (World-selection-length world)))
      
      
      ;; insert : World pos rope -> World
      (define (insert world pos a-rope)
        (let ([len (rope-length a-rope)]
              [new-world (world-insert-rope world (pos->index pos) a-rope)])
          (cleanup-text/pos+len
           (recompute-mark/insert
            (recompute-selection/insert new-world pos len) pos len) pos len)))
      
      
      ;; delete/stx : World syntax -> World
      (define (delete/stx world stx)
        (delete/pos+len world (syntax-position stx) (syntax-span stx)))
      
      ;; delete/pos+len : World pos non-negative-integer -> World
      (define (delete/pos+len world pos len)
        (if (= len 0)
            world
            (let ([new-world (world-delete-rope world (pos->index pos) len)])
              (cleanup-text/pos+len
               (recompute-mark/delete
                (recompute-selection/delete
                 new-world pos len) pos len) pos 0))))
      
      ;; delete/selection : World -> World
      (define (delete/selection world)
        (delete/pos+len world
                        (World-cursor-position world)
                        (World-selection-length world)))
      
      ;; delete/mark : World -> World
      (define (delete/mark world)
        (delete/pos+len world (World-mark-position world) (World-mark-length world)))
      
      
      ;; replace/selection : World rope -> World
      ;; FIXME: bad performance on large files.
      (define (replace/selection world a-rope)
        (print-mem*
         'replace/selection
         (let ([len (rope-length a-rope)]
               [new-world (world-replace-rope world
                                              (World-cursor-index world)
                                              a-rope
                                              (World-selection-length world))])
           (cleanup-text/selection (recompute-mark/replace
                                    (select/len new-world len)
                                    (World-cursor-position world)
                                    (World-selection-length world)
                                    len)))))
      
      ;; close : World -> World
      (define (close world)
        ;; flash-last-sexp!: world text% -> world
        ;; 
        (define (flash-last-sexp! world window)
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
        
        (let*-values
            ([(stxy) (find-siblings-ellipsis (World-cursor-position world)
                                             (World-syntax-list world))]
             [(new-world)
              (if (not stxy)
                  (holder/end world)
                  (let* ([stx (first stxy)]
                         [sty (second stxy)]
                         [world (delete/pos+len world
                                                (syntax-position stx)
                                                (- (syntax-end-position sty)
                                                   (syntax-position stx)))])
                    (holder/end
                     (if (join-ellipsis/stx? sty)
                         (anti-join/cursor world)
                         world))))])
          (queue-imperative-action new-world flash-last-sexp!)))
      
      
      ;; update-ellipsis : World syntax -> World
      (define (update-ellipsis world stx)
        (let ([str/false (update-ellipsis/stx? stx)])
          (if str/false
              (world-replace-rope world (syntax-index stx) str/false (syntax-span stx))
              world)))
      
      
      ;; dedouble-ellipsis : World -> World
      (define (dedouble-ellipsis world)
        (print-mem 
         'dedouble-ellipsis
         (lambda ()
           (with-handlers ([voice-exn? (lambda args world)])
             (let ([stxy  (find-siblings-ellipsis (World-cursor-position world) (World-syntax-list world))])
               (unless stxy (raise (make-voice-exn "unable to find the next placeholder")))
               (let* ([stx   (first stxy)]
                      [sty   (second stxy)]
                      [world (update-ellipsis world sty)]
                      [text
                       (rope-append (if (enter-ellipsis/stx? sty)
                                        (string->rope " \n ")
                                        (string->rope " "))
                                    (get-subrope/stx (World-rope world) stx))])
                 (if (and (= 0 (World-selection-length world))
                          (= (syntax-position stx) (World-cursor-position world)))
                     world
                     (insert world (syntax-end-position stx) text))))))))
      
      ;; open : boolean World symbol/false pos/false non-negative-integer non-negative-integer boolean -> World
      (define (open square? world symbol/false pos/false template-number magic-number template/magic-wrap?)
        (command world symbol/false pos/false true
                 square? template-number magic-number template/magic-wrap?))
      
      ;; symbol : World symbol pos/false non-negative-integer non-negative-integer boolean -> World
      (define (symbol world symbol pos/false template-number magic-number template/magic-wrap?)
        (command world symbol pos/false
                 false false template-number
                 magic-number template/magic-wrap?))
      
      
      ;; cursor-position-is-quoted?: world -> boolean
      ;; Returns true if the cursor's current location is at a quoted
      ;; position.
      (define (cursor-position-is-quoted? world)
        (and (<= 0
                 (sub1 (World-cursor-index world))
                 (sub1 (rope-length (World-rope world))))
             (quoting-char?
              (rope-ref (World-rope world)
                        (sub1 (World-cursor-index world))))))
      
      
      ;; command : World symbol/false pos/false boolean boolean non-negative-integer non-negative-integer boolean -> World
      (define (command world symbol/false pos/false open?
                       square? template-number magic-number template/magic-wrap?)
        (let* ([world
                (if pos/false
                    (set-cursor-position world pos/false)
                    world)] ; thus we keep the current selection
               [symbol/false
                (and symbol/false (magic/expand world
                                                (World-cursor-position world)
                                                symbol/false
                                                magic-number
                                                template/magic-wrap?))])
          (let-values ([(template number-of-templates)
                        (lookup-template symbol/false
                                         template-number
                                         open?
                                         template/magic-wrap?)])
            (let* ([text (string->rope
                          (format " ~a " (if template
                                             (shape-paren (and square? 'Square) template)
                                             symbol/false)))]
                   [text (cond
                           [(cursor-position-is-quoted? world)
                            (subrope text 1)]
                           [else text])]
                   [world (if (<= number-of-templates 1)
                              world
                              (success-message
                               world
                               (format "template ~a of ~a"
                                       (add1 (modulo template-number number-of-templates))
                                       number-of-templates)))]
                   [world
                    (print-mem*
                            'command-indent/selection
                            (indent/selection
                             (replace/selection
                              (dedouble-ellipsis world) text)))])
              (if template
                  (holder world)
                  (step-to-the-right world))))))
      
      
      (define (step-to-the-right world)
        (let ([stx (find-pos-sibling-forward (World-selection-end-position world)
                                             (World-syntax-list world))])
          (if (and stx
                   (placeholder/stx? stx))
              (select/stx world stx)
              (set-cursor-position world (World-selection-end-position world)))))
      
      (define holder
        (opt-lambda (world [base (World-cursor-position world)] [backward? false])
          (or (holder-helper world base backward?)
              (raise (make-voice-exn "Unable to find the next placeholder.")))))
      
      ;; holder : World [pos] [boolean]?-> World
      (define (holder-helper world base backward?)
        (let ([stx (find-placeholder backward? base (World-syntax-list world))])
          (and stx (select/stx world stx))))
      
      ;; holder/end : World -> World
      (define (holder/end world)
        (define maximum-jump-to-holder 30)
        (define (rope-count-nonwhitespace a-rope)
          (- (rope-length a-rope) (rope-count-whitespace a-rope)))
        (define (too-far-away? old-w new-w)
          (let* ([a (World-selection-end-index old-w)]
                 [b (World-selection-index new-w)]
                 [small (min a b)]
                 [big (max a b)])
            (< 
             maximum-jump-to-holder
             (rope-count-nonwhitespace (subrope (World-rope world) small big)))))
        
        (define (no-holder)
          (let ([stx (find-pos-parent (World-cursor-position world) (World-syntax-list world))])
            (if stx
                (set-cursor-position world (syntax-end-position stx))
                world)))
        
        (let ([w (holder-helper world (World-cursor-position world) false)])
          (if (or (not w) (too-far-away? world w))
              (no-holder)
              w)))
      
      ;; first/selection : World -> World
      (define (first/selection world)
        (let ([stx/false (find-pos-parent (World-cursor-position world) (World-syntax-list world))])
          (if stx/false
              (select/stx world (first (stx->lst stx/false)))
              (raise (make-voice-exn "No containing expression.")))))
      
      ;; last/selection : World -> World
      (define (last/selection world)
        (let ([stx/false (find-pos-parent (World-cursor-position world) (World-syntax-list world))])
          (if stx/false
              (select/stx world (first (reverse (stx->lst stx/false))))
              (raise (make-voice-exn "No containing expression.")))))
      
      ;; delete : World -> World
      (define (delete world)
        (delete/selection (dedouble-ellipsis world)))
      
      ;; bring : World -> World
      (define (bring world)
        (define (originally-unmarked?)
          (= 0 (World-mark-length world)))
        (define (mark-default world)
          (match (find-pos-fill-forward (World-selection-end-position world) (World-syntax-list world))
            [#f (raise (make-voice-exn "No mark, so nothing to fill with."))]
            [stx (mark/stx world stx)]))
        
        (let* ([world (if (originally-unmarked?) (mark-default world) world)]
               [world (extend-mark-to-newline world)]
               [fill-rope (rope-append
                           (string->rope " ")
                           (rope-append (World-mark world)
                                        (string->rope " ")))]
               [world (id (replace/selection (dedouble-ellipsis (delete/mark world)) fill-rope))]
               [world (if (= (World-cursor-position world)
                             (World-mark-position   world))
                          (set-mark-position world (World-selection-end-position world))
                          world)]
               [stx/false (find-pos-mark-forward (World-mark-position world) (World-syntax-list world))]
               [world (if (and (not (originally-unmarked?))
                               stx/false)
                          (mark/stx world stx/false)
                          world)])
          (holder/end world)))
      
      
      ;; push : World -> World
      (define (push world)
        (exchange (bring (exchange world))))
      
      
      (define (extend-mark-to-newline world)
        (let ([matched-whitespace
               (regexp-match #rx"^[ \t\n]*\n" 
                             (rope->string
                              (rope-leading-whitespace
                               (subrope (World-rope world)
                                        (World-mark-end-index world)))))])
          (if matched-whitespace
              (mark/pos+len world
                            (World-mark-position world)
                            (+ (World-mark-length world) 
                               (string-length (first matched-whitespace))))
              world)))
      
      
      ;; copy : World -> World
      (define (copy world)
        (when (World-selection world)
          (set-clipboard-content (World-selection world)))
        world)
      
      ;; cut : World -> World
      (define (cut world)
        (delete/selection (dedouble-ellipsis (copy world))))
      
      ;; paste : World -> World
      (define (paste world)
        (if (get-clipboard-content)
            (replace/selection (dedouble-ellipsis world)
                               (rope-append
                                (string->rope " ")
                                (rope-append (get-clipboard-content)
                                             (string->rope " "))))
            world))
      
      
      (define *indentation-overhang* 500)
      
      ;; enter/pos+len : World pos len -> World
      (define (enter/pos+len world pos len)
        (indent/pos+len (insert world pos (string->rope "\n"))
                        (line-pos (World-rope world) pos)
                        (+ len *indentation-overhang*)))
      
      ;; enter/selection : World -> World
      (define (enter/selection world)
        (enter/pos+len world (World-cursor-position world) (World-selection-length world)))
      
      ;; join/pos+len : World pos len -> World
      (define (join/pos+len world pos len)
        (let* ([eol-index (line-end-index (World-rope world) (pos->index pos))]
               [eof-index (rope-length (World-rope world))]
               [line-start (line-pos (World-rope world) pos)])
          (when (= eol-index eof-index)
            (raise (make-voice-exn "No line to join.")))
          (indent/pos+len (delete/pos+len world (index->pos eol-index) 1)
                          line-start
                          (+ len (- pos line-start) *indentation-overhang*))))
      
      ;; join/selection : World -> World
      (define (join/selection world)
        (join/pos+len world (World-cursor-position world) (World-selection-length world)))
      
      ;; anti-join/pos+len : World pos len -> World
      (define (anti-join/pos+len world pos len)
        (let* ([eol-index (sub1 (line-index (World-rope world) (pos->index pos)))]
               [line-start (line-pos (World-rope world) pos)])
          (when (< eol-index 0)
            (raise (make-voice-exn "No line to anti-join.")))
          (indent/pos+len (delete/pos+len world (index->pos eol-index) 1)
                          line-start
                          (+ len (- pos line-start)))))
      
      ;; anti-join/cursor : World -> World
      (define (anti-join/cursor world)
        (anti-join/pos+len world (World-cursor-position world) 0))
      
      ;; transpose : World -> World
      (define (transpose world)
        (let ([text (make-object (text-rope-mixin scheme:text%))])
          (send text set-rope (World-rope world))
          (send text diva:set-selection-position
                (sub1 (World-cursor-position world)))
          
          (send text transpose-sexp (sub1 (World-cursor-position world)))
          (copy-struct World (unmark world)
                       [World-cursor-position (add1 (send text get-start-position))]
                       [World-selection-length 0]
                       [World-rope (send text get-rope)])))
      
      ;; magic-options: world number symbol -> (listof string)
      (define (magic-options world pos symbol)
        (let* ([symbols/stx  (find-all-magic symbol/stx? pos (World-syntax-list world))]
               [strs      (map (lambda (stx) (symbol->string (syntax-e stx))) symbols/stx)]
               [strs      (filter (prefix/string? (symbol->string symbol)) strs)]
               [tag-strs  (map tag-name (tag-library-lookup (get-current-tag-library) (symbol->string symbol)))]
               [strs      (append strs tag-strs)]
               [strs      (filter (lambda (str) (< (string-length (symbol->string symbol))
                                                   (string-length str)))
                                  strs)]
               [strs      (cons (symbol->string symbol) strs)])
              (filter-double strs)))
        
      ;; magic/expand : World pos symbol non-negative-integer boolean -> symbol
      (define (magic/expand world pos symbol magic-number wrap?)
        (let* ([symbols      (map string->symbol (magic-options world pos symbol))]
               [magic-number (if wrap? (modulo magic-number (length symbols)) magic-number)])
          (list-ref/safe symbols magic-number)))

      ;; magic/completion : World symbol -> symbol
      (define (magic/completion world pos symbol)
        (let* ([symbols (rest (map string->symbol (magic-options world pos symbol)))])
          (if (empty? symbols)
              symbol
              (string->symbol (list->string (list-gcd (map string->list (map symbol->string symbols))))))))


      ;; magic-bash : World symbol -> World
      (define (magic-bash world symbol)
        (let ([a-rope (string->rope (symbol->string (magic/completion world symbol)))])
          (cursor-at-selection-end (replace/selection world a-rope))))))

  
  ;; cleanup-whitespace : index rope (index list) -> str and (index list)
  (define (cleanup-whitespace index a-rope markers)
    (define (whitespace? achar)
      (and (char? achar)
           (char-whitespace? achar)))
    
    (define (skip-to index chars markers fn)
      (if (empty? (rest chars))
          (values empty (decrease> index markers))
          (fn index (rest chars) (decrease> index markers))))
    
    (define (output-to index chars markers fn)
      (if (empty? (rest chars))
          (values chars markers)
          (let-values ([(c m) (fn (add1 index) (rest chars) markers)])
            (values (cons (first chars) c) m))))
    
    (define (decrease> index markers)
      (map (lambda (m) (if (> m index) (sub1 m) m)) markers))
    
    (define (next-state chars)
      (cond [(whitespace? (first chars))
             eat-whitespace]
            [(open-paren? (first chars)) eat-whitespace]
            [(pipe? (first chars)) pipe]
            [(double-quote? (first chars)) double-quote]
            [else textchar]))
    
    (define (eat-whitespace index chars markers)
      (cond [(whitespace? (first chars))
             (skip-to index chars markers eat-whitespace)]
            [else (output-to index chars markers (next-state chars))]))
    
    (define (pipe index chars markers)
      (cond [(pipe? (first chars)) (output-to index chars markers textchar)]
            [else (output-to index chars markers pipe)]))
    
    (define (double-quote index chars markers)
      (cond [(double-quote? (first chars)) (output-to index chars markers textchar)]
            [else (output-to index chars markers double-quote)]))
    
    (define (textchar index chars markers)
      (cond [(not (whitespace? (first chars)))
             (output-to index chars markers (next-state chars))]
            [(empty? (rest chars))
             (skip-to index chars markers (next-state chars))]
            [(whitespace? (second chars))
             (skip-to index chars markers textchar)]
            [(close-paren? (second chars))
             (skip-to index chars markers textchar)]
            [else (output-to index chars markers eat-whitespace)]))
    
    
    (if (= (rope-length a-rope) 0)
        (values a-rope markers)
        (let-values ([(c m) (eat-whitespace index (vector->list (rope->vector a-rope)) markers)])
          (values (vector->rope (list->vector c)) m)))))