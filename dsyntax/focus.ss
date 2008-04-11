(module focus mzscheme
  (require "struct.ss"
           (lib "etc.ss")
           (lib "list.ss")
           (lib "contract.ss")
           (only "move.ss"
                 get-move-after-dstx
                 after-displayed-string
                 move-compose
                 apply-move))
  
  (define cursor-or-false/c (or/c cursor? false/c))
  
  (define focus-function/c (cursor? . -> . cursor-or-false/c))
  (provide/contract [make-toplevel-cursor ((listof dstx?) . -> . cursor?)]
                    
                    [loc-after (loc? dstx? . -> . loc?)]
                    
                    [focus-in focus-function/c]
                    [focus-in/no-snap focus-function/c]
                    [focus-out focus-function/c]
                    [focus-older focus-function/c]
                    [focus-older/no-snap focus-function/c]
                    [focus-younger focus-function/c]
                    [focus-younger/no-snap focus-function/c]
                    [focus-successor focus-function/c]
                    [focus-predecessor focus-function/c]
                    [focus-toplevel focus-function/c]
                    
                    [focus-pos
                     (cursor? natural-number/c . -> . cursor-or-false/c)])
  
  
  
  
  ;; make-toplevel-cursor: (listof dstx) -> cursor
  ;;
  ;; Creates the initial cursor focused on the first object in the list.
  ;; FIXME: what if there's a space?
  (define (make-toplevel-cursor dstxs)
    (cond
      [(empty? dstxs)
       (make-cursor (new-space "")
                    (make-loc 1 0 0)
                    #f
                    '()
                    '()
                    '())]
      [else
       (make-cursor (first dstxs)
                    (make-loc 1 0 0)
                    #f
                    '()
                    '()
                    (rest dstxs))]))
  
  
  ;; loc-after: loc dstx -> loc
  ;;
  ;; Returns the location immediately after this one, going
  ;; past a-dstx.
  (define (loc-after a-loc a-dstx)
    (apply-move (get-move-after-dstx a-dstx) a-loc))
  
  
  ;; focus-in: cursor -> (union cursor #f)
  ;;
  ;; Enters a fusion and puts focus on the first non-space child.
  ;; Postcondition: the parent of the resulting cursor, if it exists,
  ;; must have been focused on a fusion.
  (define (focus-in a-cursor)
    (local ((define focus (cursor-dstx a-cursor)))
      (cond
        [(atom? focus) #f]
        [(space? focus) #f]
        [(fusion? focus)
         (let loop ([younger-rev '()]
                    [younger-loc-rev '()]
                    [children (fusion-children focus)]
                    [loc (after-displayed-string (cursor-loc a-cursor)
                                                 (fusion-prefix focus))])
           (cond [(empty? children) #f]
                 [(space? (first children))
                  (loop (cons (first children) younger-rev)
                        (cons loc younger-loc-rev)
                        (rest children)
                        (after-displayed-string
                         loc (space-content (first children))))]
                 [else
                  (make-cursor (first children)
                               loc
                               a-cursor
                               younger-rev
                               younger-loc-rev
                               (rest children))]))])))
  
  
  ;; focus-in/no-snap: cursor -> (or/c cursor #f)
  (define (focus-in/no-snap a-cursor)
    (local ((define focus (cursor-dstx a-cursor)))
      (cond
        [(atom? focus) #f]
        [(space? focus) #f]
        [(fusion? focus)
         (let ([younger-rev '()]
               [younger-loc-rev '()]
               [children (fusion-children focus)]
               [loc (after-displayed-string (cursor-loc a-cursor)
                                            (fusion-prefix focus))])
           (cond [(empty? children) #f]
                 [else
                  (make-cursor (first children)
                               loc
                               a-cursor
                               younger-rev
                               younger-loc-rev
                               (rest children))]))])))
  
  
  
  ;; focus-out: cursor -> (union cursor #f)
  ;;
  ;; Moves back up out to the fusion parent.
  ;; Postcondition: if the return is a cursor, then that cursor
  ;; must be focused on a fusion.
  (define (focus-out a-cursor)
    (cond
      [(cursor-parent a-cursor)
       => identity
       ;; fixme!: this is definitely not right if we allow for focused edits... will need
       ;; edits.ss later on to motivate the unit tests and fix.
       ]
      [else #f]))
  
  
  
  ;; focus-older: cursor -> (union cursor #f)
  ;; 
  ;; Moves the focus to the next oldest sibling.
  (define (focus-older a-cursor)
    (local ((define loc
              (loc-after (cursor-loc a-cursor)
                         (cursor-dstx a-cursor)))
            (define youngers-rev
              (cons (cursor-dstx a-cursor)
                    (cursor-youngers-rev a-cursor)))
            (define youngers-loc-rev
              (cons (cursor-loc a-cursor)
                    (cursor-youngers-loc-rev a-cursor)))
            (define olders (cursor-olders a-cursor)))
      (cond
        [(empty? olders) #f]
        [else
         (local ((define new-cursor
                   (make-cursor (first olders)
                                loc
                                (cursor-parent a-cursor)
                                youngers-rev
                                youngers-loc-rev
                                (rest olders))))
           (cond
             [(space? (cursor-dstx new-cursor))
              (focus-older new-cursor)]
             [else
              new-cursor]))])))
  
  
  ;; focus-older/no-snap: cursor -> (union cursor #f)
  ;; Moves the focus to the next older sibling, not snapping
  ;; across whitespace.
  (define (focus-older/no-snap a-cursor)
    (local ((define loc
              (loc-after (cursor-loc a-cursor)
                         (cursor-dstx a-cursor)))
            (define youngers-rev
              (cons (cursor-dstx a-cursor)
                    (cursor-youngers-rev a-cursor)))
            (define youngers-loc-rev
              (cons (cursor-loc a-cursor)
                    (cursor-youngers-loc-rev a-cursor)))
            (define olders (cursor-olders a-cursor)))
      (cond
        [(empty? olders) #f]
        [else
         (make-cursor (first olders)
                      loc
                      (cursor-parent a-cursor)
                      youngers-rev
                      youngers-loc-rev
                      (rest olders))])))
  
  
  
  
  ;; focus-younger: cursor -> (union cursor #f)
  ;;
  ;; Moves the focus to the next youngest sibling.
  (define (focus-younger a-cursor)
    (local ((define youngers-rev (cursor-youngers-rev a-cursor))
            (define youngers-loc-rev (cursor-youngers-loc-rev a-cursor))
            (define olders (cursor-olders a-cursor)))
      (cond
        [(empty? youngers-rev) #f]
        [else
         (local ((define new-cursor
                   (make-cursor (first youngers-rev)
                                (first youngers-loc-rev)
                                (cursor-parent a-cursor)
                                (rest youngers-rev)
                                (rest youngers-loc-rev)
                                (cons (cursor-dstx a-cursor)
                                      olders))))
           (cond
             [(space? (cursor-dstx new-cursor))
              (focus-younger new-cursor)]
             [else
              new-cursor]))])))
  
  
  ;; focus-younger/no-snap: cursor -> (or/c cursor #f]
  ;; Moves the focus to a younger sibling, not snapping across whitespace.
  (define (focus-younger/no-snap a-cursor)
    (local ((define youngers-rev (cursor-youngers-rev a-cursor))
            (define youngers-loc-rev (cursor-youngers-loc-rev a-cursor))
            (define olders (cursor-olders a-cursor)))
      (cond
        [(empty? youngers-rev) #f]
        [else
         (make-cursor (first youngers-rev)
                      (first youngers-loc-rev)
                      (cursor-parent a-cursor)
                      (rest youngers-rev)
                      (rest youngers-loc-rev)
                      (cons (cursor-dstx a-cursor)
                            olders))])))
  
  
  
  ;; focus-successor: cursor -> (union cursor #f)
  (define (focus-successor a-cursor)
    (cond
      [(focus-in a-cursor) => identity]
      [(focus-older a-cursor) => identity]
      [else
       (let loop ([a-cursor a-cursor])
         (cond
           [(focus-out a-cursor)
            =>
            (lambda (a-cursor)
              (cond [(focus-older a-cursor) => identity]
                    [else (loop a-cursor)]))]
           [else #f]))]))
  
  
  ;; focus-predecessor: cursor -> (union cursor #f)
  (define (focus-predecessor a-cursor)
    (cond
      [(focus-younger a-cursor)
       =>
       (lambda (a-cursor)
         (let loop ([a-cursor a-cursor])
           (cond [(focus-in a-cursor)
                  =>
                  (lambda (a-cursor)
                    (let find-last ([a-cursor a-cursor])
                      (cond
                        [(focus-older a-cursor) => find-last]
                        [else (loop a-cursor)])))]
                 [else a-cursor])))]
      [(focus-out a-cursor) => identity]
      [else #f]))
  
  
  ;; focus-toplevel: cursor -> cursor
  ;; Moves the cursor to the first dstx at the toplevel.
  (define (focus-toplevel a-cursor)
    (let ([outermost (maximally-repeat-movement a-cursor focus-out)])
      (maximally-repeat-movement outermost focus-younger)))
  
  
  ;; maximally-repeat-movement: cursor focus-function -> cursor
  ;; Repeatedly applies a-movement on a-cursor until we hit an endpoint.
  (define (maximally-repeat-movement a-cursor a-movement)
    (let loop ([a-cursor a-cursor])
      (cond
        [(a-movement a-cursor)
         =>
         (lambda (cursor-after-movement)
           (loop cursor-after-movement))]
        [else
         a-cursor])))
  
  
  
  (define (focus-pos a-cursor a-pos)
    ;; fixme
    (void)))