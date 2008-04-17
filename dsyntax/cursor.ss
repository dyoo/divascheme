(module cursor mzscheme
  (require "struct.ss"
           "move.ss"
           (lib "plt-match.ss")
           (lib "contract.ss")
           (lib "list.ss")
           (lib "etc.ss")
           (only "move.ss"
                 get-move-after-dstx
                 after-displayed-string
                 apply-move))
  
  
  (define cursor-or-false/c (or/c cursor? false/c))
  (define focus-function/c (cursor? . -> . cursor-or-false/c))
  
  (provide/contract
   [make-toplevel-cursor ((listof dstx?) . -> . cursor?)]
   
   [cursor-line (cursor? . -> . natural-number/c)]
   [cursor-col (cursor? . -> . natural-number/c)]
   [cursor-pos (cursor? . -> . natural-number/c)]
   [cursor-endloc (cursor? . -> . loc?)]
   [cursor-endpos (cursor? . -> . natural-number/c)]
   [cursor-toplevel-dstxs (cursor? . -> . (listof dstx?))]
   
   [insert-before (cursor? dstx? . -> . cursor?)]
   [insert-after (cursor? dstx? . -> . cursor?)]
   [delete (cursor? . -> . cursor?)]
   [replace (cursor? dstx? . -> . cursor?)]
   [property-set
    (cursor? symbol? any/c . -> . cursor?)]
   [property-ref
    (cursor? symbol? . -> . any)]
   
   
   [loc-after (loc? dstx? . -> . loc?)]
   
   [focus-in focus-function/c]
   [focus-in/no-snap focus-function/c]
   [focus-out focus-function/c]
   [focus-older focus-function/c]
   [focus-older/no-snap focus-function/c]
   [focus-younger focus-function/c]
   [focus-younger/no-snap focus-function/c]
   [focus-successor focus-function/c]
   [focus-successor/no-snap focus-function/c]
   [focus-predecessor focus-function/c]
   [focus-predecessor/no-snap focus-function/c]
   [focus-toplevel focus-function/c]
   [focus-youngest focus-function/c]
   [focus-oldest focus-function/c]
   
   
   [focus-find-dstx
    (cursor? (dstx? . -> . boolean?) . -> . cursor-or-false/c)]
   [focus-pos
    (cursor? natural-number/c . -> . cursor-or-false/c)]
   [focus-endpos
    (cursor? natural-number/c . -> . cursor-or-false/c)]
   [focus-container
    (cursor? natural-number/c . -> . cursor-or-false/c)])
  
  
  
  
  ;; cursor-line: cursor -> natural-number
  ;;
  ;; Returns the line where the cursor is focused.
  (define (cursor-line a-cursor)
    (loc-line (cursor-loc a-cursor)))
  
  
  ;; cursor-col: cursor -> natural-number
  ;;
  ;; Returns the column of the leftmost position where
  ;; the cursor is focused.
  (define (cursor-col a-cursor)
    (loc-col (cursor-loc a-cursor)))
  
  
  (define (cursor-pos a-cursor)
    (loc-pos (cursor-loc a-cursor)))
  
  (define (cursor-endloc a-cursor)
    (apply-move (get-move-after-dstx (cursor-dstx a-cursor))
                (cursor-loc a-cursor)))
  
  (define (cursor-endpos a-cursor)
    (loc-pos (cursor-endloc a-cursor)))
  
  
  ;; cursor-toplevel-dstxs: cursor -> (listof dstx)
  ;; Returns the toplevel dstx elements.
  (define (cursor-toplevel-dstxs a-cursor)
    (let ([top-cursor (focus-toplevel a-cursor)])
      (cons (cursor-dstx top-cursor)
            (cursor-olders top-cursor))))
  
  
  ;; cursor-insert-before: cursor dstx -> cursor
  ;; Inserts a dstx before our focus, and refocuses the cursor on the new element.
  (define (insert-before a-cursor a-dstx)
    (match a-cursor
      [(struct cursor (dstx loc parent youngers-rev youngers-loc-rev olders))
       (cond [(empty? youngers-rev)
              (let ([parent (focus-out a-cursor)])
                (match parent
                  [(struct cursor ((struct fusion (parent-props parent-prefix parent-children parent-suffix))
                                   parent-loc
                                   parent-parent
                                   parent-youngers-rev
                                   parent-youngers-loc-rev
                                   parent-olders))
                   (let ([new-cursor (make-cursor (make-fusion parent-props parent-prefix (cons a-dstx parent-children) parent-suffix)
                                                  parent-loc
                                                  parent-parent
                                                  parent-youngers-rev
                                                  parent-youngers-loc-rev
                                                  parent-olders)])
                     (focus-in/no-snap new-cursor))]
                  [else
                   (make-toplevel-cursor (cons a-dstx (cons dstx olders)))]))]
             [else
              (let ([pred-cursor (focus-younger/no-snap a-cursor)])
                (insert-after pred-cursor a-dstx))])]))
  
  
  ;; cursor-insert-after: cursor dstx -> cursor
  ;; Inserts a dstx after our focus, and refocuses the cursor on the new element.
  (define (insert-after a-cursor a-dstx)
    (match a-cursor
      [(struct cursor (dstx loc parent youngers-rev youngers-loc-rev olders))
       (let ([new-cursor (make-cursor dstx
                                      loc
                                      parent
                                      youngers-rev
                                      youngers-loc-rev
                                      (cons a-dstx olders))])
         (focus-older/no-snap new-cursor))]))
  
  
  ;; cursor-delete: cursor -> cursor
  ;; Delete the currently focused dstx.  Focus moves to the next older dstx.  If no such
  ;; dstx exists, then focus moves to the immediate younger dstx.  Again, if that doesn't exist,
  ;; then focus moves to an automatically generated empty space.
  (define (delete a-cursor)
    (match a-cursor
      [(struct cursor (dstx loc parent youngers-rev youngers-loc-rev olders))
       (cond [(empty? olders)
              (cond [(empty? youngers-rev)
                     (make-cursor (new-space "") loc parent youngers-rev youngers-loc-rev olders)]
                    [else
                     (make-cursor (first youngers-rev)
                                  (first youngers-loc-rev)
                                  parent
                                  (rest youngers-rev)
                                  (rest youngers-loc-rev)
                                  olders)])]
             [else
              (make-cursor (first olders)
                           loc
                           parent
                           youngers-rev
                           youngers-loc-rev
                           (rest olders))])]))
  
  
  ;; cursor-replace: cursor dstx -> cursor
  ;; Replace the currently focused dstx with the new dstx
  (define (replace a-cursor a-new-dstx)
    (match a-cursor
      [(struct cursor (dstx loc parent youngers-rev youngers-loc-rev olders))
       (make-cursor a-new-dstx loc parent youngers-rev youngers-loc-rev olders)]))
  
  
  ;; cursor-dstx-property-set: cursor symbol any -> cursor
  (define (property-set a-cursor a-symbol a-value)
    (match a-cursor
      [(struct cursor (dstx loc parent youngers-rev youngers-loc-rev olders))
       (let ([new-cursor
              (make-cursor (dstx-property-set dstx a-symbol a-value)
                           loc
                           parent
                           youngers-rev
                           youngers-loc-rev
                           olders)])
         new-cursor)]))
  
  ;; cursor-dstx-property-ref: cursor symbol -> any
  ;; Returns the property value of the currently focused dstx.
  (define (property-ref a-cursor a-symbol)
    (dstx-property-ref (cursor-dstx a-cursor) a-symbol))
  
  
  
  ;; make-toplevel-cursor: (listof dstx) -> cursor
  ;;
  ;; Creates the initial cursor focused on the first object in the list.
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
    (local ((define focused-dstx (cursor-dstx a-cursor)))
      (cond
        [(atom? focused-dstx) #f]
        [(special-atom? focused-dstx) #f]
        [(space? focused-dstx) #f]
        [(fusion? focused-dstx)
         (let loop ([younger-rev '()]
                    [younger-loc-rev '()]
                    [children (fusion-children focused-dstx)]
                    [loc (after-displayed-string (cursor-loc a-cursor)
                                                 (fusion-prefix focused-dstx))])
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
    (local ((define focused-dstx (cursor-dstx a-cursor)))
      (cond
        [(atom? focused-dstx) #f]
        [(special-atom? focused-dstx) #f]
        [(space? focused-dstx) #f]
        [(fusion? focused-dstx)
         (let ([younger-rev '()]
               [younger-loc-rev '()]
               [children (fusion-children focused-dstx)]
               [loc (after-displayed-string (cursor-loc a-cursor)
                                            (fusion-prefix focused-dstx))])
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
    (let ([parent-cursor (cursor-parent a-cursor)])
      (cond
        [parent-cursor
         (match parent-cursor
           [(struct cursor ((struct fusion (props opener old-children closer))
                            loc parent youngers-rev youngers-loc-rev olders))
            (let ([new-children
                   (append/rev (cursor-youngers-rev a-cursor)
                               (cons (cursor-dstx a-cursor)
                                 (cursor-olders a-cursor)))])
              (cond
                ;; If there has been no change in structure, leave things be.
                [(and (= (length new-children)
                         (length old-children))
                      (andmap eq? new-children old-children))
                 parent-cursor]
                [else
                 ;; Otherwise, reconstruct a new parent cursor.
                 (make-cursor (make-fusion props opener new-children closer)
                              loc parent youngers-rev youngers-loc-rev olders)])
              )])]
        [else #f])))
  
  
  ;; append/rev: (listof X) (listof X) -> (listof X)
  ;; Appends the elements of lst/rev in reverse order onto the tail.
  (define (append/rev lst/rev tail)
    (cond
      [(empty? lst/rev)
       tail]
      [else
       (append/rev (rest lst/rev)
                   (cons (first lst/rev) tail))]))
  
  
  
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
  
  ;; focus-successor/no-snap: cursor -> (union cursor #f)
  (define (focus-successor/no-snap a-cursor)
    (cond
      [(focus-in/no-snap a-cursor) => identity]
      [(focus-older/no-snap a-cursor) => identity]
      [else
       (let loop ([a-cursor a-cursor])
         (cond
           [(focus-out a-cursor)
            =>
            (lambda (a-cursor)
              (cond [(focus-older/no-snap a-cursor) => identity]
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
  
  
  ;; focus-predecessor/no-snap: cursor -> (union cursor #f)
  (define (focus-predecessor/no-snap a-cursor)
    (cond
      [(focus-younger/no-snap a-cursor)
       =>
       (lambda (a-cursor)
         (let loop ([a-cursor a-cursor])
           (cond [(focus-in/no-snap a-cursor)
                  =>
                  (lambda (a-cursor)
                    (let find-last ([a-cursor a-cursor])
                      (cond
                        [(focus-older/no-snap a-cursor) => find-last]
                        [else (loop a-cursor)])))]
                 [else a-cursor])))]
      [(focus-out a-cursor) => identity]
      [else #f]))
  
  
  ;; focus-toplevel: cursor -> cursor
  ;; Moves the cursor to the first dstx at the toplevel.
  (define (focus-toplevel a-cursor)
    (let ([outermost (maximally-repeat-movement a-cursor focus-out)])
      (maximally-repeat-movement outermost focus-younger/no-snap)))
  
  
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
  
  
  ;; focus-oldest: cursor -> cursor
  (define (focus-oldest a-cursor)
    (maximally-repeat-movement a-cursor focus-older/no-snap))
  
  ;; focus-oldest: cursor -> cursor
  (define (focus-youngest a-cursor)
    (maximally-repeat-movement a-cursor focus-younger/no-snap))
  
  
  ;; focus-find-dstx: cursor (dstx -> boolean?) -> (or/c cursor #f)
  ;; Refocus the cursor, based on a predicate that distinguishing between
  ;; dstxs.
  (define (focus-find-dstx a-cursor a-pred)
    (focus-search (focus-toplevel a-cursor)
                  focus-successor/no-snap
                  (lambda (a-cursor)
                    (a-pred (cursor-dstx a-cursor)))))
  
  
  ;; focus-search: cursor focus-function (cursor -> boolean) -> (or/c cursor #f)
  ;; Move across a cursor until the predicate is true.  If we can't find,
  ;; return #f.  Otherwise, return the cursor.
  (define (focus-search a-cursor a-movement a-pred)
    (cond
      [(a-pred a-cursor)
       a-cursor]
      [else
       (let ([new-cursor (a-movement a-cursor)])
         (cond
           [new-cursor
            (focus-search new-cursor a-movement a-pred)]
           [else #f]))]))
  
  
  ;; focus-pos: cursor number -> (or/c cursor #f)
  ;; Given a cursor and a position, refocuses the cursor at the dstx
  ;; at or immediately to the left of the cursor.  If no such syntax
  ;; exists, returns #f.
  ;; Does not snap across whitespace, except for the special case
  ;; of the sentinel empty-space character.
  (define (focus-pos a-cursor a-pos)
    ;; First scan forward, and then scan backward.
    (let ([cursor-forward
           (or (focus-search a-cursor
                             focus-successor/no-snap
                             (lambda (a-cursor)
                               (or (at-or-after? a-cursor a-pos)
                                   (at-end? a-cursor))))
               a-cursor)])
      (let ([new-cursor (focus-search cursor-forward
                                      focus-predecessor/no-snap
                                      (lambda (a-cursor)
                                        (at-or-before? a-cursor a-pos)))])
        (cond [(and new-cursor
                    (sentinel-space? (cursor-dstx new-cursor))
                    (focus-older/no-snap new-cursor))
               => identity]
              [else new-cursor]))))
  
  
  ;; focus-container: cursor number -> (or/c cursor #f)
  ;; Similar to focus-pos.  We look for the smallest dstx that contains
  ;; the given position.
  (define (focus-container a-cursor a-pos)
    (define (after? a-cursor a-pos)
      (> (cursor-pos a-cursor) a-pos))
    
    (define (between? a-cursor a-pos)
      (and (<= (cursor-pos a-cursor) a-pos)
           (< a-pos (cursor-endpos a-cursor))))
    
    ;; First scan forward, and then scan backward.
    (let ([cursor-forward
           (or (focus-search a-cursor
                             focus-successor/no-snap
                             (lambda (a-cursor)
                               (or (after? a-cursor a-pos)
                                   (at-end? a-cursor))))
               a-cursor)])
      (focus-search cursor-forward
                    focus-predecessor/no-snap
                    (lambda (a-cursor)
                      (between? a-cursor a-pos)))))
  
  
  
  ;; focus-endpos: cursor number -> (or/c cursor #f)
  (define (focus-endpos a-cursor a-pos)
    ;; todo: first, focus outward, then focus on successors?
    (focus-search (focus-toplevel a-cursor)
                  focus-successor/no-snap
                  (lambda (a-cursor)
                    (= (cursor-endpos a-cursor) a-pos))))
  
  
  ;; sentinel-space?: dstx -> boolean
  ;; Returns true if we're on a sentinel space.
  (define (sentinel-space? a-dstx)
    (and (space? a-dstx)
         (string=? "" (space-content a-dstx))))
  
  
  ;; at-or-before?: cursor pos -> boolean
  ;; Returns true if the cursor is positioned at or before a-pos.
  (define (at-or-before? a-cursor a-pos)
    (<= (cursor-pos a-cursor) a-pos))
  
  
  ;; at-or-after?: cursor pos -> boolean
  ;; Returns true if the cursor is positioned at or after a-pos.
  (define (at-or-after? a-cursor a-pos)
    (>= (cursor-pos a-cursor) a-pos))
  
  
  
  ;; at-end?: cursor -> boolean
  ;; Returns true if we're at the end, when there is no sucessor.
  (define (at-end? a-cursor)
    (eqv? (focus-successor/no-snap a-cursor) #f)))