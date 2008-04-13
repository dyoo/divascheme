(module cursor mzscheme
  (require "struct.ss"
           "move.ss"
           "focus.ss"
           (lib "plt-match.ss")
           (lib "contract.ss")
           (lib "list.ss"))
  
  
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
  
  
  ;; cursor-insert-before: cursor dstx -> cursor
  ;; Inserts a dstx before our focus, and refocuses the cursor on the new element.
  (define (cursor-insert-before a-cursor a-dstx)
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
                (cursor-insert-after pred-cursor a-dstx))])]))
  
  
  ;; cursor-insert-after: cursor dstx -> cursor
  ;; Inserts a dstx after our focus, and refocuses the cursor on the new element.
  (define (cursor-insert-after a-cursor a-dstx)
    (match a-cursor
      [(struct cursor (dstx loc parent youngers-rev youngers-loc-rev olders))
       (let ([new-cursor (make-cursor dstx
                                      loc
                                      parent
                                      youngers-rev
                                      youngers-loc-rev
                                      (cons a-dstx olders))])
         (focus-older/no-snap new-cursor))]))
  
  
  (define (cursor-delete a-cursor)
    ;; fixme!
    (void))
  
  
  ;; cursor-dstx-property-set: cursor symbol any -> cursor
  (define (cursor-dstx-property-set a-cursor a-symbol a-value)
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
  
  
  (provide/contract [cursor-line (cursor? . -> . natural-number/c)]
                    [cursor-col (cursor? . -> . natural-number/c)]
                    [cursor-pos (cursor? . -> . natural-number/c)]
                    [cursor-endloc (cursor? . -> . loc?)]
                    [cursor-endpos (cursor? . -> . natural-number/c)]
                    [cursor-insert-before (cursor? dstx? . -> . cursor?)]
                    [cursor-insert-after (cursor? dstx? . -> . cursor?)]
                    [cursor-delete (cursor? . -> . cursor?)]
                    [cursor-dstx-property-set (cursor? symbol? any/c . -> . cursor?)]))