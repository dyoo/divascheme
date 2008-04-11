(module edit mzscheme
  (require (lib "contract.ss")
           (lib "plt-match.ss")
           (lib "list.ss")
           "focus.ss"
           "struct.ss")
  
  
  
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
       (let ([new-cursor
              (make-cursor
               dstx loc parent youngers-rev youngers-loc-rev (cons a-dstx olders))])
         (focus-older/no-snap new-cursor))]))
  
  
  (define (cursor-delete a-cursor)
    ;; fixme!
    (void))
  
  
  (define (cursor-set-property a-cursor a-symbol a-value)
    ;; fixme!
    (void))
  
  
  (provide/contract
   [cursor-insert-before (cursor? dstx? . -> . cursor?)]
   [cursor-insert-after (cursor? dstx? . -> . cursor?)]
   [cursor-delete (cursor? . -> . cursor?)]
   [cursor-set-property (cursor? symbol? any/c . -> . cursor?)]))