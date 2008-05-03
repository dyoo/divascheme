(module mock-woot mzscheme
  (require (lib "contract.ss")
           (lib "list.ss")
           (lib "plt-match.ss")
           "woot-struct.ss"
           "../dsyntax/dsyntax.ss"
           "utilities.ss")
  
  ;; This is a mock-up of what interface we need from the woot algorithm.
  ;; Unlike the real woot, we expect this to break when there are concurrent
  ;; edits!
  
  
  ;; unexecuted is a list of the unexecuted msg structures.
  ;; cursor is a functional cursor maintaining the structures that we
  ;; know about.
  ;; State should be an opaque structure.
  (define-struct state (unexecuted cursor))
  
  
  
  (provide/contract [new-mock-woot ((listof dstx?) . -> . state?)]
                    [visible-before-or-at (state? woot-id? . -> . woot-id?)]
                    [consume-msg! (state? msg? . -> . (listof op?))])
  
  
  
  ;; Creates a new mock-woot interface.
  ;; Fixme: we need to initialize with the woot ids of the boundaries.
  (define (new-mock-woot initial-dstxs)
    (make-state
     '()
     (make-toplevel-cursor initial-dstxs)))
  
  
  ;; consume-msg: a-state -> (listof Protocol-Syntax-Tree)
  ;; Add a new message to the state, and return a list of Interpreter
  ;; commands that we can evaluate.
  (define (consume-msg! a-state a-msg)
    (add-to-unexecuted! a-state a-msg)
    (integrate-all-executables! a-state))
  
  
  ;; add-to-unexecuted!: mock-woot msg -> void
  ;; Adds to our pending queue of unexecuted messages.
  (define (add-to-unexecuted! a-state a-msg)
    (set-state-unexecuted! a-state
                           (cons a-msg (state-unexecuted a-state))))
  
  
  ;; integrate-all-executables!: state -> (listof Protocol-Syntax-Tree)
  ;; Look for all executable messages, integrate them, and get back
  ;; a list of the commands to evaluate, in topological order.
  (define (integrate-all-executables! a-state)
    (let loop ()
      (let ([executables (filter (lambda (a-msg)
                                   (is-executable? a-state a-msg))
                                 (state-unexecuted a-state))])
        (cond
          [(empty? executables)
           '()]
          [else
           (remove-from-unexecuted! a-state executables)
           (append (map (lambda (a-msg)
                          (integrate! a-state a-msg))
                        executables)
                   (loop))]))))
  
  
  ;; remove-from-unexecuted!: mock-woot (listof msg) -> void
  (define (remove-from-unexecuted! a-state msgs-to-remove)
    (set-state-unexecuted! a-state
                           (foldl (lambda (a-msg msgs) (remove a-msg msgs))
                                  (state-unexecuted a-state)
                                  msgs-to-remove)))
  
  
  
  ;; integrate!: mock-woot msg -> op
  ;; Needs to return any new operations that we've been able to successfully
  ;; integrate.
  (define (integrate! a-state a-msg)
    (match a-msg
      [(struct msg:insert (host-id dstx after-id before-id))
       (integrate-insert! a-state a-msg)]
      [(struct msg:delete (host-id id))
       (integrate-delete! a-state a-msg)]))
  
  
  ;; integrate-insert!: state msg -> op
  (define (integrate-insert! a-state a-msg)
    ;; fixme: adjust cursor
    ;; fixme: find whatever dstx is visible, and insert after that thing.
    (printf "integrating ~s~n" a-msg)
    (match a-msg
      [(struct msg:insert (host-id dstx after-id before-id))
       (let ([a-cursor (focus/woot-id (state-cursor a-state) after-id)])
         (set-state-cursor! a-state (insert-after a-cursor dstx)))
       (make-op:insert-after a-msg dstx after-id)]))
  
  
  
  ;; integrate-delete!: state msg -> op
  (define (integrate-delete! a-state a-msg)
    ;; fixme: adjust cursor
    ;; fixme: find whatever dstx is visible, and insert after that thing.
    (match a-msg
      [(struct msg:delete (host-id id))
       (let ([a-cursor (focus/woot-id (state-cursor a-state) id)])
         (set-state-cursor!
          a-state
          (replace a-cursor (dstx-set-invisible (cursor-dstx a-cursor)))))
       (make-op:delete a-msg id)]))
  
  
  ;; visible-before-or-at: state woot-id -> (or/c woot-id #f)
  ;; Given the woot-id of a dstx, returns the woot-id of a dstx that is visible
  ;; at or before the given dstx.
  (define (visible-before-or-at a-state a-woot-id)
    (let loop ([a-cursor (focus/woot-id (state-cursor a-state) a-woot-id)])
      (cond
        [(dstx-visible? (cursor-dstx a-cursor))
         (dstx-woot-id (cursor-dstx a-cursor))]
        [(focus-younger/no-snap a-cursor)
         => loop]
        [else
         #f])))
  
  
  
  ;; is-executable?: mock-woot msg -> boolean
  ;; Returns true if we can execute and integrate this message.
  (define (is-executable? a-state a-msg)
    (let ([result
           (match a-msg
             [(struct msg:insert (host-id dstx after-woot-id before-woot-id))
              (cond
                [(and after-woot-id before-woot-id)
                 ;; Inserting between two dstxs
                 (and (focus/woot-id (state-cursor a-state) after-woot-id)
                      (focus/woot-id (state-cursor a-state) before-woot-id)
                      #t)]
                [else
                 ;; Inserting at end of fusion's children
                 (and (focus/woot-id (state-cursor a-state) after-woot-id)
                      #t)])]
             [(struct msg:delete (host-id woot-id))
              (and (focus/woot-id (state-cursor a-state) woot-id)
                   #t)])])
      (printf "is-executable? ~s ~s~n" result a-msg)))
  
  
  ;; focus/woot-id: cursor woot-id -> (or/c cursor false/c)
  ;; Refocuses the cursor on the dstx with the given woot id, or returns false.
  (define (focus/woot-id a-cursor a-woot-id)
    (focus-find/dstx a-cursor
                     (lambda (a-dstx)
                       (equal? a-woot-id (dstx-woot-id a-dstx)))))
  
  
  ;; dstx-set-invisible: dstx -> dstx
  ;; Turn off the visible property of the dstx.
  (define (dstx-set-invisible a-dstx)
    (dstx-property-set a-dstx 'visible #f))
  
  
  ;; dstx-visible?: dstx -> boolean
  ;; Return the visible property of the dstx.  Default is visible.
  (define (dstx-visible? a-dstx)
    (dstx-property-ref a-dstx 'visible (lambda () #t))))