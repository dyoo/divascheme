(module mock-woot mzscheme
  (require (lib "contract.ss")
           (lib "list.ss")
           (lib "plt-match.ss")
           "../topological-queue/topological-queue.ss"
           "../dsyntax/dsyntax.ss"
           "woot-struct.ss"
           "utilities.ss")
  
  ;; This is a mock-up of what interface we need from the woot algorithm.
  ;; Unlike the real woot, we expect this to break when there are concurrent
  ;; edits!  Fixme!
  
  
  ;; State should be an opaque structure.
  ;;
  ;; cursor is a functional cursor maintaining the structures that we
  ;; know about.
  ;; tqueue represents the topological queue for messages.
  (define-struct state (cursor tqueue))
  
  
  (provide/contract [new-mock-woot (cursor? . -> . state?)]
                    [visible-before-or-at (state? woot-id? . -> . woot-id?)]
                    [consume-msg! (state? msg? . -> . (listof op?))]
                    
                    ;; for debugging
                    [struct state ([cursor cursor?]
                                   [tqueue tqueue?])]
                    )
  
  
  
  ;; Creates a new mock-woot interface.
  ;; Fixme: we need to initialize with the woot ids of the boundaries.
  (define (new-mock-woot initial-cursor)
    (let ([a-state (make-state initial-cursor (new-tqueue))])
      (initially-satisfy-from-toplevel-dstxs a-state)
      a-state))
  
  
  
  ;; initially-satisfy-toplevel-dstxs: state -> void
  ;; Tells the topological sort which elements we already know about.
  (define (initially-satisfy-from-toplevel-dstxs a-state)
    (for-each (lambda (a-dstx)
                (for-each (lambda (a-woot-id)
                            (tqueue-satisfy! (state-tqueue a-state)
                                             (woot-id->dependency a-woot-id)))
                          (dstx-all-woot-ids a-dstx)))
              (cursor-toplevel-dstxs (state-cursor a-state))))
  
  
  ;; consume-msg: a-state -> (listof op)
  ;; Add a new message to the state, and return a list of Interpreter
  ;; commands that we can evaluate.
  (define (consume-msg! a-state a-msg)
    (add-to-unexecuted! a-state a-msg)
    (integrate-all-executables! a-state))
  
  
  ;; add-to-unexecuted!: mock-woot msg -> void
  ;; Adds to our pending queue of unexecuted messages.
  (define (add-to-unexecuted! a-state a-msg)
    (tqueue-add! (state-tqueue a-state) a-msg (msg-precondition-dependencies a-msg)))
  
  
  ;; integrate-all-executables!: state -> (listof op)
  ;; Look for all executable messages, integrate them, and get back
  ;; a list of the commands to evaluate, in topological order.
  (define (integrate-all-executables! a-state)
    (let ([next-executable-msg (tqueue-try-get (state-tqueue a-state))])
      (cond
        [(not next-executable-msg)
         '()]
        [else
         (let ([an-op (integrate! a-state next-executable-msg)])
           (for-each (lambda (a-dep)
                       (tqueue-satisfy! (state-tqueue a-state) a-dep))
                     (msg-integration-satisfies next-executable-msg))
           (cons an-op (integrate-all-executables! a-state)))])))
  
  
  
  ;; integrate!: mock-woot msg -> op
  ;; Needs to return any new operations that we've been able to successfully
  ;; integrate.
  (define (integrate! a-state a-msg)
    (match a-msg
      [(struct msg:insert (host-id dstx after-id before-id))
       (integrate-insert! a-state a-msg)]
      [(struct msg:delete (host-id id))
       (integrate-delete! a-state a-msg)]
      [(struct msg:move (host-id from-id after-id before-id new-id))
       (integrate-move! a-state a-msg)]))
  
  
  ;; integrate-insert!: state msg -> op
  (define (integrate-insert! a-state a-msg)
    (match a-msg
      [(struct msg:insert (host dstx after-id before-id))
       (let* ([a-cursor-before (focus/woot-id (state-cursor a-state) after-id)]
              [a-cursor-just-before (focus-search a-cursor-before
                                                  focus-older/no-snap
                                                  (λ (el)
                                                    (or (not (focus-older/no-snap el))
                                                        (equal?
                                                         before-id
                                                         (cursor-woot-id (focus-older/no-snap el)))
                                                        (woot-id-> (cursor-woot-id (focus-older/no-snap el))
                                                                   (dstx-woot-id dstx)))))])
         (set-state-cursor! a-state (insert-after a-cursor-just-before dstx))
         (cond
           [(cursor-visible-at-and-above? (state-cursor a-state))
            (make-op:insert-after a-msg dstx (cursor-woot-id a-cursor-just-before))]
           [else
            (make-op:no-op a-msg)]))]))
  
  
  
  ;; integrate-delete!: state msg -> op
  (define (integrate-delete! a-state a-msg)
    (match a-msg
      [(struct msg:delete (host-id id))
       (let* ([a-cursor (focus/woot-id (state-cursor a-state) id)]
              [chased-cursor (cursor-chase a-cursor)])
         (cond
           [(cursor-visible-at-and-above? chased-cursor)
            (set-state-cursor!
             a-state
             (replace chased-cursor (dstx-set-invisible (cursor-dstx chased-cursor))))
            (make-op:delete a-msg id)]
           [else
            (set-state-cursor! a-state
                               (replace chased-cursor (dstx-set-invisible (cursor-dstx chased-cursor))))
            (make-op:no-op a-msg)]))]))
  
  ;; integrate-move!: state msg -> state
  (define (integrate-move! a-state a-msg)
    (match a-msg
      [(struct msg:move (host from-id after-id before-id new-id))
       (let* ([from-cursor (focus/woot-id (state-cursor a-state) from-id)]
              [moved-dstx (cursor-dstx from-cursor)])
         (set-state-cursor! a-state
                            (replace from-cursor (make-tomb-m new-id)))
         (integrate-insert! a-state (make-msg:insert (dstx-set-woot-id moved-dstx new-id)
                                                     after-id
                                                     before-id)))]))
  
  
  
  
  ;; visible-before-or-at: state woot-id -> (or/c woot-id #f)
  ;; Given the woot-id of a dstx, returns the woot-id of a dstx that is visible
  ;; at or before the given dstx.
  (define (visible-before-or-at a-state a-woot-id)
    (let loop ([a-cursor (focus/woot-id (state-cursor a-state) a-woot-id)])
      (cond
        [(dstx-visible? (cursor-dstx a-cursor))
         (cursor-woot-id a-cursor)]
        [(focus-younger/no-snap a-cursor)
         => loop]
        [else
         #f])))
  
  
  
  ;; cursor-visible-at-and-above?: state woot-id -> boolean
  ;; Returns true if the dstx given by the woot id is visible because all
  ;; of its enclosing expressions are visible.
  (define (cursor-visible-at-and-above? a-cursor)
    (let loop ([a-cursor a-cursor])
      (cond
        [(dstx-visible? (cursor-dstx a-cursor))
         (cond
           [(focus-out a-cursor)
            =>
            loop]
           [else
            #t])]
        [else
         #f])))
  
  
  ;; woot-id->dependency: woot-id -> symbol
  ;; Given a woot id, returns a symbol that can be fed into
  ;; topological-queue.
  (define (woot-id->dependency a-woot-id)
    (string->symbol (string-append (number->string (woot-id-logic-id a-woot-id))
                                   "::"
                                   (woot-id-host-id a-woot-id))))
  
  
  ;; msg-precondition-dependencies: msg -> (listof symbol)
  ;; Returns the preconditions necessary to integrate the message.
  ;; Insertion requires that the after-woot-id is known, and
  ;; optionally the before-woot-id.
  ;; Deletion requires the woot-id to be executed.
  (define (msg-precondition-dependencies a-msg)
    (match a-msg
      [(struct msg:insert (host-id dstx after-woot-id before-woot-id))
       (cond
         [(and after-woot-id before-woot-id)
          (list (woot-id->dependency after-woot-id)
                (woot-id->dependency before-woot-id))]
         [else
          (list (woot-id->dependency after-woot-id))])]
      [(struct msg:delete (host-id woot-id))
       (list (woot-id->dependency woot-id))]))
  
  
  ;; msg-integration-satisfies: msg -> (listof symbol)
  ;; Once this message is integrated, returns a list of the dependencies
  ;; that this message has satisfied.
  (define (msg-integration-satisfies a-msg)
    (match a-msg
      [(struct msg:insert (host-id dstx after-woot-id before-woot-id))
       (map woot-id->dependency (dstx-all-woot-ids dstx))]
      [(struct msg:delete (host-id woot-id))
       (list)]))
  
  
  
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
    (dstx-property-ref a-dstx 'visible (lambda () #t)))
  
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
  
  ;; cursor-chase: cursor -> cursor
  ;; follows tomb-m references from a dstx
  (define (cursor-chase a-cursor)
    (let* ((a-dstx (cursor-dstx a-cursor))
           (a-tomb (dstx-tomb a-dstx)))
      (if (tomb:m? a-tomb)
          (cursor-chase (focus/woot-id a-cursor
                                       (tomb:m-id a-dstx)))
          a-cursor)))
  
  )
