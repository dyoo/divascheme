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
                    [consume-msg! (state? msg? . -> . (listof op?))])
  
  
  
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
                (printf "looking at ~s~n" a-dstx)
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
         (let ([an-op
                (integrate! a-state next-executable-msg)])
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
       (integrate-delete! a-state a-msg)]))
  
  
  ;; integrate-insert!: state msg -> op
  (define (integrate-insert! a-state a-msg)
    (match a-msg
      [(struct msg:insert (host-id dstx after-id before-id))
       (let ([a-cursor (focus/woot-id (state-cursor a-state) after-id)])
         (set-state-cursor! a-state (insert-after a-cursor dstx)))
       (make-op:insert-after a-msg dstx after-id)]))
  
  
  
  ;; integrate-delete!: state msg -> op
  (define (integrate-delete! a-state a-msg)
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
  
  
  ;; woot-id->dependency: woot-id -> symbol
  ;; Given a woot id, returns a symbol that can be fed into
  ;; topological-queue.
  (define (woot-id->dependency a-woot-id)
    (string->symbol (string-append (woot-id-logic-id a-woot-id)
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
       (dstx-all-woot-ids dstx)]
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
    (dstx-property-ref a-dstx 'visible (lambda () #t))))