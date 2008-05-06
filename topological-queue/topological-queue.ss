(module topological-queue mzscheme
  ;; Provides a queue for elements that have dependencies.  An element can be pushed
  ;; onto the queue along with its dependencies.  We can also tell the queue
  ;; when some dependency has been satisfied. Finally, we can get elements from the
  ;; queue whose dependencies are all satisfied.
  ;;
  (require (lib "async-channel.ss")
           (lib "contract.ss")
           (lib "list.ss")
           (lib "plt-match.ss"))
  
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Data types
  
  
  ;; A dependency is assumed to be an opaque value that can be only compared by eq?.
  
  
  ;; A target contains the number of unsatisfied dependencies, and the
  ;; element that we return when the dependency count goes to zero.
  (define-struct target (dep-count elt) #f)
  
  ;; tqueues represent the topological queue structure.  They are opaque.
  ;;
  ;; a tqueue holds:
  ;; satisfied-deps: a set of the satisfied dependencies.
  ;; dep-to-targets: a map from a dependency to a (listof target) that depend on it.
  ;; ready: an async-channel that holds all the elements.
  ;;
  ;; External interaction will pass things off through the worker-channel
  ;; to serialize operations into the queue.
  (define-struct tqueue (satisfied-deps dep-to-targets ready worker-channel worker-thread))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
  
  
  ;; new-tqueue: -> tqueue
  ;; Returns a new topological queue.  Creates an internal thread
  ;; to handle serialized access to the thread.
  (define (new-tqueue)
    (let ([a-tqueue
           (make-tqueue (make-hash-table) ;; satisifed-deps will be the keys
                        (make-hash-table) ;; dep-to-targets will map a dep to a list of targets
                        (make-async-channel) ; ready will contain any ready elements.
                        (make-channel) ;; worker channel
                        #f
                        )])
      ;; Start the worker thread loop at this point:
      (set-tqueue-worker-thread!
       a-tqueue
       (thread (lambda () (worker-thread-loop a-tqueue))))
      a-tqueue))
  
  
  ;; cmds will let us interact with the worker thread.
  (define-struct cmd ())
  (define-struct (cmd:add! cmd) (elt deps done))
  (define-struct (cmd:satisfy! cmd) (dep done))
  
  
  ;; tqueue-add!: tqueue X (listof dep) -> void
  ;; Adds a new element to the topological queue.
  (define (tqueue-add! a-tqueue an-elt deps)
    (keep-worker-thread-alive! a-tqueue)
    (let ([sema (make-semaphore)])
      (channel-put (tqueue-worker-channel a-tqueue)
                   (make-cmd:add! an-elt deps sema))
      (sync sema)))
  
  
  ;; tqueue-satisfy!: tqueue dep -> void
  ;; Tells the tqueue that a certain dependency has just been satisfied.
  ;; Any target targets whose dependencies are cleared are moved into the
  ;; ready channel.
  (define (tqueue-satisfy! a-tqueue a-dep)
    (keep-worker-thread-alive! a-tqueue)
    (let ([sema (make-semaphore)])
      (channel-put (tqueue-worker-channel a-tqueue)
                   (make-cmd:satisfy! a-dep sema))
      (sync sema)))
  
  
  ;; tqueue-get: tqueue -> elt
  ;; Gets a ready element from the tqueue.  Blocks if none are available.
  (define (tqueue-get a-tqueue)
    (keep-worker-thread-alive! a-tqueue)
    (async-channel-get (tqueue-ready a-tqueue)))
  
  
  ;; tqueue-try-get: tqueue -> (or/c elt #f)
  ;; Try to get an element out of the queue.  If no such element exists, returns #f.
  (define (tqueue-try-get a-tqueue)
    (keep-worker-thread-alive! a-tqueue)
    (async-channel-try-get (tqueue-ready a-tqueue)))
  
  
  ;; tqueue-ready-channel: tqueue -> async-channel
  ;; Returns low-level access to the async-channel that returns
  ;; the ready elements.
  (define (tqueue-ready-channel a-tqueue)
    (keep-worker-thread-alive! a-tqueue)
    (tqueue-ready a-tqueue))
  
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Helpers and internal definitions.
  
  
  ;; keep-worker-thread-alive!: tqueue -> void
  ;; For kill-safety, we resume the worker thread if it had stopped earlier.
  (define (keep-worker-thread-alive! a-tqueue)
    (thread-resume (tqueue-worker-thread a-tqueue) (current-thread)))
  
  
  ;; worker-thread-loop: tqueue -> void
  ;; The worker thread will just do things.
  (define (worker-thread-loop a-tqueue)
    (let ([cmd (channel-get (tqueue-worker-channel a-tqueue))])
      (match cmd
        [(struct cmd:add! (elt deps done))
         (internal-add! a-tqueue elt deps)
         (semaphore-post done)]
        [(struct cmd:satisfy! (dep done))
         (internal-satisfy! a-tqueue dep)
         (semaphore-post done)]))
    (worker-thread-loop a-tqueue))
  
  
  ;; internal-add!: tqueue X (listof dep) -> void
  ;; Adds a new element to the system.
  (define (internal-add! a-tqueue an-elt deps)
    (let ([a-target (make-target (length deps) an-elt)])
      (cond
        ;; Degenerate case: if there aren't any dependencies, just add it to our ready queue.
        [(empty? deps)
         (when (target-can-execute? a-target)
           (add-to-ready! a-tqueue a-target))]
        
        ;; Otherwise, mark off the dependencies that are already satisfied.
        [else
         (for-each (lambda (a-dep)
                     (cond
                       [(dependency-satisifed? a-tqueue a-dep)
                        (target-decrement-dep-count!/maybe-add-to-ready a-tqueue a-target)]
                       [else
                        (register-dependency! a-tqueue a-target a-dep)]))
                   deps)])))
  
  
  ;; internal-satisfy!: tqueue dep -> void
  ;; Tells the tqueue that a certain dependency has just been satisfied.
  ;; Any target targets whose dependencies are cleared are moved into the
  ;; ready channel.
  (define (internal-satisfy! a-tqueue a-dep)
    (for-each (lambda (a-target)
                (target-decrement-dep-count!/maybe-add-to-ready a-tqueue a-target))
              (hash-table-get (tqueue-dep-to-targets a-tqueue) a-dep '()))
    (hash-table-remove! (tqueue-dep-to-targets a-tqueue) a-dep)
    (hash-table-put! (tqueue-satisfied-deps a-tqueue) a-dep #t))
  
  
  
  ;; dependency-satisifed?: tqueue dep -> boolean
  ;; Returns true when a-tqueue knows that a-dep is already satisfied.
  (define (dependency-satisifed? a-tqueue a-dep)
    (hash-table-get (tqueue-satisfied-deps a-tqueue) a-dep #f))
  
  
  ;; target-decrement-dep-count!: target -> void
  ;; Decrements the dependency count of the target element.
  (define (target-decrement-dep-count!/maybe-add-to-ready a-tqueue a-target)
    (unless (> (target-dep-count a-target) 0)
      (error 'target-decrement-dep-count!
             "Impossible to decrement past zero."))
    (set-target-dep-count! a-target (sub1 (target-dep-count a-target)))
    ;; When the count goes to zero, add to the ready queue.
    (when (target-can-execute? a-target)
      (add-to-ready! a-tqueue a-target)))
  
  
  ;; add-to-ready!: tqueue target -> void
  (define (add-to-ready! a-tqueue a-target)
    (async-channel-put (tqueue-ready a-tqueue) (target-elt a-target)))
  
  
  ;; target-can-execute?: target -> boolean
  ;; Returns true if the target is ready for execution.
  (define (target-can-execute? a-target)
    (= (target-dep-count a-target) 0))
  
  
  ;; register-dependency!: tqueue target dep -> void
  ;; Adds a new dep->target mapping.
  (define (register-dependency! a-tqueue a-target a-dep)
    (let ([ht (tqueue-dep-to-targets a-tqueue)])
      (hash-table-put!
       ht a-dep
       (cons a-target (hash-table-get ht a-dep '())))))
  
  
  
  
  ;; An element can be anything.
  (define elt/c any/c)
  ;; A dependency can be anything.
  (define dep/c any/c)
  
  (provide/contract [new-tqueue (-> tqueue?)]
                    [tqueue? (any/c . -> . boolean?)]
                    [tqueue-add! (tqueue? elt/c (listof dep/c) . -> . any)]
                    [tqueue-satisfy! (tqueue? dep/c . -> . any)]
                    [tqueue-get (tqueue? . -> . elt/c)]
                    [tqueue-try-get (tqueue? . -> . (or/c elt/c false/c))]
                    [tqueue-ready-channel (tqueue? . -> . async-channel?)]))