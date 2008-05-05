(module topological-queue mzscheme
  ;; Provides a queue for elements that have dependencies.  An element can be pushed
  ;; onto the queue along with its dependencies.  We can also tell the queue
  ;; when some dependency has been satisfied. Finally, we can get elements from the
  ;; queue whose dependencies are all satisfied.
  
  (require (lib "async-channel.ss")
           (lib "plt-match.ss")
           (lib "contract.ss"))
  
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Data types
  
  
  ;; A dependency is assumed to be an opaque value that can be only compared by eq?.
  
  
  ;; an unexecuted contains the number of unsatisfied dependencies, and the
  ;; element that we return when the dependency count goes to zero.
  (define-struct unexecuted (dep-count elt))
  
  ;; a tqueue holds:
  ;; satisfied-deps: a set of the satisfied dependencies.
  ;; dep-to-unexecuteds: a map from a dependency to a (listof unexecuted) that depend on it.
  ;; ready: an async-channel that holds all the elements.
  (define-struct tqueue (satisfied-deps dep-to-unexecuteds ready))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
  
  
  
  ;; new-topological-queue: -> tqueue
  ;; Returns a new topological queue.
  (define (new-topological-queue)
    (make-tqueue (make-hash-table)
                 (make-hash-table)
                 (make-async-channel)))
  
  
  ;; add: tqueue X (listof dep) -> void
  ;; Adds a new element to the system.
  ;; Fixme: not kill safe.
  (define (add a-tqueue an-elt deps)
    (match a-tqueue
      [(struct tqueue (satisfied-deps dep-to-unexecuteds ready))
       
       (let ([an-unexecuted (make-unexecuted (length deps) an-elt)])
         ;; Mark off the dependencies that are already satisfied.
         (for-each (lambda (a-dep)
                     (cond
                       [(satisifed? a-tqueue a-dep)
                        (unexecuted-decrement-dep-count! a-tqueue an-unexecuted)]
                       [else
                        (register-dependency! a-tqueue an-unexecuted a-dep)]))
                   deps))]))
  
  
  
  
  ;; satisifed?: tqueue dep -> boolean
  ;; Returns true when a-tqueue knows that a-dep is already satisfied.
  (define (satisifed? a-tqueue a-dep)
    (hash-table-get (tqueue-satisfied-deps a-tqueue) a-dep #f))
  
  
  ;; unexecuted-decrement-dep-count!: unexecuted -> void
  ;; Decrements the dependency count of the unexecuted element.
  (define (unexecuted-decrement-dep-count! a-tqueue an-unexecuted)
    (unless (> (unexecuted-dep-count an-unexecuted) 0)
      (error 'unexecuted-decrement-dep-count!
             "Impossible to decrement past zero."))
    (set-unexecuted-dep-count! an-unexecuted
                               (sub1 (unexecuted-dep-count an-unexecuted)))
    
    ;; And if the count goes to zero, add to the ready queue.
    (when (unexecuted-can-execute? an-unexecuted)
      (async-channel-put (tqueue-ready a-tqueue) (unexecuted-elt an-unexecuted))))
  
  
  
  ;; unexecuted-can-execute?: unexecuted -> boolean
  ;; Returns true if the unexecuted is ready for execution.
  (define (unexecuted-can-execute? an-unexecuted)
    (= (unexecuted-dep-count an-unexecuted) 0))
  
  
  
  ;; Gets a ready element from the tqueue.  Blocks if none are available.
  (define (get a-tqueue)
    (async-channel-get (tqueue-ready a-tqueue)))
  
  
  (define (register-dependency! a-tqueue an-unexecuted a-dep)
    ;; fixme
    (void))
  
  
  (define (satisfy a-tqueue a-dep)
    ;; fixme
    (void))
  
  
  
  
  ;; An element can be anything.
  (define elt/c any/c)
  ;; A dependency can be anything.
  (define dep/c any/c)
  
  (provide/contract [new-topological-queue (-> tqueue?)]
                    [add (tqueue? elt/c (listof dep/c) . -> . any)]
                    [get (tqueue? . -> . elt/c)]
                    [satisfy (tqueue? dep/c . -> . any)]))