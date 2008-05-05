(module topological-queue mzscheme
  ;; Provides a queue for elements that have dependencies.  An element can be pushed
  ;; onto the queue along with its dependencies.  We can also tell the queue
  ;; when some dependency has been satisfied. Finally, we can get elements from the
  ;; queue whose dependencies are all satisfied.
  
  (require (lib "async-channel.ss")
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
  (define (add a-tqueue an-elt deps)
    (void))
  
  
  ;; Gets a ready element from the tqueue.  Blocks if none are available.
  (define (get a-tqueue)
    (async-channel-get (tqueue-ready a-tqueue)))
  
  
  (define (satisfy a-tqueue a-dep)
    (void))
  
  
  ;; An element can be anything.
  (define elt/c any/c)
  ;; A dependency can be anything.
  (define dep/c any/c)
  
  (provide/contract [new-topological-queue (-> tqueue?)]
                    [add (tqueue? elt/c (listof dep/c) . -> . any)]
                    [get (tqueue? . -> . elt/c)]
                    [satisfy (tqueue? dep/c . -> . any)]))