(module mock-woot mzscheme
  (require (lib "contract.ss")
           "../structures.ss"
           "../dsyntax/dsyntax.ss"
           "msg-structs.ss")
  
  (define-struct state (unexecuted a-cursor))
  
  ;; Creates a new mock-woot interface.
  ;; Fixme: we need to initialize with the woot ids of the boundaries.
  (define (new-mock-woot)
    (make-mock-woot '() (make-toplevel-cursor (list))))
  
  
  ;; consume-msg: a-state -> ???
  ;; Add a new message to the state.
  (define (consume-msg! a-state a-msg)
    (set-state-unexecuted! (state-unexecuted a-state)))
  
  
  ;; integrate: mock-woot msg -> (listof ???)
  ;; Needs to return any new operations that we've been able to successfully
  ;; integrate.
  (define (integrate a-state a-msg)
    (void))
  
  
  ;; is-executable?: mock-woot msg -> boolean
  ;; Returns true if we can execute and integrate this message.
  (define (is-executable? a-state a-msg)
    #f)
  
  
  (provide/contract [new-mock-woot (-> mock-woot?)]
                    [consume-msg! (mock-woot? msg? . -> . (listof imperative-op?))]
                    [integrate (mock-woot? msg? . -> . (listof imperative-op?))]))