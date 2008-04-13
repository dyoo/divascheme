(module operations mzscheme
  (require (lib "contract.ss")
           "rope.ss")
  
  ;; An operation represents the imperative effect of interpreting a
  ;; Protocol-Syntax-Tree.
  (define-struct operation ())
  
  ;; operation:sequence: do all the ops in order.
  (define-struct (operation:sequence operation) (ops))
  
  ;; To do nothing, just return a (make-operation:sequence '()).
  
  ;; operation:insert: insert the rope at the following position
  (define-struct (operation:insert operation) (rope pos))
  
  ;; operation:delete: delete from the following position
  (define-struct (operation:delete operation) (pos len))
  
  ;; operation:move: move from the selection to the other position.
  ;; from-pos and to-pos are both positions relative to the pre-state.
  (define-struct (operation:move operation) (from-pos len to-pos))
  
  
  (provide/contract [struct operation ()]
                    [struct (operation:sequence operation)
                            ([ops (listof operation?)])]
                    [struct (operation:insert operation)
                            ([rope rope?]
                             [pos natural-number/c])]
                    [struct (operation:delete operation)
                            ([pos natural-number/c]
                             [len natural-number/c])]
                    [struct (operation:move operation)
                            ([from-pos natural-number/c]
                             [len natural-number/c]
                             [to-pos natural-number/c])]))