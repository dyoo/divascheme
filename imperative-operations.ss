(module imperative-operations mzscheme
  (require (lib "contract.ss")
           (lib "plt-match.ss")
           (lib "mred.ss" "mred")
           "rope.ss"
           "structures.ss")
  
  (define-struct op ())
  
  ;; Primitive operations that don't deal with the buffer
  ;; as structure.
  
  ;; op:sequence: do the operations in order
  (define-struct (op:sequence op) (ops))
  
  ;; op:insert-rope: insert the rope at position a-pos
  (define-struct (op:insert-rope op) (rope a-pos))
  
  ;; op:delete: delete between start-pos and end-pos
  (define-struct (op:delete-range op) (start-pos end-pos))
  
  
  
  
  ;; apply-op: op world text% update-world-fn update-mred-fn
  ;;
  ;; fixme: do we really need update-world-fn and update-mred-fn?
  (define (apply-op an-op a-world a-text update-world-fn update-mred-fn)
    (match an-op
      [(struct op:sequence (ops))
       (void)]
      [(struct op:insert-rope (rope a-pos))
       (void)]
      [(struct op:delete-range (rope a-pos))
       (void)]))
  
  
  (provide/contract
   [struct op ()]
   [struct (op:sequence op) ([ops (listof op?)])]
   [struct (op:insert-rope op) ([rope rope?]
                                [a-pos natural-number/c])]
   [struct (op:delete-range op) ([start-pos natural-number/c]
                                 [end-pos natural-number/c])]
   
   [apply-op (op?
              World?
              (is-a?/c text%)
              (World? . -> . World?)
              (World? . -> . World?)
              . -> . any)]))