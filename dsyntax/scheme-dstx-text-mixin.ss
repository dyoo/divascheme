(module scheme-dstx-text-mixin mzscheme
  
  (require (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "list.ss")
           "dsyntax.ss"
           "dstx-text-mixin.ss")
  
  ;; Returns the local id of the dstx.
  (define (dstx-local-id a-dstx dstx-local-id)
    (dstx-property-ref a-dstx 'local-id))
  
  
  ;; A scheme-dstx-text-mixin is specialized to check on certain
  ;; syntactic properties of dstx's.
  ;; In particular:
  ;;
  ;;   * Check that unary fusions are unary.
  ;;   * Check comments that go to end-of-line.
  ;;
  (define (scheme-dstx-text-mixin super%)
    (class (dstx-text-mixin super%)
      (inherit get-dstx-cursor)
      (super-new)
      
      (define/augment (after-structured-insert-after f-cursor)
        (check-neighborhood-of f-cursor))
      
      (define/augment (after-structured-insert-before f-cursor)
        (check-neighborhood-of f-cursor))
      
      (define/augment (after-structured-delete f-cursor)
        (check-neighborhood-of f-cursor))
      
      
      (define (check-neighborhood-of f-cursor)
        ;; Check to see if this itself is bad.
        (when (bad-unary-fusion? (cursor-dstx f-cursor))
          (destruct-fusion! (cursor-dstx f-cursor)))
        
        ;; Then check the parent.
        (when (and (focus-out f-cursor)
                   (bad-unary-fusion? (cursor-dstx (focus-out f-cursor))))
          (destruct-fusion! (cursor-dstx (focus-out f-cursor)))))
      
      
      
      (define (destruct-fusion! a-fusion)
        (let ([cursor (get-dstx-cursor)])
          (send cursor focus-find/dstx!
                (lambda (a-dstx)
                  (= (dstx-local-id a-dstx)
                     (dstx-local-id a-fusion))))
          
          (when (string-nonempty? (fusion-prefix a-fusion))
            (send cursor insert-before!
                  (new-special-atom (new string-snip% [fusion-prefix a-fusion])))
            (send cursor focus-older/no-snap!))
          
          (for-each (lambda (child)
                      (void))
                    (fusion-children a-fusion))
          
          (when (string-nonempty? (fusion-suffix a-fusion))
            (send cursor insert-before!
                  (new-special-atom (new string-snip% [fusion-suffix a-fusion])))
            (send cursor focus-older/no-snap!))
          (send cursor delete!)))))
  
  
  
  
  ;; string-nonempty?: string -> boolean
  ;; Returns true if the string has something in it.
  (define (string-nonempty? a-str)
    (not (= (string-length a-str) 0)))
  
  
  
  ;; bad-unary-fusion?: dstx -> boolean
  ;; Returns true if this is a fusion that is in an inconsistent state.
  (define (bad-unary-fusion? a-dstx)
    (and (fusion? a-dstx)
         (unary-fusion-prefix? (fusion-prefix a-dstx))
         (not (exactly-one-nonspace-child? (fusion-children a-dstx)))))
  
  
  ;; exactly-one-nonspace?: (listof dstx) -> boolean
  ;; Returns true only if there's exactly one nonspace dstx in the
  ;; givne list.
  (define (exactly-one-nonspace-child? dstxs)
    (cond
      [(empty? dstxs)
       #f]
      [(not (space? (first dstxs)))
       (andmap (lambda (a-dstx) (not (space? a-dstx))) (rest dstxs))]
      [else
       (exactly-one-nonspace-child? (rest dstxs))]))
  
  
  ;; unary-fusion-prefix?: string -> boolean
  (define (unary-fusion-prefix? a-prefix)
    (and (member a-prefix (list "'" "`" "," ",@"
                                "#;"
                                "#&"
                                "#'" "#`" "#," "#,@"
                                "#cs" "#ci" "#s"))
         #t)))