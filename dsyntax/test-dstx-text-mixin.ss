(module test-dstx-text-mixin mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "plt-match.ss")
           (planet "test.ss" ("schematics" "schemeunit.plt" 2 8))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2 8))
           (prefix table: (planet "table.ss" ("soegaard" "galore.plt" 3)))
           "struct.ss"
           "cursor.ss"
           "dstx-text-mixin.ss")
  
  
  (provide test-dstx-text-mixin)
  
  (define (test)
    (test/text-ui test-dstx-text-mixin))
  
  (define (make-text-instance)
    (new (dstx-text-mixin text%)))
  
  
  (define (strip-local-ids a-dstx)
    (dstx-deepmap (lambda (a-dstx)
                    (dstx-property-strip a-dstx))
                  a-dstx))
  
  (define empty-table
    (let ()
      ;; symbol-cmp: symbol symbol -> (or/c -1 0 1)
      (define (symbol-cmp sym-1 sym-2)
        (let ([s1 (symbol->string sym-1)]
              [s2 (symbol->string sym-2)])
          (cond
            [(string<? s1 s2) -1]
            [(string>? s1 s2) 1]
            [else 0])))
      (table:make-ordered symbol-cmp)))
  
  
  (define (dstx-property-strip a-dstx)
    (match a-dstx
      [(struct atom (_ content))
       (make-atom empty-table content)]
      [(struct special-atom (_ content width))
       (make-special-atom empty-table content width)]
      [(struct space (_ content))
       (make-space empty-table content)]
      [(struct fusion (_ prefix children suffix))
       (make-fusion empty-table prefix children suffix)]))
  
  
  (define test-dstx-text-mixin
    (test-suite
     "test-dstx-text-mixin.ss"
     (test-case
      "inserting a single element"
      (let* ([text (make-text-instance)]
             [cursor (send text get-dstx-cursor)])
        (send cursor cursor-insert-after (new-atom "hello"))
        ;; Check what's on screen...
        (check-equal? (send text get-text) "hello")
        ;; As well as what's in the dstx
        (check-equal? (map strip-local-ids (send text get-top-dstxs))
                      (map strip-local-ids (list (new-space "") (new-atom "hello"))))
        (check-true (number? (send cursor cursor-dstx-property-ref 'local-id)))))
     
     
     (test-case
      "inserting two elements with insert-after"
      (let* ([text (make-text-instance)]
             [cursor (send text get-dstx-cursor)])
        (send cursor cursor-insert-after (new-atom "hello"))
        (send cursor cursor-insert-after (new-space " "))
        (send cursor cursor-insert-after (new-atom "world"))
        ;; Check what's on screen...
        (check-equal? (send text get-text) "hello world")
        ;; As well as what's in the dstx
        (check-equal? (map strip-local-ids (send text get-top-dstxs))
                      (map strip-local-ids (list (new-space "")
                                                 (new-atom "hello")
                                                 (new-space " ")
                                                 (new-atom "world"))))
        (check-true (number? (send cursor cursor-dstx-property-ref 'local-id)))
        (check-equal? (strip-local-ids (send cursor cursor-dstx))
                      (strip-local-ids (new-atom "world")))
        (send cursor focus-predecessor)
        (check-true (number? (send cursor cursor-dstx-property-ref 'local-id)))
        (check-equal? (strip-local-ids (send cursor cursor-dstx))
                      (strip-local-ids (new-atom "hello")))))
     
     
     (test-case
      "inserting two elements with insert-before"
      (let* ([text (make-text-instance)]
             [cursor (send text get-dstx-cursor)])
        (send cursor cursor-insert-after (new-atom "world"))
        (send cursor cursor-insert-before (new-space " "))
        (send cursor cursor-insert-before (new-atom "goodbye"))
        ;; Check what's on screen...
        (check-equal? (send text get-text) "goodbye world")
        ;; As well as what's in the dstx
        (check-equal? (map strip-local-ids (send text get-top-dstxs))
                      (map strip-local-ids (list (new-space "")
                                                 (new-atom "goodbye")
                                                 (new-space " ")
                                                 (new-atom "world"))))))
     
     (test-case
      "editing a symbol at the front"
      (let* ([text (make-text-instance)]
             [cursor (send text get-dstx-cursor)])
        (send cursor cursor-insert-after (new-atom "orld"))
        (send text insert "w" 0)
        
        ;; Check what's on screen...
        (check-equal? (send text get-text) "world")
        ;; As well as what's in the dstx
        (check-equal? (map strip-local-ids (send text get-top-dstxs))
                      (map strip-local-ids (list (new-space "") (new-atom "world"))))))
     
     
     (test-case
      "editing a symbol internally"
      (let* ([text (make-text-instance)]
             [cursor (send text get-dstx-cursor)])
        (send cursor cursor-insert-after (new-atom "wrld"))
        (send text insert "o" 1)
        
        ;; Check what's on screen...
        (check-equal? (send text get-text) "world")
        ;; As well as what's in the dstx
        (check-equal? (map strip-local-ids (send text get-top-dstxs))
                      (map strip-local-ids (list (new-space "") (new-atom "world")))))))))