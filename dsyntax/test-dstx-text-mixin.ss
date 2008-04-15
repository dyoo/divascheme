(module test-dstx-text-mixin mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "plt-match.ss")
           (lib "list.ss")
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
  
  
  ;; Remove local ids from a dstx, just for testing purposes.
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
      "deleting a single element"
      (let* ([text (make-text-instance)]
             [cursor (send text get-dstx-cursor)])
        (send cursor cursor-insert-after (new-atom "hello"))
        (send cursor cursor-delete)
        (check-equal? (send text get-text) "")
        (check-equal? (map strip-local-ids (send text get-top-dstxs))
                      (map strip-local-ids (list (new-space ""))))))
     
     
     (test-case
      "deleting between two"
      (let* ([text (make-text-instance)]
             [cursor (send text get-dstx-cursor)])
        (send cursor cursor-insert-after (new-atom "A"))
        (send cursor cursor-insert-after (new-atom "B"))
        (send cursor cursor-insert-after (new-atom "C"))
        (send cursor focus-younger)
        (send cursor cursor-delete)
        (check-equal? (send text get-text) "AC")
        (check-equal? (strip-local-ids (send cursor cursor-dstx))
                      (strip-local-ids (new-atom "C")))))
     
     
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
      "manually editing a symbol at the front"
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
      "manually editing a symbol at the back"
      (let* ([text (make-text-instance)]
             [cursor (send text get-dstx-cursor)])
        (send cursor cursor-insert-after (new-atom "worl"))
        (send cursor cursor-insert-after (new-space " "))
        (send cursor cursor-insert-after (new-atom "peace"))
        (send text insert "d" 4)
        
        ;; Check what's on screen...
        (check-equal? (send text get-text) "world peace")
        ;; As well as what's in the dstx
        (check-equal? (map strip-local-ids (send text get-top-dstxs))
                      (map strip-local-ids (list (new-space "")
                                                 (new-atom "world")
                                                 (new-space " ")
                                                 (new-atom "peace"))))))
     
     
     (test-case
      "manually editing a symbol at the back"
      (let* ([text (make-text-instance)]
             [cursor (send text get-dstx-cursor)])
        (send cursor cursor-insert-after (new-atom "worl"))
        (send text insert "d" 4)
        ;; Check what's on screen...
        (check-equal? (send text get-text) "world")
        ;; As well as what's in the dstx
        (check-equal? (map strip-local-ids (send text get-top-dstxs))
                      (map strip-local-ids (list (new-space "")
                                                 (new-atom "world"))))))
     
     
     (test-case
      "manually editing a symbol internally"
      (let* ([text (make-text-instance)]
             [cursor (send text get-dstx-cursor)])
        (send cursor cursor-insert-after (new-atom "wrld"))
        (send text insert "o" 1)
        
        ;; Check what's on screen...
        (check-equal? (send text get-text) "world")
        ;; As well as what's in the dstx
        (check-equal? (map strip-local-ids (send text get-top-dstxs))
                      (map strip-local-ids (list (new-space "") (new-atom "world"))))))
     
     
     (test-case
      "inserting a fusion"
      (let* ([text (make-text-instance)]
             [cursor (send text get-dstx-cursor)])
        (send cursor cursor-insert-after (new-fusion "("
                                                     (list (new-atom "hello"))
                                                     ")"))
        ;; Check what's on screen...
        (check-equal? (send text get-text) "(hello)")
        ;; As well as what's in the dstx
        (check-equal? (map strip-local-ids (send text get-top-dstxs))
                      (map strip-local-ids (list (new-space "") (new-fusion "(" (list (new-atom "hello")) ")"))))))
     
     
     (test-case
      "inserting inside a fusion"
      (let* ([text (make-text-instance)]
             [cursor (send text get-dstx-cursor)])
        (send cursor cursor-insert-after (new-fusion "("
                                                     (list (new-atom "hello"))
                                                     ")"))
        (send cursor focus-in)
        (send cursor cursor-insert-after (new-space " "))
        (send cursor cursor-insert-after (new-fusion "["
                                                     (list (new-atom "world"))
                                                     "]"))
        ;; Check what's on screen...
        (check-equal? (send text get-text) "(hello [world])")
        ;; As well as what's in the dstx
        (check-equal? (map strip-local-ids
                           (send text get-top-dstxs))
                      (map strip-local-ids
                           (list (new-space "")
                                 (new-fusion "("
                                             (list (new-atom "hello")
                                                   (new-space " ")
                                                   (new-fusion "["
                                                               (list (new-atom "world"))
                                                               "]"))
                                             ")"))))))
     
     (test-case
      "inserting nonsense becomes a special atom"
      (let* ([text (make-text-instance)])
        (send text insert "(foo!")
        ;; Check what's on screen...
        (check-equal? (send text get-text) "(foo!")
        (check-equal? (length (send text get-top-dstxs)) 1)
        (let* ([should-be-special (first (send text get-top-dstxs))]
               [snip (special-atom-content should-be-special)])
          (check-true (is-a? snip string-snip%))
          (check-equal? (send snip get-text 0 (send snip get-count))
                        "(foo!"))))
     
     (test-case
      "manually editing the front of an atom with delete"
      (let* ([text (make-text-instance)])
        (let ([cursor (send text get-dstx-cursor)])
          (send cursor cursor-insert-after (new-atom "an-atom")))
        (send text delete 0 3)
        (check-equal? (send text get-text) "atom")
        (check-equal? (map strip-local-ids (send text get-top-dstxs))
                      (map strip-local-ids (list (new-space "")
                                                 (new-atom "atom"))))))
     
     
     (test-case
      "manually editing the end of an atom with delete"
      (let* ([text (make-text-instance)])
        (let ([cursor (send text get-dstx-cursor)])
          (send cursor cursor-insert-after (new-atom "an-at"))
          (send cursor cursor-insert-after (new-atom "om-bomb")))
        (send text delete 7 12)
        (check-equal? (send text get-text) "an-atom")
        (check-equal? (map strip-local-ids (send text get-top-dstxs))
                      (map strip-local-ids (list (new-space "")
                                                 (new-atom "an-at")
                                                 (new-atom "om"))))))
     
     (test-case
      "manually deleting some spaces in a fusion"
      (let* ([text (make-text-instance)])
        (send text insert "(module  foo mzscheme)")
        (let* ([f-cursor (send (send text get-dstx-cursor) get-functional-cursor)])
          (let ([id1 (cursor-dstx-property-ref
                      (focus-in f-cursor)
                      'local-id)]
                [id2 (cursor-dstx-property-ref
                      (focus-older (focus-in f-cursor))
                      'local-id)]
                [id3 (cursor-dstx-property-ref
                      (focus-older
                       (focus-older
                        (focus-in f-cursor)))
                      'local-id)])
            (send text delete 7 8)
            (check-equal? (send text get-text) "(module foo mzscheme")
            (check-equal? (map strip-local-ids (send text get-top-dstxs))
                          (map strip-local-ids
                               (list
                                (new-fusion "("
                                            (list (new-atom "module")
                                                  (new-space " ")
                                                  (new-atom "foo")
                                                  (new-space " ")
                                                  (new-atom "mzscheme"))
                                            ")"))))
            (let ([cursor (send text get-dstx-cursor)])
              (send cursor focus-in)
              (check-equal? (cursor-dstx-property-ref 'local-id) id1)
              (send cursor focus-older)
              (check-equal? (cursor-dstx-property-ref 'local-id) id2)
              (send cursor focus-older)
              (check-equal? (cursor-dstx-property-ref 'local-id) id3))))
        )))))