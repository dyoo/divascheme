(module test-edit mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2 8))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2 8))
           "cursor.ss"
           "struct.ss")
  
  (define (test)
    (test/text-ui test-edit))
  
  (provide test-edit)
  
  (define test-edit
    (test-suite
     "test-edit.ss"
     (test-case
      "inserting a single atom before."
      (let ([cursor (make-toplevel-cursor (list (new-atom "world")))])
        (let ([new-cursor
               (cursor-insert-before cursor (new-atom "hello"))])
          (check-equal? (cursor-dstx new-cursor)
                        (new-atom "hello"))
          (check-equal? (cursor-dstx
                         (focus-older new-cursor))
                        (new-atom "world")))))
     
     
     (test-case
      "inserting a single atom after."
      (let ([cursor (make-toplevel-cursor (list (new-atom "greetings")))])
        (let ([new-cursor
               (cursor-insert-after cursor (new-atom "earthling"))])
          (check-equal? (cursor-dstx new-cursor)
                        (new-atom "earthling"))
          (check-equal? (cursor-dstx
                         (focus-younger new-cursor))
                        (new-atom "greetings")))))
     
     
     (test-case
      "inserting from an empty toplevel"
      (let* ([a-cursor (make-toplevel-cursor '())]
             [a-cursor (cursor-insert-after a-cursor (new-atom "hello"))]
             [a-cursor (cursor-insert-after a-cursor (new-atom "this"))]
             [a-cursor (cursor-insert-after a-cursor (new-atom "is"))]
             [a-cursor (cursor-insert-after a-cursor (new-fusion "("
                                                                 (list (new-atom "a") (new-atom "test"))
                                                                 ")"))])
        (check-equal? (cursor-dstx a-cursor)
                      (new-fusion "("
                                  (list (new-atom "a") (new-atom "test"))
                                  ")"))
        (check-equal? (cursor-dstx (focus-younger a-cursor))
                      (new-atom "is"))))
     
     (test-case
      "inserting into a fusion"
      (let* ([a-cursor (make-toplevel-cursor (list (new-fusion "(" '() ")")))]
             [a-cursor (focus-in/no-snap a-cursor)]
             [a-cursor (cursor-insert-after a-cursor (new-atom "inside"))])
        (check-equal? (cursor-dstx a-cursor) (new-atom "inside"))
        (check-equal? (cursor-dstx (focus-out a-cursor))
                      (new-fusion "(" (list (new-atom "inside")) ")"))))
     
     (test-case
      "setting a property"
      (let ([cursor (make-toplevel-cursor (list (new-atom "answer")))])
        (check-equal? (dstx-property-names (cursor-dstx cursor)) '())
        (let ([new-cursor
               (cursor-dstx-property-set cursor 'value 42)])
          (check-equal? (dstx-property-names (cursor-dstx new-cursor)) '(value))
          (check-equal? (atom-content (cursor-dstx new-cursor)) "answer")
          (check-equal? (dstx-property-ref (cursor-dstx new-cursor) 'value)
                        42))))
     
     (test-case
      "setting a property twice"
      (let ([cursor (make-toplevel-cursor (list (new-atom "answer")))])
        (check-equal? (dstx-property-names (cursor-dstx cursor)) '())
        (let* ([new-cursor
                (cursor-dstx-property-set cursor 'value 42)]
               [new-cursor
                (cursor-dstx-property-set new-cursor 'value
                                          (add1 (dstx-property-ref (cursor-dstx new-cursor) 'value)))])
          (check-equal? (dstx-property-names (cursor-dstx new-cursor)) '(value))
          (check-equal? (atom-content (cursor-dstx new-cursor)) "answer")
          (check-equal? (dstx-property-ref (cursor-dstx new-cursor) 'value)
                        43))))
     
     
     (test-case
      "deleting the empty toplevel should be idempotent"
      (let ([a-cursor (make-toplevel-cursor (list))])
        (let* ([new-cursor (cursor-delete a-cursor)])
          (check-equal? a-cursor new-cursor))))
     
     (test-case
      "deleting a single toplevel atom"
      (let ([a-cursor (make-toplevel-cursor (list (new-atom "DELETED!")))])
        (check-equal? (cursor-delete a-cursor)
                      (make-toplevel-cursor (list)))))
     
     (test-case
      "deleting from two toplevel atoms"
      (let ([a-cursor (make-toplevel-cursor (list (new-atom "x")
                                                  (new-atom "y")))])
        (check-equal? (cursor-delete a-cursor)
                      (make-toplevel-cursor (list (new-atom "y"))))))
     
     (test-case
      "deleting from second of the two toplevel atoms"
      (let* ([a-cursor (make-toplevel-cursor (list (new-atom "x")
                                                   (new-atom "y")))]
             [a-cursor (focus-successor a-cursor)])
        (check-equal? (cursor-delete a-cursor)
                      (make-toplevel-cursor (list (new-atom "x"))))))
     
     (test-case
      "deleting from inside a fusion (removing y)"
      (let* ([a-cursor (make-toplevel-cursor
                        (list (new-atom "x")
                              (new-fusion "[" (list (new-atom "y")
                                                    (new-atom "z"))
                                          "]")))]
             [a-cursor (focus-successor a-cursor)]
             [a-cursor (focus-in a-cursor)])
        ;; check that the focus moved to z
        (check-equal? (cursor-dstx (cursor-delete a-cursor))
                      (new-atom "z"))
        ;; and check content.
        (check-equal? (focus-toplevel (cursor-delete a-cursor))
                      (make-toplevel-cursor
                       (list (new-atom "x")
                             (new-fusion "[" (list (new-atom "z"))
                                         "]"))))))
     
     
     (test-case
      "deleting from inside a fusion (removing z)"
      (let* ([a-cursor (make-toplevel-cursor
                        (list (new-atom "x")
                              (new-fusion "[" (list (new-atom "y")
                                                    (new-atom "z"))
                                          "]")))]
             [a-cursor (focus-successor a-cursor)]
             [a-cursor (focus-successor a-cursor)]
             [a-cursor (focus-successor a-cursor)])
        ;; check the new focus...
        (check-equal? (cursor-dstx (cursor-delete a-cursor))
                      (new-atom "y"))
        ;; and check the content.
        (check-equal? (focus-toplevel (cursor-delete a-cursor))
                      (make-toplevel-cursor
                       (list (new-atom "x")
                             (new-fusion "[" (list (new-atom "y"))
                                         "]")))))))))
