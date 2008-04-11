(module test-edit mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2 8))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2 8))
           "edit.ss"
           "struct.ss"
           "focus.ss")
  
  (define (test)
    (test/text-ui test-edit))
  
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
                        43)))))))