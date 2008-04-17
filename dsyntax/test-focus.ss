(module test-focus mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2 8))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2 8))
           "cursor.ss"
           "struct.ss")
  
  (provide test-focus)
  
  (define test-focus
    (test-suite
     "test-focus.ss"
     (test-case
      "focus-toplevel"
      (let ([a-cursor (make-toplevel-cursor (list (new-atom "hello") (new-atom "world")))])
        (check-equal? (cursor-dstx (focus-toplevel a-cursor))
                      (new-atom "hello"))
        (check-equal? (cursor-dstx (focus-successor (focus-toplevel a-cursor)))
                      (new-atom "world"))))
     
     
     (test-case
      "focus-pos"
      (let ([a-cursor (make-toplevel-cursor (list (new-atom "foo")
                                                  (new-atom "bar")))])
        (check-equal? (cursor-dstx (focus-pos a-cursor 0))
                      (new-atom "foo"))
        (check-equal? (cursor-dstx (focus-pos a-cursor 1))
                      (new-atom "foo"))
        (check-equal? (cursor-dstx (focus-pos a-cursor 2))
                      (new-atom "foo"))
        (check-equal? (cursor-dstx (focus-pos a-cursor 3))
                      (new-atom "bar"))
        (check-equal? (cursor-dstx (focus-pos a-cursor 4))
                      (new-atom "bar"))
        (check-equal? (cursor-dstx (focus-pos a-cursor 5))
                      (new-atom "bar"))
        (check-equal? (cursor-dstx (focus-pos a-cursor 6))
                      (new-atom "bar"))))
     
     
     (test-case
      "focus-pos on a space"
      (let ([a-cursor (make-toplevel-cursor (list (new-space " ")))])
        (check-equal? (cursor-dstx (focus-pos a-cursor 0))
                      (new-space " "))
        (check-equal? (cursor-dstx (focus-pos a-cursor 1))
                      (new-space " "))))
     
     (test-case
      "focus-pos at an ending space"
      (let ([a-cursor (make-toplevel-cursor (list (new-atom "x") (new-space " ")))])
        (check-equal? (cursor-dstx (focus-pos a-cursor 0))
                      (new-atom "x"))
        (check-equal? (cursor-dstx (focus-pos a-cursor 1))
                      (new-space " "))
        (check-equal? (cursor-dstx (focus-pos a-cursor 2))
                      (new-space " "))))
     
     (test-case
      "focus-in and focus-out, when no modifications occur, should preserve fusions."
      (let ([a-cursor (make-toplevel-cursor
                       (list (new-fusion "("
                                         (list (new-atom "hello"))
                                         ")")))])
        (check-eq? (cursor-dstx a-cursor)
                   (cursor-dstx (focus-out (focus-in a-cursor))))))
     
     (test-case
      "focus-in and focus-out should only preserve fusions on eq?"
      (let ([a-cursor (make-toplevel-cursor
                       (list (new-fusion "("
                                         (list (new-special-atom "hello"))
                                         ")")))])
        (check-false (eq? (cursor-dstx a-cursor)
                          (cursor-dstx
                           (focus-out
                            (insert-after
                             (delete (focus-in a-cursor))
                             (new-special-atom "hello"))))))))
     
     
     (test-case
      "focus-pos with structure"
      (let* ([a-dstx (new-fusion "["
                                 (list (new-atom "box"))
                                 "]")]
             [a-cursor (make-toplevel-cursor (list a-dstx))])
        (check-equal? (cursor-dstx (focus-pos a-cursor 0))
                      a-dstx)
        (check-equal? (cursor-dstx (focus-pos a-cursor 1))
                      (new-atom "box"))
        (check-equal? (cursor-dstx (focus-pos a-cursor 2))
                      (new-atom "box"))
        (check-equal? (cursor-dstx (focus-pos a-cursor 3))
                      (new-atom "box"))
        (check-equal? (cursor-dstx (focus-pos a-cursor 4))
                      (new-atom "box"))))
     
     (test-case
      "focus-container on atoms"
      (let* ([a-dstx (new-atom "hi")]
             [a-cursor (make-toplevel-cursor (list a-dstx))])
        (check-equal? (cursor-dstx (focus-container a-cursor 0))
                      (new-atom "hi"))
        (check-equal? (cursor-dstx (focus-container a-cursor 1))
                      (new-atom "hi"))
        (check-equal? (focus-container a-cursor 2)
                      #f)))
     
     (test-case
      "focus-container when the cursor is not at the beginning"
      (let* ([a-dstx (new-fusion "(" (list (new-atom "x")
                                           (new-space " ")
                                          (new-atom "y"))
                                 ")")]
             [a-cursor (make-toplevel-cursor (list a-dstx))])
        (check-equal? (cursor-dstx (focus-container
                                    (focus-oldest (focus-in a-cursor)) 2))
                      (new-space " "))))
     
     
     (test-case
      "focus-container on fusions"
      (let* ([a-dstx (new-fusion "("
                                 (list (new-atom "bye"))
                                 ")")]
             [a-cursor (make-toplevel-cursor (list a-dstx))])
        (check-equal? (cursor-dstx (focus-container a-cursor 0))
                      (new-fusion "("
                                  (list (new-atom "bye"))
                                  ")"))
        (check-equal? (cursor-dstx (focus-container a-cursor 1))
                      (new-atom "bye"))
        (check-equal? (cursor-dstx (focus-container a-cursor 2))
                      (new-atom "bye"))
        (check-equal? (cursor-dstx (focus-container a-cursor 3))
                      (new-atom "bye"))
        (check-equal? (cursor-dstx (focus-container a-cursor 4))
                      (new-fusion "("
                                  (list (new-atom "bye"))
                                  ")"))
        (check-equal? (focus-container a-cursor 5)
                      #f)))))
  
  
  (define (test)
    (test/text-ui test-focus)))