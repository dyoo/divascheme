(module test-focus mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2 8))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2 8))
           "focus.ss"
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
                      (new-atom "bar"))))))
  
  
  (define (test)
    (test/text-ui test-focus)))