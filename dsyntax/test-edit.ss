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
                        (new-atom "greetings"))))))))