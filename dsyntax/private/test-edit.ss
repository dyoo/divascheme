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
      "inserting a single atom."
      (let ([cursor (make-toplevel-cursor '())])
        (let ([new-cursor
               (cursor-insert-before cursor (new-atom "hello"))])
          (check-equal? (cursor-dstx (focus-toplevel new-cursor))
                        (new-atom "hello"))))))))