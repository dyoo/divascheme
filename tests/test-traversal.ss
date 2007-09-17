(module test-traversal mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2 8))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2 8))
           (lib "list.ss")
           (lib "etc.ss")
           "../traversal.ss"
           "../semi-read-syntax/semi-read-syntax.ss")
  
  (define traversal-tests
    (test-suite
     "traversal.ss"
     (test-case
      "find-pos-parent of the empty list should be #f"
      (check-false
       (find-pos-parent 1 (semi-read-syntax-list #f (open-input-string "()")))))
     
     (test-case
      "find-pos-parent within the empty list should return that list."
      (local [(define syntaxes
                (semi-read-syntax-list #f (open-input-string "()")))]
        (check-eq?
         (find-pos-parent 2 syntaxes)
         (first syntaxes))))))
  
  (test/text-ui traversal-tests))