(module dsyntax-test-suite mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2 8))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2 8))
           "test-dsyntax.ss"
           "test-edit.ss"
           "test-focus.ss"
           "test-parse-plt-scheme.ss"
           "test-dstx-text-mixin.ss")
  
  (define dsyntax-test-suite
    (test-suite
     "all tests for the dsyntax package"
     test-dsyntax
     test-edit
     test-focus
     test-parse-plt-scheme
     test-dstx-text-mixin))
  
  (define (test)
    (test/text-ui dsyntax-test-suite)))