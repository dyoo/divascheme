(module tests mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  (require (planet "graphical-ui.ss" ("schematics" "schemeunit.plt" 2)))
  (require "datatype.ss")
  (require "hierarchy.ss")
  (require "class.ss")
  (require "struct.ss")

  (define all-tests
    (test-suite
     "all struct.plt tests"
     test-hierarchy
     test-datatype
     test-class
     test-struct))

  (test/graphical-ui all-tests)

  (provide all-tests))
