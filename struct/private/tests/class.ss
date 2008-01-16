(module class mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2)))
  (require (lib "class.ss"))
  (require "../../class.ss")
  (require (lib "match.ss"))
  (require (prefix plt: (lib "plt-match.ss")))

  (define my-class%
    (class object%
      (public my-method)
      (init-private foo)
      (init-private (bar 'default-bar))
      (field (mumble 55))
      (define (my-method)
        (list foo bar))
      (super-new)))

  (define x (new my-class% (foo 'a-foo-value)))
  (define y (new my-class% (foo 'another-foo) (bar 'bar-bar-bar)))

  ;; TODO: make some tests!
  ;;  - default values
  ;;  - check that private names are private
  ;;  - check that init and init-field are distinct

  (define test-%
    (test-suite
     "% tests"
     (test-case "field name used as variable name"
                     (check = (match x
                                 [(% my-class% mumble)
                                  mumble])
                             55))
     (test-case "field name bound to another name"
                     (check = (match x
                                 [(% my-class% [fumble mumble])
                                  fumble])
                             55))
     (test-case "method call"
                     (check-equal? (match x
                                      [(% my-class% [foo (my-method)])
                                       foo])
                                    '(a-foo-value default-bar)))
     (test-case "field name used as variable name (plt-match)"
                     (check = (plt:match x
                                 [(% my-class% mumble)
                                  mumble])
                             55))
     (test-case "field name bound to another name (plt-match)"
                     (check = (plt:match x
                                 [(% my-class% [fumble mumble])
                                  fumble])
                             55))
     (test-case "method call (plt-match)"
                     (check-equal? (plt:match x
                                      [(% my-class% [foo (my-method)])
                                       foo])
                                    '(a-foo-value default-bar)))
     ))

  (define test-class
    (test-suite
     "all class.ss tests"
     test-%
     ))

  (provide test-class))
