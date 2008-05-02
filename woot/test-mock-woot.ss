(module test-mock-woot mzscheme
  (require "../dsyntax/dsyntax.ss"
           "utilities.ss"
           "msg-structs.ss"
           (prefix mock: "mock-woot.ss")
           (planet "test.ss" ("schematics" "schemeunit.plt" 2 8))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2 8)))
  
  (provide test-mock-woot)
  
  (define (test)
    (test/text-ui test-mock-woot))
  
  (define sentinel-space
    (dstx-set-woot-id (new-space "")
                      first-sentinel-woot-id))
  
  (define test-mock-woot
    (test-suite
     "test-mock-woot.ss"
     
     (test-case
      "construction"
      (mock:new-mock-woot (list first-sentinel-woot-id)))
     
     (test-case
      "inserting an atom into empty buffer."
      (let* ([woot (mock:new-mock-woot (list first-sentinel-woot-id))]
             [new-cmds
              (mock:consume-msg! (make-msg:insert
                                  (deep-attach-woot-ids (new-atom "hello"))
                                  first-sentinel-woot-id #f))])
        ;; fimxe: check that we get back a command that inserts after the sentinel space.
        (void)))
     
     
     
     (test-case
      "inserting without satisfying precondition should be empty"
      (let* ([woot (mock:new-mock-woot (list first-sentinel-woot-id))]
             [new-cmds
              (mock:consume-msg! (make-msg:insert
                                  (deep-attach-woot-ids (new-atom "hello"))
                                  (fresh-woot-id "test")
                                  #f))])
        (check-equal? new-cmds '()))))))