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
  
  (define host-id "test")
  
  
  (define test-mock-woot
    (test-suite
     "test-mock-woot.ss"
     
     (test-case
      "construction"
      (mock:new-mock-woot (list sentinel-space)))
     
     (test-case
      "inserting an atom into empty buffer."
      (let* ([woot (mock:new-mock-woot (list sentinel-space))]
             [new-cmds
              (mock:consume-msg! (make-msg:insert
                                  host-id
                                  (deep-attach-woot-ids (new-atom "hello") host-id)
                                  first-sentinel-woot-id #f))])
        ;; fimxe: check that we get back a command that inserts after the sentinel space.
        (void)))
     
     
     
     (test-case
      "inserting without satisfying precondition should be empty"
      (let* ([woot (mock:new-mock-woot (list sentinel-space))]
             [new-cmds
              (mock:consume-msg! (make-msg:insert
                                  host-id
                                  (deep-attach-woot-ids (new-atom "hello") host-id)
                                  (fresh-woot-id host-id)
                                  #f))])
        (check-equal? new-cmds '()))))))