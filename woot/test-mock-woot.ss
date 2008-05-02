(module test-mock-woot mzscheme
  (require "../dsyntax/dsyntax.ss"
           "utilities.ss"
           "woot-struct.ss"
           (lib "list.ss")
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
             [an-atom (deep-attach-woot-ids (new-atom "hello") host-id)]
             [new-ops
              (mock:consume-msg! woot
                                 (make-msg:insert
                                  host-id
                                  an-atom
                                  first-sentinel-woot-id #f))])
        (check-equal? (length new-ops) 1)
        (let ([first-op (first new-ops)])
          (check-true (op:insert-after? first-op))
          (check-equal? (op:insert-after-id first-op) first-sentinel-woot-id)
          (check-eq? (op:insert-after-dstx first-op) an-atom))))
     
     
     
     (test-case
      "inserting without satisfying precondition should be empty"
      (let* ([woot (mock:new-mock-woot (list sentinel-space))]
             [new-cmds
              (mock:consume-msg! woot
                                 (make-msg:insert
                                  host-id
                                  (deep-attach-woot-ids (new-atom "hello") host-id)
                                  (fresh-woot-id host-id)
                                  #f))])
        (check-equal? new-cmds '()))))))