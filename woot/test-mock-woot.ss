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
  
  (define (decorate a-dstx)
    (deep-attach-woot-ids a-dstx host-id))
  
  
  (define test-mock-woot
    (test-suite
     "test-mock-woot.ss"
     
     (test-case
      "construction"
      (mock:new-mock-woot (list sentinel-space)))
     
     
     (test-case
      "inserting an atom into empty buffer."
      (let* ([woot (mock:new-mock-woot (list sentinel-space))]
             [an-atom (decorate (new-atom "hello"))]
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
                                  (decorate (new-atom "hello"))
                                  (fresh-woot-id host-id)
                                  #f))])
        (check-equal? new-cmds '())))
     
     
     (test-case
      "inserting hello world in reverse order"
      (let* ([woot (mock:new-mock-woot (list sentinel-space))]
             [world-atom (decorate (new-atom "world"))]
             [hello-atom (decorate (new-atom "hello"))]
             [new-cmds-1
              (mock:consume-msg! woot
                                 (make-msg:insert host-id world-atom (dstx-woot-id hello-atom) #f))]
             [new-cmds-2
              (mock:consume-msg! woot
                                 (make-msg:insert host-id hello-atom (dstx-woot-id sentinel-space) #f))])
        (check-equal? new-cmds-1 '())
        (check-equal? (length new-cmds-2) 2)
        (check-equal? (op:insert-after-dstx (first new-cmds-2))
                      hello-atom)
        (check-equal? (op:insert-after-id (first new-cmds-2))
                      (dstx-woot-id sentinel-space))
        
        (check-equal? (op:insert-after-dstx (second new-cmds-2))
                      world-atom)
        (check-equal? (op:insert-after-id (second new-cmds-2))
                      (dstx-woot-id hello-atom)))))))