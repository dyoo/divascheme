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
  
  
  (define host-id "test")
  
  (define (decorate a-dstx)
    (deep-attach-woot-ids a-dstx host-id))
  
  (define initial-cursor
    (let ([a-cursor (make-toplevel-cursor (list))])
      (replace a-cursor (dstx-set-woot-id (cursor-dstx a-cursor)
                                          first-sentinel-woot-id))))
  
  (define my-sentinel-space
    (cursor-dstx initial-cursor))
  
  
  (define test-mock-woot
    (test-suite
     "test-mock-woot.ss"
     
     (test-case
      "construction"
      (mock:new-mock-woot initial-cursor))
     
     
     (test-case
      "inserting an atom into empty buffer."
      (let* ([woot (mock:new-mock-woot initial-cursor)]
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
      "inserting a few atoms into empty buffer."
      (let* ([woot (mock:new-mock-woot initial-cursor)]
             [a-atom (decorate (new-atom "a"))]
             [c-atom (decorate (new-atom "c"))]
             [d-atom (decorate (new-atom "d"))]
             [b-atom (decorate (new-atom "b"))]
             [new-ops-1
              (mock:consume-msg! woot
                                 (make-msg:insert
                                  host-id
                                  a-atom
                                  first-sentinel-woot-id #f))]
             [new-ops-2
              (mock:consume-msg! woot
                                 (make-msg:insert
                                  host-id
                                  c-atom
                                  (dstx-woot-id a-atom) #f))]
             [new-ops-3
              (mock:consume-msg! woot
                                 (make-msg:insert
                                  host-id
                                  d-atom
                                  (dstx-woot-id c-atom) #f))]
             [new-ops-4
              (mock:consume-msg! woot
                                 (make-msg:insert
                                  host-id
                                  b-atom
                                  (dstx-woot-id a-atom)
                                  (dstx-woot-id c-atom)))])
        (check-equal? (length new-ops-1) 1)
        (check-equal? (length new-ops-2) 1)
        (check-equal? (length new-ops-3) 1)
        (check-equal? (length new-ops-4) 1)
        (let ([first-op (first new-ops-1)])
          (check-true (op:insert-after? first-op))
          (check-equal? (op:insert-after-id first-op) first-sentinel-woot-id)
          (check-eq? (op:insert-after-dstx first-op) a-atom))
        
        (let ([first-op (first new-ops-2)])
          (check-true (op:insert-after? first-op))
          (check-equal? (op:insert-after-id first-op) (dstx-woot-id a-atom))
          (check-eq? (op:insert-after-dstx first-op) c-atom))
        
        (let ([first-op (first new-ops-3)])
          (check-true (op:insert-after? first-op))
          (check-equal? (op:insert-after-id first-op) (dstx-woot-id c-atom))
          (check-eq? (op:insert-after-dstx first-op) d-atom))
        
        (let ([first-op (first new-ops-4)])
          (check-true (op:insert-after? first-op))
          (check-equal? (op:insert-after-id first-op) (dstx-woot-id a-atom))
          (check-eq? (op:insert-after-dstx first-op) b-atom))))
     
     
     
     (test-case
      "inserting without satisfying precondition should be empty"
      (let* ([woot (mock:new-mock-woot initial-cursor)]
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
      (let* ([woot (mock:new-mock-woot initial-cursor)]
             [world-atom (decorate (new-atom "world"))]
             [hello-atom (decorate (new-atom "hello"))]
             [new-cmds-1
              (mock:consume-msg! woot
                                 (make-msg:insert host-id world-atom (dstx-woot-id hello-atom) #f))]
             [new-cmds-2
              (mock:consume-msg! woot
                                 (make-msg:insert host-id hello-atom (dstx-woot-id my-sentinel-space) #f))])
        (check-equal? new-cmds-1 '())
        (check-equal? (length new-cmds-2) 2)
        (check-equal? (op:insert-after-dstx (first new-cmds-2))
                      hello-atom)
        (check-equal? (op:insert-after-id (first new-cmds-2))
                      (dstx-woot-id my-sentinel-space))
        
        (check-equal? (op:insert-after-dstx (second new-cmds-2))
                      world-atom)
        (check-equal? (op:insert-after-id (second new-cmds-2))
                      (dstx-woot-id hello-atom))))
     
     
     
     (test-case
      "deleting bar from 'foo bar baz'"
      (let* ([foo (decorate (new-atom "foo"))]
             [bar (decorate (new-atom "bar"))]
             [baz (decorate (new-atom "baz"))]
             [a-cursor (insert-after
                        (insert-after (insert-after initial-cursor foo)
                                      bar)
                        baz)]
             [woot (mock:new-mock-woot a-cursor)]
             [new-cmds
              (mock:consume-msg! woot
                                 (make-msg:delete host-id (dstx-woot-id bar)))])
        (check-equal? (length new-cmds) 1)
        (check-equal? (op:delete-id (first new-cmds)) (dstx-woot-id bar))
        (check-equal? (mock:visible-before-or-at woot (dstx-woot-id bar))
                      (dstx-woot-id foo))))
     
     
     
     (test-case
      "deleting bar from 'foo bar baz', then adding it to the end"
      (let* ([foo (decorate (new-atom "foo"))]
             [bar (decorate (new-atom "bar"))]
             [baz (decorate (new-atom "baz"))]
             [a-cursor (insert-after
                        (insert-after (insert-after initial-cursor foo)
                                      bar)
                        baz)]
             [woot (mock:new-mock-woot a-cursor)]
             [new-cmds-1
              (mock:consume-msg! woot
                                 (make-msg:delete host-id (dstx-woot-id bar)))]
             [new-cmds-2
              (mock:consume-msg! woot
                                 (make-msg:insert host-id bar (dstx-woot-id baz) #f))])
        (check-equal? (length new-cmds-1) 1)
        (check-equal? (op:delete-id (first new-cmds-1)) (dstx-woot-id bar))
        (check-equal? (mock:visible-before-or-at woot (dstx-woot-id bar))
                      (dstx-woot-id bar))
        
        (check-equal? (length new-cmds-2) 1)
        (check-equal? (op:insert-after-dstx (first new-cmds-2)) bar)
        (check-equal? (op:insert-after-id (first new-cmds-2)) (dstx-woot-id baz))))
     
     
     
     (test-case
      "deleting b from  'a b c', then adding it again it to the middle"
      (let* ([a (decorate (new-atom "a"))]
             [b (decorate (new-atom "b"))]
             [c (decorate (new-atom "c"))]
             [b-new (decorate (new-atom "b"))]
             [a-cursor (insert-after
                        (insert-after (insert-after initial-cursor a)
                                      b)
                        c)]
             [woot (mock:new-mock-woot a-cursor)]
             [new-cmds-1
              (mock:consume-msg! woot
                                 (make-msg:delete host-id (dstx-woot-id b)))]
             [new-cmds-2
              (mock:consume-msg! woot
                                 (make-msg:insert host-id
                                                  b-new
                                                  (dstx-woot-id a)
                                                  (dstx-woot-id c)))])
        (check-equal? (length new-cmds-1) 1)
        (check-equal? (op:delete-id (first new-cmds-1)) (dstx-woot-id b))
        
        (check-equal? (length new-cmds-2) 1)
        (check-equal? (op:insert-after-dstx (first new-cmds-2)) b-new)
        (check-equal? (op:insert-after-id (first new-cmds-2)) (dstx-woot-id a))))
     )))