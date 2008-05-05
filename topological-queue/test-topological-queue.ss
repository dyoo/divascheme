(module test-topological-queue mzscheme
  (require "topological-queue.ss"
           (planet "test.ss" ("schematics" "schemeunit.plt" 2 8))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2 8)))
  
  (provide test-topological-queue)
  
  (define (test)
    (test/text-ui test-topological-queue))
  
  (define test-topological-queue
    (test-suite
     "test-topological-queue.ss"
     (test-case
      "c depends on b, b depends on a"
      (let ([tqueue (new-tqueue)])
        (tqueue-add! tqueue 'a '())
        (check-eq? (tqueue-get tqueue) 'a)
        (tqueue-add! tqueue 'b '(a))
        (check-false (tqueue-try-get tqueue))
        (tqueue-add! tqueue 'c '(b))
        (check-false (tqueue-try-get tqueue))
        (tqueue-satisfy! tqueue 'a)
        
        (check-eq? (tqueue-try-get tqueue) 'b)
        (tqueue-satisfy! tqueue 'b)
        (check-eq? (tqueue-try-get tqueue) 'c)))
     
     
     (test-case
      "'b' depends on 'a' and 'c'"
      (let ([tqueue (new-tqueue)])
        (tqueue-add! tqueue 'a '())
        (tqueue-add! tqueue 'c '())
        (tqueue-add! tqueue 'b '(a c))
        
        (check-eq? (tqueue-get tqueue) 'a)
        (check-eq? (tqueue-get tqueue) 'c)
        (check-false (tqueue-try-get tqueue))
        (tqueue-satisfy! tqueue 'a)
        (check-false (tqueue-try-get tqueue))
        (tqueue-satisfy! tqueue 'c)
        (check-eq? (tqueue-get tqueue) 'b)))
     
     
     (test-case
      "'b' depends on 'a' and 'c' again"
      (let ([tqueue (new-tqueue)])
        (tqueue-satisfy! tqueue 'a)
        (tqueue-satisfy! tqueue 'c)
        (tqueue-add! tqueue 'b '(a c))
        (check-eq? (tqueue-get tqueue) 'b))))))