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
      (let ([tqueue (new-topological-queue)])
        (add! tqueue 'a '())
        (check-eq? (get tqueue) 'a)
        (add! tqueue 'b '(a))
        (check-false (try-get tqueue))
        (add! tqueue 'c '(b))
        (check-false (try-get tqueue))
        (satisfy! tqueue 'a)
        
        (check-eq? (try-get tqueue) 'b)
        (satisfy! tqueue 'b)
        (check-eq? (try-get tqueue) 'c)))
     
     
     (test-case
      "'b' depends on 'a' and 'c'"
      (let ([tqueue (new-topological-queue)])
        (add! tqueue 'a '())
        (add! tqueue 'c '())
        (add! tqueue 'b '(a c))
        
        (check-eq? (get tqueue) 'a)
        (check-eq? (get tqueue) 'c)
        (check-false (try-get tqueue))
        (satisfy! tqueue 'a)
        (check-false (try-get tqueue))
        (satisfy! tqueue 'c)
        (check-eq? (get tqueue) 'b)))
     
     
     (test-case
      "'b' depends on 'a' and 'c' again"
      (let ([tqueue (new-topological-queue)])
        (satisfy! tqueue 'a)
        (satisfy! tqueue 'c)
        (add! tqueue 'b '(a c))
        (check-eq? (get tqueue) 'b))))))