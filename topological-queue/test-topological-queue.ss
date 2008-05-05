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
        (check-eq? (try-get tqueue) 'c))))))