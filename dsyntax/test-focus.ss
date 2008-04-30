(module test-focus mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2 8))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2 8))
           "cursor.ss"
           "struct.ss")
  
  (provide test-focus)
  
  ;; with-cursor-anywhere: cursor (-> void) -> void
  ;; evaluate the thunk everywhere.  Lets us exhaustively check the 
  ;; pos-focusing functions
  (define (with-cursor-anywhere a-cursor thunk) 
    (let loop ([a-cursor (focus-toplevel a-cursor)])
      (cond
        [a-cursor
         (thunk a-cursor)
         (loop (focus-successor/no-snap a-cursor))]
        [else
         (void)])))
  
  
  
  (define test-focus
    (test-suite
     "test-focus.ss"
     (test-case
      "focus-toplevel"
      (let ([a-cursor (make-toplevel-cursor (list (new-atom "hello") (new-atom "world")))])
        (check-equal? (cursor-dstx (focus-toplevel a-cursor))
                      (new-atom "hello"))
        (check-equal? (cursor-dstx (focus-successor (focus-toplevel a-cursor)))
                      (new-atom "world"))))
     
     
     (test-case
      "focus-pos"
      (let ([a-cursor (make-toplevel-cursor (list (new-atom "foo")
                                                  (new-atom "bar")))])
        (with-cursor-anywhere 
         a-cursor 
         (lambda (a-cursor)
           (check-equal? (cursor-dstx (focus-pos a-cursor 0))
                         (new-atom "foo"))
           (check-equal? (focus-pos a-cursor 1)
                         #f)
           (check-equal? (focus-pos a-cursor 2)
                         #f)
           (check-equal? (cursor-dstx (focus-pos a-cursor 3))
                         (new-atom "bar"))
           (check-equal? (focus-pos a-cursor 4)
                         #f)
           (check-equal? (focus-pos a-cursor 5)
                         #f)
           (check-equal? (focus-pos a-cursor 6)
                         #f)))))
     
     
     (test-case
      "focus-pos on a space at the very beginning."
      (let ([a-cursor (make-toplevel-cursor (list (new-space " ")))])
        (with-cursor-anywhere 
         a-cursor
         (lambda (a-cursor)
           (check-equal? (cursor-dstx (focus-pos a-cursor 0))
                         (new-space " "))
           (check-equal? (focus-pos a-cursor 1)
                         #f)))))
     
     (test-case
      "focus-pos at an ending space"
      (let ([a-cursor (make-toplevel-cursor (list (new-atom "x") (new-space " ")))])
        (with-cursor-anywhere 
         a-cursor
         (lambda (a-cursor)
           (check-equal? (cursor-dstx (focus-pos a-cursor 0))
                         (new-atom "x"))
           (check-equal? (cursor-dstx (focus-pos a-cursor 1))
                         (new-space " "))
           (check-equal? (focus-pos a-cursor 2)
                         #f)
           (check-equal? (focus-pos a-cursor 3)
                         #f)))))
     
     (test-case
      "focus-in and focus-out, when no modifications occur, should preserve fusions."
      (let ([a-cursor (make-toplevel-cursor
                       (list (new-fusion "("
                                         (list (new-atom "hello"))
                                         ")")))])
        (check-eq? (cursor-dstx a-cursor)
                   (cursor-dstx (focus-out (focus-in a-cursor))))))
     
     (test-case
      "focus-in and focus-out should only preserve fusions on eq?"
      (let ([a-cursor (make-toplevel-cursor
                       (list (new-fusion "("
                                         (list (new-special-atom "hello"))
                                         ")")))])
        (check-false (eq? (cursor-dstx a-cursor)
                          (cursor-dstx
                           (focus-out
                            (insert-after
                             (delete (focus-in a-cursor))
                             (new-special-atom "hello"))))))))
     
     
     (test-case
      "focus-pos with structure"
      (let* ([a-dstx (new-fusion "["
                                 (list (new-atom "box"))
                                 "]")]
             [a-cursor (make-toplevel-cursor (list a-dstx))])
        (with-cursor-anywhere 
         a-cursor
         (lambda (a-cursor)           
           (check-equal? (cursor-dstx (focus-pos a-cursor 0))
                         a-dstx)
           (check-equal? (cursor-dstx (focus-pos a-cursor 1))
                         (new-atom "box"))
           (check-equal? (focus-pos a-cursor 2)
                         #f)
           (check-equal? (focus-pos a-cursor 3)
                         #f)
           (check-equal? (focus-pos a-cursor 4)
                         #f)))))
     
     (test-case
      "focus-container on atoms"
      (let* ([a-dstx (new-atom "hi")]
             [a-cursor (make-toplevel-cursor (list a-dstx))])
        (with-cursor-anywhere
         a-cursor
         (lambda (a-cursor)
           (check-equal? (cursor-dstx (focus-container a-cursor 0))
                         (new-atom "hi"))
           (check-equal? (cursor-dstx (focus-container a-cursor 1))
                         (new-atom "hi"))
           (check-equal? (focus-container a-cursor 2)
                         #f)))))
     
     (test-case
      "focus-container when the cursor is not at the beginning"
      (let* ([a-dstx (new-fusion "(" (list (new-atom "x")
                                           (new-space " ")
                                           (new-atom "y"))
                                 ")")]
             [a-cursor (make-toplevel-cursor (list a-dstx))])
        (check-equal? (cursor-dstx (focus-container
                                    (focus-oldest (focus-in a-cursor)) 2))
                      (new-space " "))))
     
     
     (test-case
      "focus-container on fusions"
      (let* ([a-dstx (new-fusion "("
                                 (list (new-atom "bye"))
                                 ")")]
             [a-cursor (make-toplevel-cursor (list a-dstx))])
        (with-cursor-anywhere
         a-cursor
         (lambda (a-cursor)
           (check-equal? (cursor-dstx (focus-container a-cursor 0))
                         (new-fusion "("
                                     (list (new-atom "bye"))
                                     ")"))
           (check-equal? (cursor-dstx (focus-container a-cursor 1))
                         (new-atom "bye"))
           (check-equal? (cursor-dstx (focus-container a-cursor 2))
                         (new-atom "bye"))
           (check-equal? (cursor-dstx (focus-container a-cursor 3))
                         (new-atom "bye"))
           (check-equal? (cursor-dstx (focus-container a-cursor 4))
                         (new-fusion "("
                                     (list (new-atom "bye"))
                                     ")"))
           (check-equal? (focus-container a-cursor 5)
                         #f)))))
     
     
     (test-case
      "focus-container on nested fusions again"
      (let* ([a-dstx (new-fusion
                      "("
                      (list (new-fusion
                             "("
                             (list (new-fusion
                                    "("
                                    (list (new-fusion
                                           "("
                                           (list (new-atom "d"))
                                           ")"))
                                    ")"))
                             ")"))
                      ")")]
             [a-cursor (make-toplevel-cursor (list a-dstx))])
        (with-cursor-anywhere
         a-cursor
         (lambda (a-cursor)
           (check-equal? (cursor-dstx (focus-container a-cursor 0)) a-dstx)
           (check-equal? (cursor-dstx (focus-container a-cursor 1))
                         (cursor-dstx (focus-in (focus-toplevel a-cursor))))
           (check-equal? (cursor-dstx (focus-container a-cursor 2))
                         (cursor-dstx (focus-in (focus-in (focus-toplevel a-cursor)))))
           (check-equal? (cursor-dstx (focus-container a-cursor 3))
                         (cursor-dstx (focus-in (focus-in (focus-in (focus-toplevel a-cursor))))))
           (check-equal? (cursor-dstx (focus-container a-cursor 4))
                         (cursor-dstx (focus-in (focus-in (focus-in (focus-in (focus-toplevel a-cursor)))))))
           (check-equal? (cursor-dstx (focus-container a-cursor 5))
                         (cursor-dstx (focus-in (focus-in (focus-in (focus-toplevel a-cursor))))))
           (check-equal? (cursor-dstx (focus-container a-cursor 6))
                         (cursor-dstx (focus-in (focus-in (focus-toplevel a-cursor)))))
           (check-equal? (cursor-dstx (focus-container a-cursor 7))
                         (cursor-dstx (focus-in (focus-toplevel a-cursor))))
           (check-equal? (cursor-dstx (focus-container a-cursor 8))
                         (cursor-dstx (focus-toplevel a-cursor)))
           (check-equal? (focus-container a-cursor 9)
                         #f)))))
     
     
     (test-case
      "focus-container on nested fusions, when we've got more content at the end."
      (let* ([a-dstx (new-fusion
                      "("
                      (list (new-fusion
                             "("
                             (list (new-fusion
                                    "("
                                    (list (new-fusion
                                           "("
                                           (list (new-atom "d"))
                                           ")"))
                                    ")"))
                             ")"))
                      ")")]
             [a-cursor (focus-oldest (make-toplevel-cursor (list a-dstx (new-atom "last"))))])
        (with-cursor-anywhere
         a-cursor
         (lambda (a-cursor)
           (check-equal? (cursor-dstx (focus-container a-cursor 0)) a-dstx)
           (check-equal? (cursor-dstx (focus-container a-cursor 1))
                         (cursor-dstx (focus-in (focus-toplevel a-cursor))))
           (check-equal? (cursor-dstx (focus-container a-cursor 2))
                         (cursor-dstx (focus-in (focus-in (focus-toplevel a-cursor)))))
           (check-equal? (cursor-dstx (focus-container a-cursor 3))
                         (cursor-dstx (focus-in (focus-in (focus-in (focus-toplevel a-cursor))))))
           (check-equal? (cursor-dstx (focus-container a-cursor 4))
                         (cursor-dstx (focus-in (focus-in (focus-in (focus-in (focus-toplevel a-cursor)))))))
           (check-equal? (cursor-dstx (focus-container a-cursor 5))
                         (cursor-dstx (focus-in (focus-in (focus-in (focus-toplevel a-cursor))))))
           (check-equal? (cursor-dstx (focus-container a-cursor 6))
                         (cursor-dstx (focus-in (focus-in (focus-toplevel a-cursor)))))
           (check-equal? (cursor-dstx (focus-container a-cursor 7))
                         (cursor-dstx (focus-in (focus-toplevel a-cursor))))
           (check-equal? (cursor-dstx (focus-container a-cursor 8))
                         (cursor-dstx (focus-toplevel a-cursor)))
           (check-equal? (cursor-dstx (focus-container a-cursor 9))
                         (new-atom "last"))))))
     
     (test-case
      "focus-endpos"
      (let* ([a-dstx (new-atom "hi")]
             [a-cursor (make-toplevel-cursor (list a-dstx))])
        (with-cursor-anywhere
         a-cursor
         (lambda (a-cursor)
           (check-false (focus-endpos a-cursor 0))
           (check-false (focus-endpos a-cursor 1))
           (check-equal? (cursor-dstx (focus-endpos a-cursor 2))
                         (new-atom "hi"))
           (check-false (focus-endpos a-cursor 3))))))
     
     (test-case
      "focus-endpos returns the most successive."
      (let* ([a-dstx (new-atom "hi")]
             [sentinel-1 (make-space '() "")]
             [sentinel-2 (make-space '() "")]
             [a-cursor (make-toplevel-cursor (list sentinel-1 a-dstx sentinel-2))])
        (with-cursor-anywhere
         a-cursor
         (lambda (a-cursor)
           (check-eq? (cursor-dstx (focus-endpos a-cursor 0))
                      sentinel-1)
           (check-false (focus-endpos a-cursor 1))
           (check-eq? (cursor-dstx (focus-endpos a-cursor 2))
                      sentinel-2)
           (check-false (focus-endpos a-cursor 3))))))))
  
  
  (define (test)
    (test/text-ui test-focus)))