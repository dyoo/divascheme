(module test-focus-2 mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2 8))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2 8))
           (lib "list.ss")
           "dsyntax.ss"
           "parse-plt-scheme.ss")
  
  
  
  (provide test-focus-2)
  
  (define (test)
    (test/text-ui test-focus-2))
  
  (define test-focus-2
    (test-suite
     "test-focus-2.ss"
     (test-case
      "empty case"
      (test-both ""))
     
     (test-case
      "simple atom"
      (test-both "a"))
     
     (test-case
      "simple atom"
      (test-both "alphabet"))
     
     (test-case
      "simple string"
      (test-both "\"hello world\""))
     
     (test-case
      "simple number"
      (test-both "42"))
     
     (test-case
      "factorial"
      (test-both "(define (factorial x) (cond [(= x 0) 1] [else (* x (factorial (sub1 x)))]))"))
     
     ))
  
  
  
  ;; test-both: string -> void
  ;; Drive the test harnesses for the pos and endpos checkers.
  (define (test-both a-str)
    (test-string-pos a-str)
    (test-string-endpos a-str))
  
  
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
  
  ;; Repeat the following thunk f N times.  Passes the iteration number to f.
  (define (do-range N f)
    (let loop ([i 0])
      (cond
        [(< i N)
         (f i)
         (loop (add1 i))]
        [else
         (void)])))
  
  
  ;; test-string-pos: string -> void
  ;; Checks for expected start-pos on the parsed structures.
  (define (test-string-pos a-str)
    (let* ([a-cursor (make-toplevel-cursor (parse-port (open-input-string a-str)))]
           [last-pos (last-position a-cursor)]
           [ht (collect-start-pos a-cursor)])
      (with-cursor-anywhere
       a-cursor
       (lambda (a-cursor)
         (do-range (+ last-pos 2)
                   (lambda (i)
                     (cond
                       [(hash-table-get ht i #f)
                        =>
                        (lambda (expected-dstx)
                          (check-eq? (cursor-dstx (focus-pos a-cursor i)) expected-dstx
                                     (format "pos ~a" i)))]
                       [else
                        (check-false (focus-pos a-cursor i)
                                     (format "pos ~a" i))])))))))
  
  
  
  ;; test-string-endpos: string -> void
  ;; Checks for expected endpos on the parsed structures.
  (define (test-string-endpos a-str)
    (let* ([a-cursor (make-toplevel-cursor (parse-port (open-input-string a-str)))]
           [last-pos (last-position a-cursor)]
           [ht (collect-endpos a-cursor)])
      (with-cursor-anywhere
       a-cursor
       (lambda (a-cursor)
         (do-range (+ last-pos 2)
                   (lambda (i)
                     (cond
                       [(hash-table-get ht i #f)
                        =>
                        (lambda (expected-dstx)
                          (check-eq? (cursor-dstx (focus-endpos a-cursor i)) expected-dstx
                                     (format "endpos ~a" i)))]
                       [else
                        (check-false (focus-endpos a-cursor i)
                                     (format "endpos ~a" i))])))))))
  
  
  
  
  #;(define (test-string-endpos ip)
      (let ([a-cursor (make-toplevel-cursor (parse-port ip))])
        (void)))
  
  
  
  
  
  ;; last-position: cursor -> natural-number
  ;; Returns the last position.
  (define (last-position a-cursor)
    (let loop ([a-cursor (focus-toplevel a-cursor)])
      (cond
        [(focus-successor/no-snap a-cursor)
         (max (cursor-endpos a-cursor)
              (loop (focus-successor/no-snap a-cursor)))]
        [else
         (cursor-endpos a-cursor)])))
  
  
  (define (focus-last-successor a-cursor)
    (let loop ([a-cursor a-cursor])
      (cond
        [(focus-successor/no-snap a-cursor)
         (loop (focus-successor/no-snap a-cursor))]
        [else
         a-cursor])))
  
  
  
  ;; collect-start-pos: cursor -> (hash-table-of (list number dstx))
  ;; Returns a list of start-pos/dstx lists.
  (define (collect-start-pos a-cursor)
    (define ht (make-hash-table 'equal))
    (let loop ([a-cursor (focus-toplevel a-cursor)])
      (cond
        [(not a-cursor)
         (void)]
        [else
         (hash-table-put! ht (cursor-pos a-cursor) (cursor-dstx a-cursor))
         (loop (focus-successor/no-snap a-cursor))]))
    ht)
  
  
  ;; collect-endpos: cursor -> (hash-table-of (list number dstx))
  ;; Returns a list of end-pos/dstx lists.
  (define (collect-endpos a-cursor)
    (define ht (make-hash-table 'equal))
    (let loop ([a-cursor (focus-last-successor a-cursor)])
      (cond
        [(not a-cursor)
         (void)]
        [else
         (cons
          (hash-table-put! ht (cursor-endpos a-cursor)
                           (cursor-dstx a-cursor))
          (loop (focus-predecessor/no-snap a-cursor)))]))
    ht))