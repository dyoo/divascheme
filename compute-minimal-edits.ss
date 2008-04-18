(module compute-minimal-edits mzscheme
  (require (lib "contract.ss")
           (lib "etc.ss")
           (lib "list.ss"))
  
  #|
Computes minimal edits, according to Levenstein edit distance.
The only operations we allow are inserts and deletes.

Base cases:

    change(X, "") = delete all of X
    change("", Y) = insert all of Y

Inductive case:

    change(X:x, Y:y) = 
      choose the minimum of the following choices:
        * change(X, Y) 
          (only if x = y)

        * change(X:x, Y) + insert y

        * change(X, Y:y) + delete x
|#
  
  
  
  (define-struct edit () #f)
  (define-struct (edit:insert edit) (offset elements) #f)
  (define-struct (edit:delete edit) (offset len) #f)
  
  (define-struct edit:sequence (cost edits))
  
  ;; Returns the minimum edit sequence.
  (define (min-edits first-edit-seqs . rest-edit-seqs)
    (foldl (lambda (x acc)
             (if (< (edit:sequence-cost x)
                    (edit:sequence-cost acc))
                 x
                 acc))
           first-edit-seqs
           rest-edit-seqs))
  
  
  ;; Add an edit in front.
  (define (edit-prepend an-edit a-sequence)
    (make-edit:sequence (add1 (edit:sequence-cost a-sequence))
                        (cons an-edit (edit:sequence-edits a-sequence))))
  
  
  
  ;; compute-minimal-edits: (vectorof X) (vectorof X) (X X -> boolean) -> edit
  ;; compute the minimum edits necessary to change vector 'from' to vector 'to'
  (define (compute-minimal-edits from to =?)
    (define cache (new-table (add1 (vector-length from)) (add1 (vector-length to))))
    
    ;; change all the elements
    ;; from[0, i) -> to[0, j)
    (define (change i j)
      (cond [(table-ref cache i j)
             => identity]
            [else
             (let ([result
                    (cond [(and (= i 0) (= j 0))
                           (make-edit:sequence 0 (list))]
                          
                          [(= i 0)
                           (make-edit:sequence j (list (make-edit:insert 0
                                                                         (build-list
                                                                          j
                                                                          (lambda (j)
                                                                            (vector-ref to j))))))]
                          [(= j 0)
                           (make-edit:sequence i (list (make-edit:delete 0 i)))]
                          
                          [else
                           (min-edits
                            
                            ;; replace (or equality)
                            (if (=? (vector-ref from (sub1 i))
                                    (vector-ref to (sub1 j)))
                                (change (sub1 i) (sub1 j))
                                (edit-prepend (make-edit:delete (sub1 i) 1)
                                              (edit-prepend
                                               (make-edit:insert (sub1 i) (list (vector-ref to (sub1 j))))
                                               (change (sub1 i) (sub1 j)))))
                            
                            ;; Insert
                            (edit-prepend (make-edit:insert i (list (vector-ref to (sub1 j))))
                                          (change i (sub1 j)))
                            
                            ;; Delete
                            (edit-prepend (make-edit:delete (sub1 i) 1)
                                          (change (sub1 i) j)))])])
               (table-set! cache i j result)
               result)]))
    
    (edit:sequence-edits
     (change (vector-length from)
             (vector-length to))))
  
  
  
  (define-struct table (data m n) #f)
  
  (define (new-table m n)
    (make-table (make-vector (* m n) #f) m n))
  
  (define (table-ref a-table i j)
    (vector-ref (table-data a-table) (+ (* i (table-n a-table)) j)))
  
  (define (table-set! a-table i j v)
    (vector-set! (table-data a-table) (+ (* i (table-n a-table)) j) v))
  
  
  
  (provide/contract [struct edit ()]
                    [struct (edit:insert edit) ([offset natural-number/c]
                                                [elements (listof any/c)])]
                    [struct (edit:delete edit) ([offset natural-number/c]
                                                [len natural-number/c])]
                    [compute-minimal-edits (vector? vector? (any/c any/c . -> . boolean?)
                                                    . -> . (listof edit?))]))
