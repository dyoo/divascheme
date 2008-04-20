(module test-dstx-text-mixin mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "plt-match.ss")
           (lib "list.ss")
           (planet "test.ss" ("schematics" "schemeunit.plt" 2 8))
           (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2 8))
           "struct.ss"
           "cursor.ss"
           "dstx-text-mixin.ss")
  
  
  (provide test-dstx-text-mixin)
  
  (define (test)
    (test/text-ui test-dstx-text-mixin))
  
  (define (make-text-instance)
    (let ([instance (new (dstx-text-mixin text%))])
      (send instance enable-dstx-parsing)
      instance))
  
  
  ;; Remove local ids from a dstx, just for testing purposes.
  (define (strip-local-ids a-dstx)
    (dstx-deepmap (lambda (a-dstx)
                    (dstx-property-strip a-dstx))
                  a-dstx))
  
  
  
  
  (define (dstx-property-strip a-dstx)
    (match a-dstx
      [(struct atom (_ content))
       (new-atom content)]
      [(struct special-atom (_ content width))
       (new-special-atom content width)]
      [(struct space (_ content))
       (new-space content)]
      [(struct fusion (_ prefix children suffix))
       (new-fusion prefix children suffix)]))
  
  
  (define test-dstx-text-mixin
    (test-suite
     "test-dstx-text-mixin.ss"
     
     (test-case
      "inserting a single element"
      (let* ([text (make-text-instance)]
             [cursor (send text get-dstx-cursor)])
        (send cursor insert-after! (new-atom "hello"))
        ;; Check what's on screen...
        (check-equal? (send text get-text) "hello")
        ;; As well as what's in the dstx
        (check-equal? (map strip-local-ids (send text get-toplevel-dstxs))
                      (map strip-local-ids (list (new-space "") (new-atom "hello"))))
        (check-true (number? (send cursor property-ref 'local-id)))))
     
     
     (test-case
      "deleting a single element"
      (let* ([text (make-text-instance)]
             [cursor (send text get-dstx-cursor)])
        (send cursor insert-after! (new-atom "hello"))
        (send cursor delete!)
        (check-equal? (send text get-text) "")
        (check-equal? (map strip-local-ids (send text get-toplevel-dstxs))
                      (map strip-local-ids (list (new-space ""))))))
     
     
     (test-case
      "deleting between two"
      (let* ([text (make-text-instance)]
             [cursor (send text get-dstx-cursor)])
        (send cursor insert-after! (new-atom "A"))
        (send cursor insert-after! (new-atom "B"))
        (send cursor insert-after! (new-atom "C"))
        (send cursor focus-younger!)
        (send cursor delete!)
        (check-equal? (send text get-text) "AC")
        (check-equal? (strip-local-ids (send cursor cursor-dstx))
                      (strip-local-ids (new-atom "C")))))
     
     
     (test-case
      "inserting two elements with insert-after"
      (let* ([text (make-text-instance)]
             [cursor (send text get-dstx-cursor)])
        (send cursor insert-after! (new-atom "hello"))
        (send cursor insert-after! (new-space " "))
        (send cursor insert-after! (new-atom "world"))
        ;; Check what's on screen...
        (check-equal? (send text get-text) "hello world")
        ;; As well as what's in the dstx
        (check-equal? (map strip-local-ids (send text get-toplevel-dstxs))
                      (map strip-local-ids (list (new-space "")
                                                 (new-atom "hello")
                                                 (new-space " ")
                                                 (new-atom "world"))))
        (check-true (number? (send cursor property-ref 'local-id)))
        (check-equal? (strip-local-ids (send cursor cursor-dstx))
                      (strip-local-ids (new-atom "world")))
        (send cursor focus-predecessor!)
        (check-true (number? (send cursor property-ref 'local-id)))
        (check-equal? (strip-local-ids (send cursor cursor-dstx))
                      (strip-local-ids (new-atom "hello")))))
     
     
     
     (test-case
      "inserting two elements with insert-before"
      (let* ([text (make-text-instance)]
             [cursor (send text get-dstx-cursor)])
        (send cursor insert-after! (new-atom "world"))
        (send cursor insert-before! (new-space " "))
        (send cursor insert-before! (new-atom "goodbye"))
        ;; Check what's on screen...
        (check-equal? (send text get-text) "goodbye world")
        ;; As well as what's in the dstx
        (check-equal? (map strip-local-ids (send text get-toplevel-dstxs))
                      (map strip-local-ids (list (new-space "")
                                                 (new-atom "goodbye")
                                                 (new-space " ")
                                                 (new-atom "world"))))))
     
     (test-case
      "manually editing a symbol at the front"
      (let* ([text (make-text-instance)]
             [cursor (send text get-dstx-cursor)])
        (send cursor insert-after! (new-atom "orld"))
        (send text insert "w" 0)
        
        ;; Check what's on screen...
        (check-equal? (send text get-text) "world")
        ;; As well as what's in the dstx
        (check-equal? (map strip-local-ids (send text get-toplevel-dstxs))
                      (map strip-local-ids (list (new-space "") (new-atom "world"))))))
     
     
     (test-case
      "manually editing a symbol at the back"
      (let* ([text (make-text-instance)]
             [cursor (send text get-dstx-cursor)])
        (send cursor insert-after! (new-atom "worl"))
        (send cursor insert-after! (new-space " "))
        (send cursor insert-after! (new-atom "peace"))
        (let-values ([(id0 id1 id2 id3)
                      (apply values (map (lambda (a-dstx) (dstx-property-ref a-dstx 'local-id))
                                         (send cursor cursor-toplevel-dstxs)))])
          (send text insert "d" 4)
          
          ;; Check what's on screen...
          (check-equal? (send text get-text) "world peace")
          ;; As well as what's in the dstx
          (check-equal? (map strip-local-ids (send text get-toplevel-dstxs))
                        (map strip-local-ids (list (new-space "")
                                                   (new-atom "world")
                                                   (new-space " ")
                                                   (new-atom "peace"))))
          (let-values ([(new-id0 new-id1 new-id2 new-id3)
                        (apply values (map (lambda (a-dstx) (dstx-property-ref a-dstx 'local-id))
                                           (send cursor cursor-toplevel-dstxs)))])
            (check-equal? id0 new-id0)
            (check-false (equal? id1 new-id1))
            (check-equal? id2 new-id2)
            (check-equal? id3 new-id3)))))
     
     
     (test-case
      "manually editing a symbol at the back"
      (let* ([text (make-text-instance)]
             [cursor (send text get-dstx-cursor)])
        (send cursor insert-after! (new-atom "worl"))
        (send text insert "d" 4)
        ;; Check what's on screen...
        (check-equal? (send text get-text) "world")
        ;; As well as what's in the dstx
        (check-equal? (map strip-local-ids (send text get-toplevel-dstxs))
                      (map strip-local-ids (list (new-space "")
                                                 (new-atom "world"))))))
     
     
     (test-case
      "manually editing a symbol internally"
      (let* ([text (make-text-instance)]
             [cursor (send text get-dstx-cursor)])
        (send cursor insert-after! (new-atom "wrld"))
        (send text set-position 1)
        (send text insert "o")
        
        ;; Check what's on screen...
        (check-equal? (send text get-text) "world")
        (check-equal? (send text get-start-position) 2)
        (check-equal? (send text get-end-position) 2)
        ;; As well as what's in the dstx
        (check-equal? (map strip-local-ids (send text get-toplevel-dstxs))
                      (map strip-local-ids (list (new-space "") (new-atom "world"))))))
     
     
     
     
     
     
     (test-case
      "inserting a fusion"
      (let* ([text (make-text-instance)]
             [cursor (send text get-dstx-cursor)])
        (send cursor insert-after! (new-fusion "("
                                                     (list (new-atom "hello"))
                                                     ")"))
        ;; Check what's on screen...
        (check-equal? (send text get-text) "(hello)")
        ;; As well as what's in the dstx
        (check-equal? (map strip-local-ids (send text get-toplevel-dstxs))
                      (map strip-local-ids (list (new-space "") (new-fusion "(" (list (new-atom "hello")) ")"))))))
     
     
     (test-case
      "inserting inside a fusion"
      (let* ([text (make-text-instance)]
             [cursor (send text get-dstx-cursor)])
        (send cursor insert-after! (new-fusion "("
                                                     (list (new-atom "hello"))
                                                     ")"))
        (send cursor focus-in!)
        (send cursor insert-after! (new-space " "))
        (send cursor insert-after! (new-fusion "["
                                                     (list (new-atom "world"))
                                                     "]"))
        ;; Check what's on screen...
        (check-equal? (send text get-text) "(hello [world])")
        ;; As well as what's in the dstx
        (check-equal? (map strip-local-ids
                           (send text get-toplevel-dstxs))
                      (map strip-local-ids
                           (list (new-space "")
                                 (new-fusion "("
                                             (list (new-atom "hello")
                                                   (new-space " ")
                                                   (new-fusion "["
                                                               (list (new-atom "world"))
                                                               "]"))
                                             ")"))))))
     
     (test-case
      "inserting nonsense becomes a special atom"
      (let* ([text (make-text-instance)])
        (send text insert "(foo!")
        ;; Check what's on screen...
        (check-equal? (send text get-text) "(foo!")
        (check-equal? (length (send text get-toplevel-dstxs)) 2)
        (let* ([should-be-special (second (send text get-toplevel-dstxs))]
               [snip (special-atom-content should-be-special)])
          (check-true (is-a? snip string-snip%))
          (check-equal? (send snip get-text 0 (send snip get-count))
                        "(foo!"))))
     
     (test-case
      "manually adding a string"
      (let* ([text (make-text-instance)])
        (send text insert "\"")
        (check-equal? (send text get-text) "\"")
        (send text insert "h")
        (check-equal? (send text get-text) "\"h")
        (send text insert "i")
        (check-equal? (send text get-text) "\"hi")
        (send text insert "\"")
        (check-equal? (send text get-text) "\"hi\"")))
     
     
     (test-case
      "manually editing the front of an atom with delete"
      (let* ([text (make-text-instance)])
        (let ([cursor (send text get-dstx-cursor)])
          (send cursor insert-after! (new-atom "an-atom")))
        (send text delete 0 3)
        (check-equal? (send text get-text) "atom")
        (check-equal? (map strip-local-ids (send text get-toplevel-dstxs))
                      (map strip-local-ids (list (new-space "")
                                                 (new-atom "atom"))))))
     
     
     (test-case
      "manually editing the end of an atom with delete"
      (let* ([text (make-text-instance)])
        (let ([cursor (send text get-dstx-cursor)])
          (send cursor insert-after! (new-atom "an-at"))
          (send cursor insert-after! (new-atom "om-bomb")))
        (send text delete 7 12)
        (check-equal? (send text get-text) "an-atom")
        (check-equal? (map strip-local-ids (send text get-toplevel-dstxs))
                      (map strip-local-ids (list (new-space "")
                                                 (new-atom "an-at")
                                                 (new-atom "om"))))))
     
     (test-case
      "simple parsing"
      (let ([text (make-text-instance)])
        (send text insert "(module  foo mzscheme)")
        (check-equal? (map strip-local-ids (send text get-toplevel-dstxs))
                      (map strip-local-ids
                           (list
                            (new-space "")
                            (new-fusion
                             "("
                             (list (new-atom "module")
                                   (new-space " ")
                                   (new-space " ")
                                   (new-atom "foo")
                                   (new-space " ")
                                   (new-atom "mzscheme"))
                             ")"))))))
     
     
     (test-case
      "deleting a left paren destructures the elements, leaving behind a special unparsed atom."
      (let ([text (make-text-instance)])
        (send text insert "(x y z)")
        (send text delete 0 1)
        (check-equal? (send text get-text) "x y z)")
        (check-equal? (length (send text get-toplevel-dstxs)) 2)
        (check-true (special-atom? (second (send text get-toplevel-dstxs))))
        (check-true (dstx-property-ref (second (send text get-toplevel-dstxs)) 'unparsed))))
     
     
     (test-case
      "manually deleting a right paren destructures the elements."
      (let ([text (make-text-instance)])
        (send text insert "(x y z)")
        (send text delete 6 7)
        (check-equal? (send text get-text) "(x y z")
        (check-equal? (length (send text get-toplevel-dstxs)) 2)
        (check-true (special-atom? (second (send text get-toplevel-dstxs))))))
     
     
     (test-case
      "manually deleting a right paren destructures the elements."
      (let ([text (make-text-instance)])
        (send text insert "(x)  y z")
        (send text delete 2 3)
        (check-equal? (send text get-text) "(x  y z")
        (check-equal? (length (send text get-toplevel-dstxs)) 7)
        (check-true (special-atom? (second (send text get-toplevel-dstxs))))
        (check-equal? (map strip-local-ids (rest (rest (send text get-toplevel-dstxs))))
                      (map strip-local-ids (list
                                            (new-space " ")
                                            (new-space " ")
                                            (new-atom "y")
                                            (new-space " ")
                                            (new-atom "z"))))))
     
     
     (test-case
      "manually deleting an element and a right paren destructures the elements."
      (let ([text (make-text-instance)])
        (send text insert "(x y z)")
        (send text delete 4 7)
        (check-equal? (send text get-text) "(x y")
        (check-equal? (length (send text get-toplevel-dstxs)) 2)))
     
     
     (test-case
      "manually inserting a close paren should be a special."
      (let ([text (make-text-instance)])
        (send text insert ")")
        (check-equal? (send text get-text) ")")
        (check-equal? (length (send text get-toplevel-dstxs)) 2)
        (check-true (special-atom? (second (send text get-toplevel-dstxs))))))
     
     
     (test-case
      "focus-pos"
      (let ([text (make-text-instance)])
        (send text insert "(module  foo mzscheme)")
        (let ([cursor (send text get-dstx-cursor)])
          (send cursor focus-pos! 7)
          (check-equal? (strip-local-ids (send cursor cursor-dstx))
                        (strip-local-ids (new-space " ")))
          (send cursor focus-younger/no-snap!)
          (check-equal? (strip-local-ids (send cursor cursor-dstx))
                        (strip-local-ids (new-atom "module"))))))
     
     
     (test-case
      "simple insertion of two atoms separated by space"
      (let ([text (make-text-instance)])
        (send text insert "hello")
        (check-equal? (map strip-local-ids (send text get-toplevel-dstxs))
                      (map strip-local-ids (list (new-space "") (new-atom "hello"))))
        (send text insert " ")
        (check-equal? (map strip-local-ids (send text get-toplevel-dstxs))
                      (map strip-local-ids (list (new-space "")
                                                 (new-atom "hello")
                                                 (new-space " "))))
        (send text insert "world")
        (check-equal? (map strip-local-ids (send text get-toplevel-dstxs))
                      (map strip-local-ids (list (new-space "")
                                                 (new-atom "hello")
                                                 (new-space " ")
                                                 (new-atom "world"))))))
     
     (test-case
      "simple insertion of two atoms separated by space"
      (let ([text (make-text-instance)])
        (send text delete 0 (send text last-position))
        (send text insert "[hello]")
        (check-equal? (send text get-text) "[hello]")
        (send text insert "[")
        (check-equal? (send text get-text) "[hello][")
        (send text insert "]")
        (check-equal? (send text get-text)
                      "[hello][]")))
     
     
     (test-case
      "inserting in front of an empty fusion shouldn't affect the fusion's id"
      (let ([text (make-text-instance)])
        (send text insert "()")
        (let* ([cursor (send text get-dstx-cursor)]
               [_ (send cursor focus-container! 0)]
               [old-id (send cursor property-ref 'local-id)])
          (send text insert " " 0)
          (check-equal? (send text get-text) " ()")
          (send cursor focus-container! 1)
          (check-equal? (send cursor property-ref 'local-id)
                        old-id))))
     
     (test-case
      "inserting in front of a nonempty fusion shouldn't affect the fusion's id"
      (let ([text (make-text-instance)])
        (send text insert "[$expr$]")
        (let* ([cursor (send text get-dstx-cursor)]
               [_ (send cursor focus-container! 0)]
               [old-id (send cursor property-ref 'local-id)])
          (send text insert " " 0)
          (check-equal? (send text get-text) " [$expr$]")
          (send cursor focus-container! 1)
          (check-equal? (strip-local-ids (send cursor cursor-dstx))
                        (strip-local-ids (new-fusion "[" (list (new-atom "$expr$")) "]")))
          (check-equal? (send cursor property-ref 'local-id)
                        old-id))))
     
     (test-case
      "deleting everything should get us back to the base state"
      (let ([text (make-text-instance)])
        (send text insert "(module  foo mzscheme)")
        (send text delete 0 (send text last-position))
        (check-equal? (map strip-local-ids (send text get-toplevel-dstxs))
                      (map strip-local-ids (list (new-space ""))))
        (send text insert "hello")
        (send text insert " ")
        (send text insert "world")
        (check-equal? (map strip-local-ids (send text get-toplevel-dstxs))
                      (map strip-local-ids (list (new-space "")
                                                 (new-atom "hello")
                                                 (new-space " ")
                                                 (new-atom "world"))))))
     
     (test-case
      "manually inserting into a fusion"
      (let ([text (make-text-instance)])
        (send text insert "(module foo mzscheme)")
        (send text insert "\n  (define x 42)" 20)
        (check-equal? (send text get-text) "(module foo mzscheme\n  (define x 42))")
        (check-equal? (map strip-local-ids (send text get-toplevel-dstxs))
                      (map strip-local-ids
                           (list
                            (new-space "")
                            (new-fusion "(" (list (new-atom "module")
                                                  (new-space " ")
                                                  (new-atom "foo")
                                                  (new-space " ")
                                                  (new-atom "mzscheme")
                                                  (new-space "\n")
                                                  (new-space " ")
                                                  (new-space " ")
                                                  (new-fusion "(" (list (new-atom "define")
                                                                        (new-space " ")
                                                                        (new-atom "x")
                                                                        (new-space " ")
                                                                        (new-atom "42"))
                                                              ")"))
                                        ")"
                                        ))))))
     
     (test-case
      "manually deleting some spaces in a fusion"
      (let* ([text (make-text-instance)])
        (send text insert "(module  foo mzscheme)")
        (let* ([f-cursor (send (send text get-dstx-cursor) get-functional-cursor)])
          (let ([id1 (property-ref
                      (focus-container f-cursor 1)
                      'local-id)]
                [id2 (property-ref
                      (focus-older (focus-container f-cursor 1))
                      'local-id)]
                [id3 (property-ref
                      (focus-older
                       (focus-older
                        (focus-container f-cursor 1)))
                      'local-id)])
            (send text delete 7 8)
            (check-equal? (send text get-text) "(module foo mzscheme)")
            (check-equal? (map strip-local-ids (send text get-toplevel-dstxs))
                          (map strip-local-ids
                               (list
                                (new-space "")
                                (new-fusion "("
                                            (list (new-atom "module")
                                                  (new-space " ")
                                                  (new-atom "foo")
                                                  (new-space " ")
                                                  (new-atom "mzscheme"))
                                            ")"))))
            (let ([cursor (send text get-dstx-cursor)])
              (send cursor focus-container! 1)
              (check-equal? (send cursor property-ref 'local-id) id1)
              (send cursor focus-older!)
              (check-equal? (send cursor property-ref 'local-id) id2)
              (send cursor focus-older!)
              (check-equal? (send cursor property-ref 'local-id) id3))))))
     
     
     (test-case
      "inserting a newline"
      (let* ([text (make-text-instance)])
        (send text insert "(define (id x)\n  x)")
        (send text insert "\n" 8)
        (send text insert "  " 9)
        (check-equal? (send text get-text)
                      "(define \n  (id x)\n  x)")))
     
     
     (test-case
      "deleting a space, then inserting a newline"
      (let* ([text (make-text-instance)])
        (send text insert "(define (id x)\n  x)")
        (send text delete 7 8)
        (check-equal? (send text get-text)
                      "(define(id x)\n  x)")
        (send text insert "\n" 7)
        (check-equal? (send text get-text)
                      "(define\n(id x)\n  x)")
        (send text insert " " 1)
        (check-equal? (send text get-text)
                      "( define\n(id x)\n  x)")))
     
     (test-case
      "inserting a space in front of an atom doesn't change the atom"
      (let* ([text (make-text-instance)])
        (send text insert "lambda")
        (let-values ([(id0 id1)
                      (apply values
                             (map (lambda (a-dstx)
                                    (dstx-property-ref a-dstx 'local-id))
                                  (send text get-toplevel-dstxs)))])
          (send text insert " " 0)
          (let-values ([(new-id0 new-id1 new-id2)
                        (apply values
                               (map (lambda (a-dstx)
                                      (dstx-property-ref a-dstx 'local-id))
                                    (send text get-toplevel-dstxs)))])
            (check-equal? id0 new-id0)
            (check-equal? id1 new-id2)))))
     
     
     (test-case
      "inserting single characters in an empty fusion should get us a single atom."
      (let* ([text (make-text-instance)]
             [a-cursor (send text get-dstx-cursor)])
        (send text insert "[]")
        (send text insert "a" 1)
        (send text insert "b" 2)
        (send text insert "c" 3)
        (send a-cursor focus-in!)
        (check-equal? (strip-local-ids (send a-cursor cursor-dstx))
                      (strip-local-ids (new-atom "abc")))))
     
     
     (test-case
      "inserting a string piecemeal will eventually parse."
      (let* ([text (make-text-instance)]
             [a-cursor (send text get-dstx-cursor)])
        (send text insert "\"")
        (send text insert "a")
        (send text insert "b")
        (send text insert "c")
        (send text insert "\"")
        (check-equal? (strip-local-ids (send a-cursor cursor-dstx))
                      (strip-local-ids (new-atom "\"abc\""))))))))