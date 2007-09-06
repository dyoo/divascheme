(module actions-test mzscheme
  (require (lib "etc.ss")
           (lib "list.ss")
           (lib "plt-match.ss")
           (lib "class.ss")
           (lib "struct.ss")
           "test-harness.ss"
           "../utilities.ss"
           "../structures.ss"
           "../templates.ss"
           "../actions.ss")
  
  
  
  (define (tests)
    ; actions : actions%
    (define actions (make-object actions%))
    
    ; empty-world : World
    (define empty-world (make-World ""
                                    empty
                                    (index->syntax-pos 0)
                                    #f
                                    0
                                    (index->syntax-pos 0)
                                    0
                                    (lambda () (raise '|Next called|))
                                    (lambda () (raise '|Previous called|))
                                    false
                                    false
                                    false
                                    (lambda () (raise '|Magic called|))
                                    (lambda () (raise '|Pass called|))
                                    false
                                    ""))
    
    ; eval-cmd : datum -> World
    (define (eval-cmd cmd)
      (eval `(send ,actions ,@cmd)))
    
    ; test/cmd-select : (world -> datum) index non-negative-integer string expected -> void
    (define (test/cmd-select make-cmd/select cursor-index selection-length text expected)
      (let* ([world (copy-struct World empty-world
                                 [World-text             text]
                                 [World-syntax-list      (string->syntax-list text)]
                                 [World-cursor-position  (index->syntax-pos cursor-index)]
                                 [World-selection-length selection-length])]
             [world (with-handlers ([voice-exn? (lambda args world)])
                      (eval-cmd (make-cmd/select world)))])
        (test (list [World-cursor-index world]
                    [World-selection-length world]
                    [World-text world])
              expected)))
    
    ; select/pos+len : World pos non-negative-integer -> World
    ; test/select : index non-negative-integer string index non-negative-integer -> void
    (define (test/select cursor-index selection-length text new-cursor-index new-selection-length)
      (let ([make-cmd (lambda (world) `(select/pos+len ,world ,(index->syntax-pos new-cursor-index) ,new-selection-length))])
        (test/cmd-select make-cmd cursor-index selection-length text (list new-cursor-index new-selection-length text))))
      
    ; set-cursor-position : World pos -> World
    ; test/set-cursor-position : index non-negative-integer string index -> void
    (define (test/set-cursor-position cursor-index selection-length text new-cursor-index)
      (let ([make-cmd (lambda (world) `(set-cursor-position ,world ,(index->syntax-pos new-cursor-index)))])
        (test/cmd-select make-cmd cursor-index selection-length text (list new-cursor-index 0 text))))
    
    ; cursor-at-selection-end : World -> World
    ; test/cursor-at-selection-end : index non-negative-integer string -> void
    (define (test/cursor-at-selection-end cursor-index selection-length text)
      (let ([make-cmd (lambda (world) `(cursor-at-selection-end ,world))])
        (test/cmd-select make-cmd cursor-index selection-length text (list (+ cursor-index selection-length) 0 text))))
    
    ; recompute-selection/insert : World pos non-negative-integer -> World
    ; test/recompute-selection/insert : index non-negative-integer string index non-negative-integer expected -> void
    (define (test/recompute-selection/insert cursor-index selection-length text insertion-index insertion-length . expected)
      (let ([make-cmd (lambda (world) `(recompute-selection/insert ,world ,(index->pos insertion-index) ,insertion-length))])
        (test/cmd-select make-cmd cursor-index selection-length text expected)))
    
    ; recompute-selection/delete : World pos non-negative-integer -> World
    ; test/recompute-selection/delete : index non-negative-integer string index non-negative-integer expected -> void
    (define (test/recompute-selection/delete cursor-index selection-length text deletion-index deletion-length . expected)
      (let ([make-cmd (lambda (world) `(recompute-selection/delete ,world ,(index->pos deletion-index) ,deletion-length))])
        (test/cmd-select make-cmd cursor-index selection-length text expected)))
    
    ; recompute-selection/replace : World pos non-negative-integer non-negative-integer -> World
    ; test/recompute-selection/replace : index non-negative-integer string index non-negative-integer non-negative-integer expected -> void
    (define (test/recompute-selection/replace cursor-index selection-length text replace-index deletion-length insertion-length . expected)
      (let ([make-cmd (lambda (world) `(recompute-selection/replace ,world ,(index->pos replace-index) ,deletion-length ,insertion-length))])
        (test/cmd-select make-cmd cursor-index selection-length text expected)))

    ; test/cmd-mark : (World -> datum) index non-negative-integer string expected -> void
    (define (test/cmd-mark make-mark-cmd mark-index mark-length text expected)
      (let* ([world (copy-struct World empty-world
                                 [World-text text]
                                 [World-syntax-list (string->syntax-list text)]
                                 [World-mark-position (index->syntax-pos mark-index)]
                                 [World-mark-length   mark-length])]
             [world (eval-cmd (make-mark-cmd world))])
        (test (list [World-mark-index       world]
                    [World-mark-length      world]
                    [World-text             world])
              expected)))
   
    ; mark/pos+len : World pos non-negative-integer -> World
    ; test/mark : index non-negative-integer string index non-negative-integer -> void
    (define (test/mark mark-index mark-length text new-mark-index new-mark-length)
      (let ([make-cmd (lambda (world) `(mark/pos+len ,world ,(index->syntax-pos new-mark-index) ,new-mark-length))])
        (test/cmd-mark make-cmd mark-index mark-length text (list new-mark-index new-mark-length text))))
    
    ; set-mark-position : World pos -> World
    ; test/set-mark-position : index non-negative-index string index -> void
    (define (test/set-mark-position mark-index mark-length text new-mark-index)
      (let ([make-cmd (lambda (world) `(set-mark-position ,world ,(index->syntax-pos new-mark-index)))])
        (test/cmd-mark make-cmd mark-index mark-length text (list new-mark-index 0 text))))
    
    ; mark-at-start : World -> World
    ; test/mark-at-start : index non-negative-integer string -> void
    (define (test/mark-at-start mark-index mark-length text)
      (let ([make-cmd (lambda (world) `(mark-at-start ,world))])
        (test/cmd-mark make-cmd mark-index mark-length text (list mark-index 0 text))))
    
    ; mark-at-end : World -> World
    ; test/mark-at-end : index non-negative-integer string -> void
    (define (test/mark-at-end mark-index mark-length text)
      (let ([make-cmd (lambda (world) `(mark-at-end ,world))])
        (test/cmd-mark make-cmd mark-index mark-length text (list (+ mark-index mark-length) 0 text))))
    
    ; unmark : World -> World
    ; test/unmark : index non-negative-integer string -> void
    (define (test/unmark mark-index mark-length text)
      (let ([make-cmd (lambda (world) `(unmark ,world))])
        (test/cmd-mark make-cmd mark-index mark-length text (list mark-index 0 text))))
    
    ; test/cmd-select+mark : (World -> datum) index non-negative-integer index non-negative-integer string expected -> void
    (define (test/cmd-select+mark make-select+mark-cmd cursor-index selection-length mark-index mark-length text expected)
      (let* ([world (copy-struct World empty-world
                                 [World-text text]
                                 [World-syntax-list (string->syntax-list text)]
                                 [World-cursor-position (index->syntax-pos cursor-index)]
                                 [World-selection-length selection-length]
                                 [World-mark-position   (index->syntax-pos mark-index)]
                                 [World-mark-length      mark-length])]
             [world (with-handlers ([voice-exn? (lambda args world)])
                      (eval-cmd (make-select+mark-cmd world)))])
        (test (list [World-cursor-index world]
                    [World-selection-length world]
                    [World-mark-index  world]
                    [World-mark-length world]
                    [World-text        world])
              expected)))

    ; exchange : World -> World
    ; test/exchange : index non-negative-integer index non-negative-integer string -> void
    (define (test/exchange cursor-index selection-length mark-index mark-length text)
      (let ([make-cmd (lambda (world) `(exchange ,world))])
        (test/cmd-select+mark make-cmd cursor-index selection-length mark-index mark-length text (list mark-index
                                                                                                       mark-length
                                                                                                       cursor-index
                                                                                                       selection-length
                                                                                                       text))))
    
    ; selection->mark : World -> World
    ; test/selection->mark : index non-negative-integer index non-negative-integer string -> void
    (define (test/selection->mark cursor-index selection-length mark-index mark-length text)
      (let ([make-cmd (lambda (world) `(selection->mark ,world))])
        (test/cmd-select+mark make-cmd cursor-index selection-length mark-index mark-length text (list cursor-index 0 cursor-index selection-length text))))
    
    ; recompute-mark/insert : World pos non-negative-integer -> World
    ; test/recompute-mark/insert : index non-negative-integer string index non-negative-integer expected -> void
    (define (test/recompute-mark/insert mark-index mark-length text insertion-index insertion-length . expected)
      (let ([make-cmd (lambda (world) `(recompute-mark/insert ,world ,(index->pos insertion-index) ,insertion-length))])
        (test/cmd-mark make-cmd mark-index mark-length text expected)))
    
    ; recompute-mark/delete : World pos non-negative-integer -> World
    ; test/recompute-mark/delete : index non-negative-integer string index non-negative-integer expected -> void
    (define (test/recompute-mark/delete mark-index mark-length text deletion-index deletion-length . expected)
      (let ([make-cmd (lambda (world) `(recompute-mark/delete ,world ,(index->pos deletion-index) ,deletion-length))])
        (test/cmd-mark make-cmd mark-index mark-length text expected)))
        
    ; recompute-mark/replace : World pos non-negative-integer non-negative-integer -> World
    ; test/recompute-mark/replace : index non-negative-integer string index non-negative-integer non-negative-integer expected -> void
    (define (test/recompute-mark/replace mark-index mark-length text replace-index deletion-length insertion-length . expected)
      (let ([make-cmd (lambda (world) `(recompute-mark/replace ,world ,(index->pos replace-index) ,deletion-length ,insertion-length))])
        (test/cmd-mark make-cmd mark-index mark-length text expected)))

    ; update-text : World string -> World
    ; test/update-text : string string -> void
    (define (test/update-text text new-text)
      (test (World-text (send actions update-text (copy-struct World empty-world [World-text text]) new-text)) new-text))
    
    ; indent/pos+len : World pos non-negative-integer -> World
    ; test/indent : index non-negative-integer index non-negative-integer string index non-negative-integer expected -> void
    (define (test/indent cursor-index selection-length mark-index mark-length text indent-index indent-length . expected)
      (let ([make-cmd (lambda (world) `(indent/pos+len ,world ,(index->pos indent-index) ,indent-length))])
        (test/cmd-select+mark make-cmd cursor-index selection-length mark-index mark-length text expected)))
    
    ; cleanup-text/pos+len : World pos non-negative-integer -> World
    ; test/cleanup-text : index non-negative-integer index non-negative-integer string index non-negative-integer expected -> void
    (define (test/cleanup-text cursor-index selection-length mark-index mark-length text cleanup-index cleanup-length . expected)
      (let ([make-cmd (lambda (world) `(cleanup-text/pos+len ,world ,(index->pos cleanup-index) ,cleanup-length))])
        (test/cmd-select+mark make-cmd cursor-index selection-length mark-index mark-length text expected)))
    
    ; cleanup-text/selection : World -> World
    ; test/cleanup-text/selection : index non-negative-integer index non-negative-integer string expected -> void
    (define (test/cleanup-text/selection cursor-index selection-length mark-index mark-length text . expected)
      (let ([make-cmd (lambda (world) `(cleanup-text/selection ,world))])
        (test/cmd-select+mark make-cmd cursor-index selection-length mark-index mark-length text expected)))

    ; insert : World pos string -> World
    ; test/insert : index non-negative-integer index non-negative-integer string index string expected -> void
    (define (test/insert cursor-index selection-length mark-index mark-length text insertion-index insertion-text . expected)
      (let ([make-cmd (lambda (world) `(insert ,world ,(index->pos insertion-index) ,insertion-text))])
        (test/cmd-select+mark make-cmd cursor-index selection-length mark-index mark-length text expected)))
    
    ; delete/pos+len : World pos non-negative-integer -> World
    ; test/delete/pos+len : index non-negative-integer index non-negative-integer string index non-negative-integer expected -> void
    (define (test/delete/pos+len cursor-index selection-length mark-index mark-length text deletion-index deletion-length . expected)
      (let ([make-cmd (lambda (world) `(delete/pos+len ,world ,(index->pos deletion-index) ,deletion-length))])
        (test/cmd-select+mark make-cmd cursor-index selection-length mark-index mark-length text expected)))
    
    ; delete/selection : World -> World
    ; test/delete/selection : index non-negative-integer index non-negative-integer string expected -> void
    (define (test/delete/selection cursor-index selection-length mark-index mark-length text . expected)
      (let ([make-cmd (lambda (world) `(delete/selection ,world))])
        (test/cmd-select+mark make-cmd cursor-index selection-length mark-index mark-length text expected)))
        
    ; delete/mark : World -> World
    ; test/delete/mark : index non-negative-integer index non-negative-integer string expected -> void
    (define (test/delete/mark cursor-index selection-length mark-index mark-length text . expected)
      (let ([make-cmd (lambda (world) `(delete/mark ,world))])
        (test/cmd-select+mark make-cmd cursor-index selection-length mark-index mark-length text expected)))
    
    ; replace/selection : World string -> World
    ; test/replace/selection : index non-negative-integer index non-negative-integer string string expected -> void
    (define (test/replace/selection cursor-index selection-length mark-index mark-length text replace-text . expected)
      (let ([make-cmd (lambda (world) `(replace/selection ,world ,replace-text))])
        (test/cmd-select+mark make-cmd cursor-index selection-length mark-index mark-length text expected)))
    
    ; close : World -> World
    ; test/close : index non-negative-integer index non-negative-integer string expected -> void
    (define (test/close cursor-index selection-length mark-index mark-length text . expected)
      (let ([make-cmd (lambda (world) `(close ,world))])
        (test/cmd-select+mark make-cmd cursor-index selection-length mark-index mark-length text expected)))
    
    ; dedouble-ellipsis : World -> World
    ; test/dedouble-ellipsis : index non-negative-integer index non-negative-integer string expected -> void
    (define (test/dedouble-ellipsis cursor-index selection-length mark-index mark-length text . expected)
      (let ([make-cmd (lambda (world) `(dedouble-ellipsis ,world))])
        (test/cmd-select+mark make-cmd cursor-index selection-length mark-index mark-length text `(,cursor-index ,selection-length ,@expected))))
    
    ; open : boolean World symbol/false pos/false non-negative-integer non-negative-integer boolean -> World
    ; test/open : index non-negative-integer index non-negative-integer string (union symbol false) (union pos false) expected -> void
    (define (test/open cursor-index selection-length mark-index mark-length text symbol/false pos/false . expected)
      (let ([make-cmd (lambda (world) `(open ,false ,world ,symbol/false ,pos/false 0 0 #f))])
        (test/cmd-select+mark make-cmd cursor-index selection-length mark-index mark-length text expected)))
    
    ; test/open-square : index non-negative-integer index non-negative-integer string (union symbol false) (union index false) expected -> void
    (define (test/open-square cursor-index selection-length mark-index mark-length text symbol/false index/false . expected)
      (let ([make-cmd (lambda (world) `(open ,true ,world ,symbol/false ,(and index/false (index->syntax-pos index/false)) 0 0 #f))])
        (test/cmd-select+mark make-cmd cursor-index selection-length mark-index mark-length text expected)))

    ; symbol : World symbol pos/false non-negative-integer non-negative-integer boolean -> World
    ; test/symbol : index non-negative-integer index non-negative-integer string symbol (union index false) expected -> void
    (define (test/symbol cursor-index selection-length mark-index mark-length text symbol index/false . expected)
      (let ([make-cmd (lambda (world) `(symbol ,world ,symbol ,(and index/false (index->syntax-pos index/false)) 0 0 #f))])
        (test/cmd-select+mark make-cmd cursor-index selection-length mark-index mark-length text expected)))
    
    ; delete : World -> World
    ; test/delete : index non-negative-integer index non-negative-integer string expected -> void
    (define (test/delete cursor-index selection-length mark-index mark-length text . expected)
      (let ([make-cmd (lambda (world) `(delete ,world))])
        (test/cmd-select+mark make-cmd cursor-index selection-length mark-index mark-length text expected)))

    ; fill : World -> World
    ; test/fill : index non-negative-integer index non-negative-integer string expected -> void
    (define (test/fill cursor-index selection-length mark-index mark-length text . expected)
      (let ([make-cmd (lambda (world) `(fill ,world))])
        (test/cmd-select+mark make-cmd cursor-index selection-length mark-index mark-length text expected)))
    
    ; test/push : World -> World
    ; test/push : index non-negative-integer index non-negative-integer string expected -> void
    (define (test/push cursor-index selection-length mark-index mark-length text . expected)
      (let ([make-cmd (lambda (world) `(push ,world))])
        (test/cmd-select+mark make-cmd cursor-index selection-length mark-index mark-length text expected)))      
    
    ; test/clipboard : (World -> datum) index non-negative-integer string string expected -> void
    #; (define (test/clipboard make-cmd/clip cursor-index selection-length text clipboard-content expected)
      (let* ([world (copy-struct World empty-world
                                 [World-text text]
                                 [World-syntax-list (string->syntax-list text)]
                                 [World-cursor-position (index->syntax-pos cursor-index)]
                                 [World-selection-length selection-length])]
             [_ (set-clipboard-content clipboard-content)]
             [world (eval-cmd (make-cmd/clip world))])
        (test (list [World-cursor-index world]
                    [World-selection-length world]
                    [World-text world]
                    (get-clipboard-content))
              expected)))
    
    ; copy : World -> World
    ; test/copy : index non-negative-integer string string expected -> void
    #; (define (test/copy cursor-index selection-length text clipboard-content expected)
      (let ([make-cmd (lambda (world) `(copy ,world))])
        (test/clipboard make-cmd cursor-index selection-length text clipboard-content (list cursor-index
                                                                                            selection-length
                                                                                            text
                                                                                            expected))))

    ; cut : World -> World
    ; test/cut : index non-negative-integer string string expected -> void
    #; (define (test/cut cursor-index selection-length text clipboard-content . expected)
      (let ([make-cmd (lambda (world) `(cut ,world))])
        (test/clipboard make-cmd cursor-index selection-length text clipboard-content (cons cursor-index (cons 0 expected)))))
    
    ; paste : World -> World
    ; test/paste : index non-negative-integer string string expected -> void
    #; (define (test/paste cursor-index selection-length text clipboard-content . expected)
      (let ([make-cmd (lambda (world) `(paste ,world))])
        (test/clipboard make-cmd cursor-index selection-length text clipboard-content (append expected (list clipboard-content)))))
    
    ; cleanup-whitespace : index string (index list) -> str and (index list)
    ; test/clean : index string (index list) string (index list) -> void
    #;(define (test/clean index chars markers expected-chars expected-markers)
      (let-values ([(c m) (cleanup-whitespace index chars markers)])
        (test (list c m) (list expected-chars expected-markers))))
    
    ; enter/pos+len : World pos non-negative-integer -> World
    ; test/enter : index non-negative-integer index non-negative-integer string index non-negative-integer expected -> void
    (define (test/enter cursor-index selection-length mark-index mark-length text enter-index enter-length . expected)
      (let ([make-cmd (lambda (world) `(enter/pos+len ,world ,(index->pos enter-index) ,enter-length))])
        (test/cmd-select+mark make-cmd cursor-index selection-length mark-index mark-length text expected)))
    
    ; join/pos+len : World pos non-negative-integer -> World
    ; test/join : index non-negative-interger index non-negative-interger string index non-negative-integer expected -> void
    (define (test/join cursor-index selection-length mark-index mark-length text join-index join-length . expected)
      (let ([make-cmd (lambda (world) `(join/pos+len ,world ,(index->pos join-index) ,join-length))])
        (test/cmd-select+mark make-cmd cursor-index selection-length mark-index mark-length text expected)))
    
    ; transpose : World -> World
    ; test/transpose : index non-negative-integer string expected -> void
    (define (test/transpose cursor-index selection-length text . expected)
      (let ([make-cmd (lambda (world) `(transpose ,world))])
        (test/cmd-select make-cmd cursor-index selection-length text expected)))
    
    ; magic/expand : World pos symbol non-negative-integer boolean -> symbol
    ; test/magic : index symbol non-negative-integer string expected -> void
    (define (test/magic magic-index magic-symbol magic-match text expected)
      (let* ([world (send actions update-text empty-world text)]
             [symbol (with-handlers ([voice-exn? (lambda args false)])
                       (send actions magic/expand world (index->pos magic-index) magic-symbol magic-match false))])
        (test symbol expected)))
    
    ; magic/completion : World symbol -> symbol
    ; test/completion : index string symbol expected -> void
    (define (test/completion cursor-index text magic-symbol expected)
      (let* ([world (send actions select/pos+len (send actions update-text empty-world text) (index->pos cursor-index) 0)]
             [symbol (with-handlers ([voice-exn? (lambda args false)])
                       (send actions magic/completion world magic-symbol))])
        (test symbol expected)))
    
    (print-tests 'bad)

    
    ; test/select : index non-negative-integer string index non-negative-integer -> void
    (test/select 0 0 "" 0 0)
    (test/select 0 0 "(define (foo x y z) (+ x y z))" 1 6)
    (test/select 1 6 "(define (foo x y z) (* x y z))" 8 11)
    (test/select 20 9 "(define (foo x y z) (* x y z))" 8 11)
    (test/select 20 9 "(define (foo x y z) (* x y z))" 0 0)
    
    ; test/set-cursor-position : index non-negative-integer string index -> void
    (test/set-cursor-position 0 0 "" 0)
    (test/set-cursor-position 8 11 "(define (foo x y z) (/ x y z))" 1)
    (test/set-cursor-position 0 30 "(define (foo x y z) (* x y z))" 13)
    
    ; test/cursor-at-selection-end : index non-negative-integer string -> void
    (test/cursor-at-selection-end 0 0 "")
    (test/cursor-at-selection-end 20 9 "(define (foo x y z) (* x y z))")
    (test/cursor-at-selection-end 1 6 "(define (foo x y z) (* x y z))")
    
    ; test/recompute-selection/insert : index non-negative-integer string index non-negative-integer expected -> void
    (test/recompute-selection/insert 0 0 "" 0 0 0 0 "")
    (test/recompute-selection/insert 3 5 "(foo bar)" 10 2 3 5 "(foo bar)")
    (test/recompute-selection/insert 8 4 "(bar foo)" 2 3 11 4 "(bar foo)")
    
    ; test/recompute-selection/delete : index non-negative-integer string index non-negative-integer expected -> void
    (test/recompute-selection/delete 0 0 "" 0 0 0 0 "")
    (test/recompute-selection/delete 0 5 "(foo bar)" 6 5 0 5 "(foo bar)")
    (test/recompute-selection/delete 5 4 "(bar foo)" 3 2 3 4 "(bar foo)")

    ; test/recompute-selection/replace : index non-negative-integer string index non-negative-integer non-negative-integer expected -> void
    (test/recompute-selection/replace 0 0 "" 0 0 0 0 0 "")
    (test/recompute-selection/replace 5 5 "(foo bar)" 11 3 4 5 5 "(foo bar)")
    (test/recompute-selection/replace 6 3 "(bar foo)" 1 3 2 5 3 "(bar foo)")

    ; test/mark : index non-negative-integer string index non-negative-integer -> void
    (test/mark 0 0 "" 0 0)
    (test/mark 0 0 "(define (foo x y z) (+ x y z))" 0 30)
    (test/mark 1 6 "(define (foo x y z) (+ x y z))"  30 0)
    (test/mark 10 3 "(define (foo x y z) (+ x y z))" 8 11)
    
    ; test/set-mark-position : index non-negative-index string index -> void
    (test/set-mark-position 0 0 "" 0)
    (test/set-mark-position 1 2 "((lambda (x) (x x)) (lambda (x) (x x)))" 3)
    (test/set-mark-position 989 34 "(define foo bar)" 0)
    
    ; test/mark-at-start : index non-negative-integer string -> void
    (test/mark-at-start 0 0 "")
    (test/mark-at-start 100 1000 "(define (foo x y z) (+ x y z))")
    (test/mark-at-start 234 47 "dummy")
    
    ; test/mark-at-end : index non-negative-integer string -> void
    (test/mark-at-end 0 0 "")
    (test/mark-at-end 210 43 "(define (-/+ a) (values (+ a) (- a)))")
    (test/mark-at-end 32 89 " dummy ")
    
    ; test/unmark : index non-negative-integer string -> void
    (test/unmark 0 0 "")
    (test/unmark 8 9 "(define (foo bar) (bar foo))")
    (test/unmark 1 6 "(define (foo bar) (bar foo))")
    
    ; test/exchange : index non-negative-integer index non-negative-integer string -> void
    (test/exchange 0 0 0 0 "")
    (test/exchange 0 1 2 3 "(define (foo bar) (bar foo))")
    (test/exchange 2 8 1 9 "(+ 1 2 (* 34 (- 1)))")
    
    ; test/selection->mark : index non-negative-integer index non-negative-integer string -> void
    (test/selection->mark 0 0 0 0 "")
    (test/selection->mark 1 6 43 98"(define (foo x y z) (+ x y z))")
    (test/selection->mark 0 30 0 0 "(define (foo x y z) (+ x y z))")
    
    ; test/recompute-mark/insert : index non-negative-integer string index non-negative-integer expected -> void
    (test/recompute-mark/insert 0 0 "" 0 0 0 0 "")
    (test/recompute-mark/insert 20 30 "(define (foo bar) (bar foo))" 60 9 20 30 "(define (foo bar) (bar foo))")
    (test/recompute-mark/insert 30 20 "(define (foo bar) (bar foo))" 10 20 50 20 "(define (foo bar) (bar foo))")
    
    ; test/recompute-mark/delete : index non-negative-integer string index non-negative-integer expected -> void
    (test/recompute-mark/delete 0 0 "" 0 0 0 0 "")
    (test/recompute-mark/delete 20 30 "(define (foo bar) (bar foo))" 90 80 20 30 "(define (foo bar) (bar foo))")
    (test/recompute-mark/delete 30 20 "(define (foo bar) (bar foo))" 10 10 20 20 "(define (foo bar) (bar foo))")
    
    ; test/recompute-mark/replace : index non-negative-integer string index non-negative-integer non-negative-integer expected -> void
    (test/recompute-mark/replace 0 0 "" 0 0 0 0 0 "")
    (test/recompute-mark/replace 30 20 "(define (foo bar) (bar foo))" 100 30 50 30 20 "(define (foo bar) (bar foo))")
    (test/recompute-mark/replace 30 20 "(define (foo bar) (bar foo))" 10 10 40 60 20 "(define (foo bar) (bar foo))")

    ; test/update-text : string string -> void
    (test/update-text "" "")
    (test/update-text "(define (foo x y z) (+ (* x y) z))" "(define (foo bar) (bar foo))")
    
    ; test/indent : index non-negative-integer index non-negative-integer string index non-negative-integer expected -> void
    (test/indent 0 0 0 0 "" 0 0 0 0 0 0 "")
    (test/indent 7 3 13 3 "    (  foo   bar   )     " 0 0 7 3 13 3 "    (  foo   bar   )     ")
    (test/indent 8 3 14 3 "\n    (  foo   bar   )     " 1 0 4 3 10 3 "\n(  foo   bar   )     ")
    (test/indent 1 3 5 3 "(foo\nbar)" 1 3 1 3 5 3 "(foo\nbar)")
    (test/indent 1 3 5 3 "(foo\nbar)" 5 3 1 3 6 3 "(foo\n bar)")
    (test/indent 0 0 4 9 "    (foo\nbar)" 4 9 0 0 4 14 "    (foo\n     bar)")
    
    ; test/cleanup-text : index non-negative-integer index non-negative-integer string index non-negative-integer expected -> void
    (test/cleanup-text 0 0 0 0 "" 0 0 0 0 0 0 "")
    (test/cleanup-text 1 6 15 18 "(define fact\n  (lambda (n)\n    n))"     8  4 1 6 15 18 "(define fact\n  (lambda (n)\n    n))")
    (test/cleanup-text 1 6 15 18 "(define fact\n  (lambda (n)\n    n))"    24  1 1 6 15 18 "(define fact\n  (lambda (n)\n    n))")
    (test/cleanup-text 1 6 16 20 "(define fact\n   (lambda (n)\n      n))" 25  1 1 6 15 20 "(define fact\n  (lambda (n)\n      n))")
    (test/cleanup-text 1 6 16 20 "(define fact\n   (lambda (n)\n      n))"  0 34 1 6 15 18 "(define fact\n  (lambda (n)\n    n))")
    (test/cleanup-text 1 6 16 20 "(define fact   \n(lambda (n)      \nn))"  0 34 1 6 15 18 "(define fact\n  (lambda (n)\n    n))")
    
    ; test/cleanup-text/selection : index non-negative-integer index non-negative-integer string expected -> void
    (test/cleanup-text/selection 0 0 0 0 "" 0 0 0 0 "")
    (test/cleanup-text/selection  1  6 15 18 "(define fact\n  (lambda (n)\n    n))"  1  6 15 18 "(define fact\n  (lambda (n)\n    n))")
    (test/cleanup-text/selection 15 18  1  6 "(define fact\n  (lambda (n)\n    n))" 15 18  1  6 "(define fact\n  (lambda (n)\n    n))")
    (test/cleanup-text/selection  0 34  0  0 "(define fact\n  (lambda (n)\n    n))"  0 34  0  0 "(define fact\n  (lambda (n)\n    n))")

    ; test/insert : index non-negative-integer index non-negative-integer string index string expected -> void
    (test/insert 0 0 0 0 "" 0 "" 0 0 0 0 "")
    (test/insert 8 11 1 6 "(define (foo x y z) (+ x y z))" 23 "(* x y z) " 8 11 1 6 "(define (foo x y z) (+ (* x y z) x y z))")
    (test/insert 1 6 8 11 "(define (foo bar) (bar foo))" 0 "(some thing dummy) " 20 6 27 11  "(some thing dummy) (define (foo bar) (bar foo))")
    
    ; test/delete/pos+len : index non-negative-integer index non-negative-integer string index non-negative-integer expected -> void
    (test/delete/pos+len  0  0  0  0 "" 0 0 0 0 0 0 "")
    (test/delete/pos+len  8 11  1  6 "(define (foo x y z) (+ x y z))" 25 1 8 11 1 6 "(define (foo x y z) (+ x z))")
    (test/delete/pos+len 20  9  8 11 "(define (foo x y z) (+ x y z))" 1 6 13 9 1 11 "((foo x y z) (+ x y z))" )

    ; test/delete/selection : index non-negative-integer index non-negative-integer string expected -> void
    (test/delete/selection  0  0  0  0 "" 0 0 0 0 "")
    (test/delete/selection 23  7  1  6 "(define (foo bar) (bar (bar 0) (bar (bar foo))))" 22 0 1 6 "(define (foo bar) (bar (bar (bar foo))))")
    (test/delete/selection  1  6 23  6 "(define (foo bar) (bar (bar 0) (bar (bar foo))))" 1 0 16 6 "((foo bar) (bar (bar 0) (bar (bar foo))))")
    (test/delete/selection  0 30  0  0 "(define (foo x y z) (+ x y z))" 0 0 0 0 "")
    (test/delete/selection  1  6  0  0 "(define (foo x y z) (+ x y z))" 1 0 0 0 "((foo x y z) (+ x y z))")
    (test/delete/selection 11  9  1  9 " (+ 1 2 3) (* 4 5 6) "  9 0 0 9 "(+ 1 2 3)")
    (test/delete/selection  1  9 11  9 " (+ 1 2 3) (* 4 5 6) "  0 0 0 9 "(* 4 5 6)")

    ; test/delete/mark : index non-negative-integer index non-negative-integer string expected -> void
    (test/delete/mark 0 0 0 0 "" 0 0 0 0 "")
    (test/delete/mark 1 6 23 7 "(define (foo bar) (bar (bar 0) (bar (bar foo))))" 1 6 22 0 "(define (foo bar) (bar (bar (bar foo))))")
    (test/delete/mark 23 7 1 6 "(define (foo bar) (bar (bar 0) (bar (bar foo))))" 16 7 1 0 "((foo bar) (bar (bar 0) (bar (bar foo))))")
    
    ; test/replace/selection : index non-negative-integer index non-negative-integer string string expected -> void
    (test/replace/selection 0 0 0 0 "" "" 0 0 0 0 "")
    (test/replace/selection 0 0 0 0 "" "foobar" 0 6 6 0 "foobar")
    (test/replace/selection 1 6 0 0 "(define (foo x y z) (+ x y z))" "define/public" 1 13 0 0 "(define/public (foo x y z) (+ x y z))")
    (test/replace/selection 9 3 1 6 "(define (foo bar) (bar foo))" "dummy" 9 5 1 6 "(define (dummy bar) (bar foo))")
    (test/replace/selection 1 6 9 3 "(define (foo bar) (bar foo))" "dummy" 1 5 8 3 "(dummy (foo bar) (bar foo))")
    
    ; test/close : index non-negative-integer index non-negative-integer string expected -> void
    (test/close  0  0  0  0  ""                                           0  0  0  0  ""                                         )
    (test/close  1  0  1  1  "(a ---)"                                    2  0  1  0  "()"                                       )
    (test/close  3  1  1  1  "(x a ---)"                                  3  0  1  1  "(x)"                                      )
    (test/close  0  2  0  5  "a ---"                                      0  0  0  0  ""                                         )
    (test/close  0  3 15  0  "[name expr] ---"                            0  0  0  0  ""                                         )
    (test/close  0  4  0  7  "(a ---) ---"                                0  0  0  0  ""                                         )
    (test/close  1  5  0  7  "(a ---) ---"                                2  0  0  2  "() ---"                                   )
    (test/close  2  6  2  0  "((a ---) ---)"                              3  0  2  0  "(() ---)"                                 )
    (test/close  4  7  4  1  "((a b ---) ---)"                            4  0  3  0  "((a) ---)"                                )
    (test/close  6  8  4  1  "((a b c ---) ---)"                          6  0  4  1  "((a b) ---)"                              )
    (test/close  7  9  1 17  "((c (a b ---) ---) ---)"                    7  0  1 11  "((c (a) ---) ---)"                        )
    (test/close  0 10 23 45  ""                                           0 10 23 45  ""                                         )
    (test/close  0 11 43 13  "a"                                          0 11 43 13  "a"                                        )
    (test/close  0 12  7 87  "a b ---"                                    0 12  7 87  "a b ---"                                  )
    (test/close  2 13  4  0  "a b ---"                                    9  0  1  0  "a"                                        )
    (test/close  1 14  0 28  "(a)"                                        1 14  0 28  "(a)"                                      )
    (test/close  0 15  0  0  "(a ---)"                                    0 15  0  0  "(a ---)"                                  )
    (test/close  5 16  1  3  "(let ([$name$ $binding$] ---) $body$ ---)"  5 16  1  3  "(let ([$name$ $binding$] ---) $body$ ---)")
    (test/close  6 17  1  3  "(let ([$name$ $binding$] ---) $body$ ---)"  8  6  1  3  "(let () $body$ ---)"                      )
    (test/close 30 18 41  0  "(let ([$name$ $binding$] ---) $body$ ---)"  7  6 30  0  "(let ([$name$ $binding$] ---))"           )
    (test/close  8 19 19  0  "(let () $body$ ---)"                        8  0  8  0  "(let ())"                                 )

    ; test/dedouble-ellipsis : index non-negative-integer index non-negative-integer string expected -> void
    (test/dedouble-ellipsis  0  0  0  0 "" 0 0 "")
    (test/dedouble-ellipsis  0  1  0  1 "a" 0 1"a")
    (test/dedouble-ellipsis  0  2  0  0 "---" 0 0 "---")
    (test/dedouble-ellipsis  0  1  0  1 "a ---" 0 1 "a a ---")
    (test/dedouble-ellipsis  0  0  0  1 "a ---" 0 1 "a ---")
    (test/dedouble-ellipsis  1  0  0  1 "a ---" 0 1 "a ---")
    (test/dedouble-ellipsis  1  0  0  1 "aa ---" 0 1 "aa aa ---")
    (test/dedouble-ellipsis  1  1  0  1 "aa ---" 0 1 "aa aa ---")
    (test/dedouble-ellipsis  0  4  2  1 "a b ---" 2 1 "a b ---")
    (test/dedouble-ellipsis  2  1  7  0 "a b ---" 9 0 "a b b ---")
    (test/dedouble-ellipsis  1  1  0  7 "(a ---)" 0 9 "(a a ---)")
    (test/dedouble-ellipsis  0  7  0  7 "(a ---) ---" 0 7 "(a ---) (a ---) ---")
    (test/dedouble-ellipsis  1  1  3  0 "(a ---) ---" 5 0 "(a a ---) ---")
    (test/dedouble-ellipsis  0  9  0  9 "((a ---) ---)"  0  9 "((a ---) ---)")
    (test/dedouble-ellipsis  1  1  8  0 "((a ---) ---)" 16  0 "((a ---) (a ---) ---)")
    (test/dedouble-ellipsis  2  1  0  0 "((a ---) ---)"  0  0 "((a a ---) ---)")
    (test/dedouble-ellipsis  0 12  0  0 "((a ---) ---) ---"  0  0 "((a ---) ---) ((a ---) ---) ---")
    (test/dedouble-ellipsis  1  7  0  0 "((a ---) ---) ---"  0  0 "((a ---) (a ---) ---) ---")
    (test/dedouble-ellipsis  2  1  0  0 "((a ---) ---) ---"  0  0 "((a a ---) ---) ---")
    (test/dedouble-ellipsis  0 15  0  0 "(((a ---) ---) ---)"  0  0 "(((a ---) ---) ---)")
    (test/dedouble-ellipsis  1 13  0  0 "(((a ---) ---) ---)"  0  0 "(((a ---) ---) ((a ---) ---) ---)")
    (test/dedouble-ellipsis  2  7  0  0 "(((a ---) ---) ---)"  0  0 "(((a ---) (a ---) ---) ---)")
    (test/dedouble-ellipsis  3  1  0  0 "(((a ---) ---) ---)"  0  0 "(((a a ---) ---) ---)")
    (test/dedouble-ellipsis  3  0  9  1 "(a b --- c)"  9  1 "(a b --- c)")
    (test/dedouble-ellipsis  3  1  9  1 "(a b --- c)" 11  1 "(a b b --- c)")
    (test/dedouble-ellipsis  8  1  5  9 "(a b (c d ---) e --- f) ---"  5 11 "(a b (c d d ---) e --- f) ---")
    (test/dedouble-ellipsis  6  1  1  1 "(a b (c d ---) e --- f) ---"  1  1 "(a b (c d ---) e --- f) (a b (c d ---) e --- f) ---")
    (test/dedouble-ellipsis  5  1 21  1 "(a b (c d ---) e --- f) ---" 21  1 "(a b (c d ---) e --- f) (a b (c d ---) e --- f) ---")
    (test/dedouble-ellipsis 15  1  0 27 "(a b (c d ---) e --- f) ---"  0 29 "(a b (c d ---) e e --- f) ---")
    (test/dedouble-ellipsis  8  1  0 23 "(a b (c d ---) e --- f)"  0 25 "(a b (c d d ---) e --- f)")
    (test/dedouble-ellipsis  6 22  8  1 "(a b (c d ---) e --- f)"  8  1 "(a b (c d ---) e --- f)")
    (test/dedouble-ellipsis  5 21  8  1 "(a b (c d ---) e --- f)"  8  1 "(a b (c d ---) e --- f)")
    (test/dedouble-ellipsis 15  1  8  1 "(a b (c d ---) e --- f)"  8  1 "(a b (c d ---) e e --- f)")

    ; test/open : index non-negative-integer indext non-negative-integer string (union symbol false) (union pos false) expected -> void
    (test/open 0 0  0 0 "" false false 1 6 12 0 "($expr$ ---)")
    (test/open 1 6  0 0 "($expr$ ---)" false false 2 6 0 0 "(($expr$ ---) $expr$ ---)")
    (test/open 2 6 14 6 "(($expr$ ---) $expr$ ---)" false 14 15 6 27 6 "(($expr$ ---) ($expr$ ---) $expr$ ---)")
    (test/open 0 0  0 0 "" ''let  false 7 6 43 0 "(let ([$name$ $binding$] ***)\n  $body$ +++)")
    (test/open 0 0  0 0 "" ''id   false 4 6 11 0 "(id $expr$)")
    (test/open 0 0  0 0 "" ''fact false 6 6 17 0 "(fact $expr$ ---)")
    
    ; test/open-square : index non-negative-integer index non-negative-integer string (union symbol false) (union index false) expected -> void
    (test/open-square 1 6  1  6 "(define (foo bar) (bar foo))" ''astalavista false 14 6 25 0 "([astalavista $expr$ ---] (foo bar) (bar foo))")
    (test/open-square 0 0 23 27 "(copy-struct World world [World-cursor-position pos])" ''World-selection-length 52 77 6 23 27 "(copy-struct World world [World-cursor-position pos] [World-selection-length $expr$ ---])")
    
    ; test/symbol : index non-negative-integer index non-negative-integer string symbol (union index false) expected -> void
    (test/symbol 0 0 0 0 "" ''foo  false 3 0 3 0 "foo")
    (test/symbol 0 0 0 0 "" ''cond false 9 6 52 0 "(cond\n  [$test$ $expr$ ---] +++\n  [else $expr$ ---])")
    
    ; test/delete : index non-negative-integer index non-negative-integer string expected -> void
    (test/delete 0 0 0 0 "" 0 0 0 0 "")
    (test/delete 1 6 0 0 "(define (foo bar) (bar foo))" 1 0 0 0 "((foo bar) (bar foo))")
    (test/delete 1 6 0 0 "($expr$ ---)" 1 0 0 0 "($expr$ ---)")
    
    ; test/fill : index non-negative-integer index non-negative-integer string expected -> void
    (test/fill  0  0 0 0 "" 0 0 0 0 "")
    (test/fill  3  0 0 0 "(a  c) b" 4 0 7 0 "(a b c)")
    (test/fill  0  0 2 1 "b a" 1 0 3 0 "a b")
    (test/fill  3  0 0 1 "b a" 3 0 0 1 "a b")
    (test/fill 10  0 0 0 "a ((b (c d)) (e f g) h)" 18 0 21 1 "a ((b (c d (e f g))) h)")
    (test/fill  4 12 0 0 "([x $expression$][$name$ $binding$] ---) 28 32" 8 6 31 2 "([x 28][$name$ $binding$] ---) 32")
    
    ; test/push : index non-negative-integer index non-negative-integer string expected -> void
    (test/push 0 0  0 0 "" 0 0 0 0 "")
    (test/push 0 0  3 0 "(a  c) b" 7 0 4 0 "(a b c)")
    (test/push 0 0  4 9 "([y $binding$][$name$ $expression$] ---) 432 5677" 35 4 9 6 "([y 432][$name$ $expression$] ---) 5677")
    (test/push 7 3  0 0 " thing pop" 9 0 3 0 "pop thing")
    (test/push 0 3 10 0 "pop thing " 0 5 9 0 "thing pop")
    
    #|
    ; test/copy : index non-negative-integer string string expected -> void
    (test/copy 0 0 "" "" "")
    (test/copy 1 0 "(define (foo bar) (bar foo))" "CLIPBOARD-CONTENT" "CLIPBOARD-CONTENT")
    (test/copy 1 6 "(define (foo bar) (bar foo))" "" "define")
    (test/copy 8 9 "(define (foo bar) (bar foo))" "Something in the clipboard." "(foo bar)")
    
    ; test/cut : index non-negative-integer string string expected -> void
    (test/cut 0 0 "" "" "" "")
    (test/cut 1 0 "(define (foo bar) (bar foo))" "Sweet Dreams" "(define (foo bar) (bar foo))" "Sweet Dreams")
    (test/cut 0 28 "(define (foo bar) (bar foo))" "" "" "(define (foo bar) (bar foo))")
    (test/cut 19 3 "(define (foo bar) (bar foo))" "Meet Her At The Love Parade" "(define (foo bar) (foo))" "bar")
    
    ; test/paste : index non-negative-integer string string expected -> void
    (test/paste 0 0 "" "" 0 0 "")
    (test/paste 0 0 "" "(foo bar)" 0 9 "(foo bar)")
    (test/paste 18 9 "(define (foo bar) (bar foo))" "" 17 0 "(define (foo bar))")
    (test/paste 18 9 "(define (foo bar) (bar foo))" "(foo bar)" 18 9 "(define (foo bar) (foo bar))")
    |#
    
    
    ; test/enter : index non-negative-integer index non-negative-integer string index expected -> void
    (test/enter 0 0 0 0 "" 0 0 1 0 1 0 "\n")
    (test/enter 1 6 0 0 "($expr$ ---)" 1 0 3 6 0 0 "(\n $expr$ ---)")
    (test/enter 0 0 0 0 "(define (foo bar)\n(bar foo))" 8 20 0 0 0 0 "(define\n  (foo bar)\n  (bar foo))")
    
    ; test/join : index non-negative-interger index non-negative-interger string index expected -> void
    (test/join 0 0  0 0 "" 0 0 0 0 0 0 "")
    (test/join 0 0  0 0 "\n" 0 0 0 0 0 0 "")
    (test/join 1 6 20 9 "(define (foo bar)\n  (bar foo))" 1 0 1 6 18 9"(define (foo bar) (bar foo))")
    (test/join 0 0  0 0 "(define\n  (foo bar)\n  (bar foo))" 0 22 0 0 0 0 "(define (foo bar)\n  (bar foo))")
    
    ; test/clean : index string (index list) string (index list) -> void
    #|(test/clean  0 "  hi   " empty "hi" empty)
    (test/clean  0 " \"hi   mom   \"  " empty "\"hi   mom   \"" empty)
    (test/clean  0 " |spaced symbol|  " empty "|spaced symbol|" empty)
    (test/clean  0 "  tabafterspace 	" empty "tabafterspace" empty)
    (test/clean  0 "( () (  ( )    ))" empty "(() (()))" empty)
    (test/clean  0 "( [  ] (   ) )" empty "([] ())" empty)
    (test/clean  0 "  ( define ( fact n ) (if (=  n 0 ) 1   (*  n (fact (  sub1 n ))  )) ) " empty
                   "(define (fact n) (if (= n 0) 1 (* n (fact (sub1 n)))))" empty)
    (test/clean  0 "  hi   " (list 0 1 2 3 4 5 6 7) "hi" (list 0 0 0 1 2 2 2 2))
    (test/clean 10 "  hi   " (list 5 6 12 13 14 15 16 35) "hi" (list 5 6 10 11 12 12 12 30))
    (test/clean 10 " " (list 5 6 12 13 14 15 16 35) "" (list 5 6 11 12 13 14 15 34))
    (test/clean 10 "     hi" (list 5 6 12 13 14 15 16 35) "hi" (list 5 6 10 10 10 10 11 30))
    (test/clean  0 "" (list 0) "" (list 0))
|#
    ; test/transpose : index non-negative-integer string expected -> void
    (test/transpose  0 0 "" 0 0 "")
    (test/transpose  0 7 "(first) (last)"  0 7 "(first) (last)")
    (test/transpose  7 0 "(first) (last)" 14 0 "(last) (first)")
    (test/transpose  8 6 "(first) (last)" 14 0 "(last) (first)")
    (test/transpose 14 0 "(last) (first)" 14 0 "(last) (first)")
        
    ; test/magic : index symbol non-negative-integer string expected -> void
    (test/magic  0 'd  0 "" 'd)
    (test/magic  0 'd  0 "(define (foo bar) (bar foo)) (define-struct World (text syntax-list))" 'd)
    (test/magic  0 'd  1 "(define (foo bar) (bar foo)) (define-struct World (text syntax-list))" 'define)
    (test/magic  0 'd  2 "(define (foo bar) (bar foo)) (define-struct World (text syntax-list))" 'define-struct)
    (test/magic  0 'd  3 "(define (foo bar) (bar foo)) (define-struct World (text syntax-list))"  false)
    (test/magic  0 'd -1 "(define (foo bar) (bar foo)) (define-struct World (text syntax-list))"  false)
    (test/magic 69 'd  0 "(define (foo bar) (bar foo)) (define-struct World (text syntax-list))" 'd)
    (test/magic 69 'd  1 "(define (foo bar) (bar foo)) (define-struct World (text syntax-list))" 'define-struct)
    (test/magic 69 'd  2 "(define (foo bar) (bar foo)) (define-struct World (text syntax-list))" 'define)
    (test/magic 69 'd  3 "(define (foo bar) (bar foo)) (define-struct World (text syntax-list))"  false)
    (test/magic 69 'd -1 "(define (foo bar) (bar foo)) (define-struct World (text syntax-list))"  false)

    ; test/completion : index string symbol expected -> void
    (test/completion 0 "" 'd 'd)
    (test/completion 0 "(define-struct my-struct (a b c)) (define-values (a b c) (values (make-my-struct) (make-my-struct) (make-my-struct)))" 'd 'define-)

    
    )

  (parameterize ([current-templates (lambda () mzscheme-templates)])
    (tests))

  )
