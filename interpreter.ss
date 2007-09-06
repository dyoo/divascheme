(module interpreter mzscheme
  (require (lib "etc.ss")
           (lib "list.ss")
           (lib "plt-match.ss")
           (lib "class.ss")
           (lib "struct.ss")
           (only (lib "1.ss" "srfi") last circular-list partition find)
           "traversal.ss"
           "structures.ss"
           "utilities.ss"
           "actions.ss"
           "tag-state.ss"
           "tag-reader.ss"
           "rope.ss")
  
  ;; This file provides an interpreter for the DivaLanguage,
  ;; that is this is a pure function, without any state, completely stateless,
  ;; which takes an ast (Abstract Syntax Tree) of the DivaLanguage and returns
  ;; a function which takes a World and returns a new World whose differences are conforms to the semantics to the abstract syntax tree.
  ;; Also, the returned function can raise an exception if there's something wrong:
  ;;  `next' asked when no context
  ;;  or something really bad due to a bug.
  (provide interpreter)


  ;; Provided elements to perform the tests.
  (provide eval-Protocol-Syntax-Tree
           current-actions
           default-Next-f
           default-Previous-f
           default-Magic-f
           default-Pass-f
           inc-what-distance
           dec-what-distance
           inc-Loc-distance
           dec-Loc-distance
           revert-cursor
           eval-What/open
           make-make-metric
           make-metric-w/world)

  (define diva-debug? false)
  (define (diva-printf text . args)
    (when diva-debug?
      (apply printf text args)))
    


  ;; TODO : Today, actions really do not need to be an object. Should we do something about that?
  ;; current-actions : void -> actions%
  (define current-actions (make-parameter (make-object actions%)))

  (define max-undo-count 50)

  ;; interpreter : ast World -> (union World ChangeWorld)
  (define (interpreter ast world)
    (print-mem
     'interpreter
     (lambda () (diva-printf "Interpreter was called with tree: ~a~n" ast)
       (eval-Protocol-Syntax-Tree world ast))))
  
  ;; eval-Protocol-Syntax-Tree : World ast -> (union World ChangeWorld)
  (define (eval-Protocol-Syntax-Tree world ast)
    (print-mem*
     'eval-Protocol-Syntax-Tree
     (let* ([world (match ast
                     [(struct Verb ((struct Command ('Again)) #f #f)) world]
                     [_ (copy-struct World world
                                     [World-again ast])])]
            [world (trim-undos world max-undo-count)])
       (match ast
         [(struct Verb ((struct Command ('Open)) loc what))
          (eval-Open false world loc what 0 0 false 'Normal)]
         [(struct Verb ((struct Command ('Open-Square)) loc what))
          (eval-Open true world loc what 0 0 false 'Normal)]
         [(struct Verb ((struct InsertRope-Cmd (rope)) loc #f))
          (eval-InsertRope world rope loc 0 0 false 'Normal)]
         [(struct Verb ((struct Command ('Close)) #f #f))
          (eval-Close world)]
         [(struct Verb ((struct Command ('Insert)) loc #f))
          (eval-Insert world loc)]
         
         [(struct Verb ((struct Command ('Search-Forward)) loc/false what/false))
          (eval-Search world (make-make-metric world metric-forward)
                       (World-cursor-position world) loc/false what/false)]
         [(struct Verb ((struct Command ('Search-Backward)) loc/false what/false))
          (eval-Search world (make-make-metric world metric-backward)
                       (World-cursor-position world) loc/false what/false)]
         [(struct Verb ((struct Command ('Search-Top)) loc what))
          (eval-Search world (make-make-metric world metric-forward)
                       (index->syntax-pos 0) loc what)]
         [(struct Verb ((struct Command ('Search-Bottom)) loc what))
          (eval-Search world (make-make-metric world metric-backward)
                       (index->syntax-pos 0) loc what)]
         [(struct Verb ((struct Command ('Select)) loc what))
          (eval-Select world (World-cursor-position world) loc what)]
         
         [(struct Verb ((struct Command ('Holder)) loc/false what/false))
          (eval-Holder world false loc/false what/false)]
         [(struct Verb ((struct Command ('Holder-Forward)) loc/false what/false))
          (eval-Holder world false loc/false what/false)]
         [(struct Verb ((struct Command ('Holder-Backward)) loc/false what/false))
          (eval-Holder world true loc/false what/false)]
         
         [(struct Verb ((struct Command ('Next)) #f #f))
          (eval-Next world)]
         [(struct Verb ((struct Command ('Previous)) #f #f))
          (eval-Previous world)]
         [(struct Verb ((struct Command ('Cancel)) #f #f))
          (eval-Cancel world)]
         [(struct Verb ((struct Command ('Undo)) #f #f))
          (eval-Undo world)]
         [(struct Verb ((struct Command ('Redo)) #f #f))
          (eval-Redo world)]
         
         [(struct Verb ((struct Command ('Magic)) #f #f))
          (eval-Magic world false)]
         [(struct Verb ((struct Command ('Magic-Bash)) #f what))
          (eval-Magic-Bash world what)]
         [(struct Verb ((struct Command ('Magic-Wrap)) #f #f))
          (eval-Magic world true)]
         [(struct Verb ((struct Command ('Pass)) #f #f))
          (eval-Pass world false)]
         [(struct Verb ((struct Command ('Pass-Wrap)) #f #f))
          (eval-Pass world true)]
         
         [(struct Verb ((struct Command ('Again)) #f #f))
          (eval-Again world)]
         
         [(struct Verb ((struct Command ('Out)) loc/false what/false))
          (eval-Out world loc/false what/false)]
         [(struct Verb ((struct Command ('Up)) #f #f))
          (eval-Up world)]
         [(struct Verb ((struct Command ('Down)) #f #f))
          (eval-Down world)]
         [(struct Verb ((struct Command ('Younger)) #f #f))
          (eval-Younger world)]
         [(struct Verb ((struct Command ('Older)) #f #f))
          (eval-Older world)]
         [(struct Verb ((struct Command ('First)) #f #f))
          (eval-First world)]
         [(struct Verb ((struct Command ('Last)) #f #f))
          (eval-Last world)]
         [(struct Verb ((struct Command ('Forward)) #f #f))
          (eval-Forward world)]
         [(struct Verb ((struct Command ('Backward)) #f #f))
          (eval-Backward world)]
         
         
         [(struct Verb ((struct Command ('Delete)) #f #f))
          (eval-Delete world)]
         [(struct Verb ((struct Command ('Dedouble-Ellipsis)) #f #f))
          (eval-Dedouble-Ellipsis world)]
         
         [(struct Verb ((struct Command ('Bring)) #f #f))
          (eval-Bring world)]
         [(struct Verb ((struct Command ('Push)) #f #f))
          (eval-Push world)]
         
         [(struct Verb ((struct Command ('Exchange)) #f #f))
          (eval-Exchange world)]
         
         [(struct Verb ((struct Command ('Mark)) loc/false what/false))
          (eval-Mark world loc/false what/false)]
         [(struct Verb ((struct Command ('UnMark)) #f #f))
          (eval-UnMark world)]
         
         [(struct Verb ((struct Command ('Copy)) #f #f))
          (eval-Copy world)]
         [(struct Verb ((struct Command ('Cut)) #f #f))
          (eval-Cut world)]
         [(struct Verb ((struct Command ('Paste)) #f #f))
          (eval-Paste world)]
         
         [(struct Verb ((struct Command ('Definition)) #f #f))
          (eval-Definition world)]
         [(struct Verb ((struct Command ('Usage)) #f #f))
          (eval-Usage)]
         
         [(struct Verb ((struct Command ('Enter)) #f #f))
          (eval-Enter world)]
         [(struct Verb ((struct Command ('Join)) #f #f))
          (eval-Join world)]
         [(struct Verb ((struct Command ('Indent)) #f #f))
          (eval-Indent world)]
         
         [(struct Verb ((struct Command ('Transpose)) #f #f))
          (eval-Transpose world)]
         
         [(struct Verb ((struct Command ('Tag)) #f what))
          (eval-Tag ast world what)]
         
         
         [(struct Verb ((struct Command ('Extend-Selection)) #f #f))
          (eval-Extend-Selection world)]
         
         )))
    ) ;; TODO
  
  
  (define (world-clear world)
    (copy-struct World (world-clear/extend world)
                 [World-extension-base #f]
                 [World-extension-length #f]))
  
  (define (world-clear/extend world)
    (copy-struct World world
                 [World-target-column #f]
                 [World-Next-f     (default-Next-f)]
                 [World-Previous-f (default-Previous-f)]
                 [World-Magic-f    (default-Magic-f)]
                 [World-Pass-f     (default-Pass-f)]
                 [World-redo       false]
                 [World-success-message (if (and (equal? (World-success-message world) "")
                                                 (World-extension-base world))
                                            "extending selection..."
                                            (World-success-message world))]))
  
  (define (trim-undos world undo-count)
    (print-mem*
     'trim-undos
     (define dealt-with (make-hash-table))
     (let loop ([world world]
                [undo-count undo-count])
       (and
        world
        (hash-table-get
         dealt-with
         world
         (lambda ()
           (let ([result (cond
                           [(not (World-undo world)) world]
                           [(= undo-count 0)
                            (copy-struct World world
                                         [World-cancel false]
                                         [World-undo false])]
                           [else
                            (let ([sub-undo (loop (World-undo world) (sub1 undo-count))]
                                  [sub-cancel (loop (World-cancel world) (sub1 undo-count))])
                              (copy-struct World world
                                           [World-undo sub-undo]
                                           [World-cancel sub-cancel]))])])
             (hash-table-put! dealt-with world result)
             result)))))))
  
  
  ;; eval-Loc : World (pos -> metric) pos (union Loc false) -> pos
  ;; Returns the location described by the location syntax tree.
  (define (eval-Loc world make-metric-f base loc/false)
    (define (e-w w selector)
      (let ([result (eval-What world make-metric-f base w)])
        (if result (selector result)
            base)))
    (match loc/false
      [#f base]
      [(struct Loc ((? Before?) what)) (e-w what syntax-position)]
      [(struct Loc ((? After?) what)) (e-w what syntax-end-position)]))
  
  ;; eval-What : World (pos -> metric) pos What -> syntax
  ;; In fact, this function should be called eval-What/search,
  ;; but as it is the default behavior, it is just named eval-What.
  (define (eval-What world make-metric-f base what)
    (define (aux noun ref)
      (let ([matches (eval-Noun world make-metric-f base noun)])
	(list-ref/safe matches (modulo ref (add1 (length matches))))))
    (match what
      [#f (find-pos-near base (World-syntax-list world))]
      [(struct WhatN           (noun)) (aux noun              0 )]
      [(struct WhatDN (distance noun)) (aux noun (sub1 distance))]))

  ;; eval-What/open : (union What false) -> (union symbol false)
  (define (eval-What/open what/false)
    (match what/false
      [#f false]
      [(struct WhatN  (  (struct Symbol-Noun (symbol)))) symbol]
      [(struct WhatDN (1 (struct Symbol-Noun (symbol)))) symbol]
      [_ (raise (make-voice-exn "This does not mean anything for me"))]))

  ;; eval-What/holder : World metric What -> syntax
  (define (eval-What/holder world metric what)
    (match what
      [(struct WhatN ((struct Symbol-Noun (symbol))))
       (find-placeholder/symbol symbol 0 metric (World-syntax-list world))]
      [(struct WhatDN (distance (struct Symbol-Noun (symbol))))
       (find-placeholder/symbol symbol (sub1 distance) metric (World-syntax-list world))]
      [_ (raise (make-voice-exn "This does not make sense for me"))]))
  
  ;; eval-What/select : World (pos -> syntax -> non-negative-integer) pos What -> (union non-negative-integer false)
  (define (eval-What/select world make-metric-f what-base what)
    (match what
      [(struct WhatN (noun))
       (find-rank what-base (eval-Noun world make-metric-f 0 noun))]
      [(struct WhatDN (distance noun))
       (find-rank what-base (eval-Noun world make-metric-f 0 noun))]))
  
  ;; eval-What/bash : What -> symbol
  (define (eval-What/bash what)
    (match what
      [(struct WhatN  (  (struct Symbol-Noun (symbol)))) symbol]
      [(struct WhatDN (1 (struct Symbol-Noun (symbol)))) symbol]
      [_ (raise (make-voice-exn "This does not mean anything for me"))]))
  
  ;; eval-Noun : World (pos -> metric) pos Noun -> (syntax list)
  ;; Same thing as eval-What, this function should be called eval-Noun/search.
  (define (eval-Noun world make-metric-f base noun)
    (match noun
      [(struct Symbol-Noun (symbol))
       (let ([result1 (find-all/not-first (syntax-is-symbol? symbol) (World-syntax-list world))]
             [result2 (find-all (syntax-begins-with? symbol) (World-syntax-list world))])
         (sort/metric (make-metric-f base) (append result1 result2)))]
      [(struct The-Symbol (symbol))
       (find-all/metric (syntax-is-symbol? symbol) (make-metric-f base) (World-syntax-list world))]))
  
  ;; eval-Loc+What : World (pos -> metric) (union Loc false) (union What false) -> (union pos syntax)
  (define (eval-Loc+What world make-metric-f loc/false what/false)
    (let* ([loc-base  (World-cursor-position world)]
           [what-base (eval-Loc world make-metric-f loc-base loc/false)])
      (if what/false
          (eval-What world make-metric-f what-base what/false)
          what-base)))
 
  
  ;; inc-what-distance : (union WhatN WhatDN) int -> What
  (define (inc-what-distance what/false x)
    (match what/false
      [(struct WhatN (noun)) (make-WhatDN (+ 1 x) noun)]
      [(struct WhatDN (distance noun))  (make-WhatDN (+ distance x) noun)]
      [_ (raise (make-voice-exn "Not supported"))]))

  ;; dec-what-distance : (union WhatN WhatDN) int -> What
  (define (dec-what-distance what/false x)
    (inc-what-distance what/false (- x)))
  
  ;; inc-Loc-distance : Loc int -> Loc
  (define (inc-Loc-distance loc x)
    (make-Loc (Loc-where loc)
              (inc-what-distance (Loc-what loc) x)))
  
  ;; dec-Loc-distance : Loc int -> Loc
  (define (dec-Loc-distance loc x)
    (inc-Loc-distance loc (- x)))
  
  ;; revert-cursor : World World -> World
  (define (revert-cursor old-world new-world)
    (copy-struct World new-world
                 [World-target-column #f]
                 [World-cursor-position (World-cursor-position old-world)]))
  
  ;; make-make-metric : World (pos pos -> metric) -> pos -> metric
  (define ((make-make-metric world metric-base-last) base)
    (let ([last (syntax-list-last-position (World-syntax-list world))])
      (metric-base-last base last)))
  
  ;; make-metric-w/world : World (pos pos -> metric) -> metric
  (define (make-metric-w/world world metric-base-last)
    ((make-make-metric world metric-base-last) (World-cursor-position world)))
  
  
  ;; eval-Open : boolean World Location/false What/false non-negative-integer non-negative-integer boolean mode -> World
  ;; Add Open Command, assume that the identifier is a template identifier, and insert into text, and select first placeholder
  (define (eval-Open square? world loc/false what/false template-number magic-number template/magic-wrap? mode)
    (let*
        ([symbol/false
          (print-mem
           'eval-Open-symbol/false 
           (lambda () (eval-What/open what/false)))]
         [pos 
          (print-mem
           'eval-Open-pos
           (lambda ()
             (eval-Loc world
                       (make-make-metric world metric-forward)
                       (World-cursor-position world)
                       loc/false)))]
         [new-world
          (print-mem
           'eval-Open-new-world
           (lambda ()
             (send (current-actions) open
                   square?
                   world
                   symbol/false
                   (and loc/false pos)
                   template-number
                   magic-number
                   template/magic-wrap?)))]
         
         [Magic-f           (lambda (new-world template/magic-wrap?)
                              (eval-Open square? world loc/false what/false 0 (add1 magic-number) template/magic-wrap? 'Magic))]
         [Magic-Next-f      (lambda (new-world) (eval-Open square? world loc/false what/false 0 (add1 magic-number) template/magic-wrap? 'Magic))]
         [Magic-Previous-f  (lambda (new-world) (eval-Open square? world loc/false what/false 0 (add1 magic-number) template/magic-wrap? 'Magic))]
         [Pass-f            (lambda (new-world template/magic-wrap?)
                              (eval-Open square? world loc/false what/false (add1 template-number) magic-number template/magic-wrap? 'Pass))]
         [Pass-Next-f       (lambda (new-world) (eval-Open square? world loc/false what/false (add1 template-number) magic-number template/magic-wrap? 'Pass))]
         [Pass-Previous-f   (lambda (new-world) (eval-Open square? world loc/false what/false (sub1 template-number) magic-number template/magic-wrap? 'Pass))]
         [Next-f     (match mode
                       ['Normal (default-Next-f)]
                       ['Magic     Magic-Next-f ]
                       ['Pass       Pass-Next-f ])]
         [Previous-f (match mode
                       ['Normal (default-Previous-f)]
                       ['Magic     Magic-Previous-f ]
                       ['Pass       Pass-Previous-f ])])
      
      (copy-struct World new-world
                   [World-target-column #f]
                   [World-Next-f     Next-f]
                   [World-Previous-f Previous-f]
                   [World-cancel     world]
                   [World-undo       world]
                   [World-redo       false]
                   [World-Magic-f    Magic-f]
                   [World-Pass-f     Pass-f])))


  ;; eval-InsertRope : World rope Location/false non-negative-integer non-negative-integer boolean mode -> World
  (define (eval-InsertRope world a-rope loc/false template-number magic-number
                       template/magic-wrap? mode)
    (local
        ((define (make-unary-callback new-template-number new-magic-number new-mode)
           (lambda (new-world)
             (eval-InsertRope world a-rope loc/false
                          new-template-number
                          new-magic-number
                          template/magic-wrap?
                          new-mode)))
         
         (define (make-binary-callback new-template-number new-magic-number new-mode)
           (lambda (new-world template/magic-wrap?)
             (eval-InsertRope world a-rope loc/false
                          new-template-number
                          new-magic-number
                          template/magic-wrap?
                          new-mode)))
         
         (define (get-Next-and-Previous-f)
           (let ([Magic-Next-f
                  (make-unary-callback 0 (add1 magic-number) 'Magic)]
                 [Magic-Previous-f
                  (make-unary-callback 0 (sub1 magic-number) 'Magic)]
                 [Pass-Next-f
                  (make-unary-callback (add1 template-number) magic-number 'Pass)]
                 [Pass-Previous-f
                  (make-unary-callback (sub1 template-number) magic-number 'Pass)])
             (let ([Next-f (match mode
                             ['Normal (default-Next-f)]
                             ['Magic Magic-Next-f]
                             ['Pass Pass-Next-f])]
                   [Previous-f (match mode
                                 ['Normal (default-Previous-f)]
                                 ['Magic Magic-Previous-f]
                                 ['Pass Pass-Previous-f])])
               (values Next-f Previous-f)))))
      
      (let*-values ([(loc-base)
                     (World-cursor-position world)]
                    [(pos)
                     (eval-Loc world (make-make-metric world metric-forward)
                               loc-base loc/false)]
                    [(new-world)
                     (send (current-actions) symbol
                           world
                           (string->symbol (rope->string a-rope))
                           (and loc/false pos)
                           template-number magic-number template/magic-wrap?)]
                    [(Magic-f)
                     (make-binary-callback 0 (add1 magic-number) 'Magic)]
                    [(Pass-f)
                     (make-binary-callback (add1 template-number) magic-number 'Pass)]
                    [(Next-f Previous-f)
                     (get-Next-and-Previous-f)])
        (copy-struct World new-world
                     [World-target-column #f]
                     [World-Next-f Next-f]
                     [World-Previous-f Previous-f]
                     [World-cancel world]
                     [World-undo world]
                     [World-redo false]
                     [World-Magic-f Magic-f]
                     [World-Pass-f Pass-f]))))
  
  
  
  ;; eval-Close : World -> World
  (define (eval-Close world)
    (let ([new-w (world-clear (send (current-actions) close world))])
      (if (rope=? (World-rope world)
                  (World-rope new-w))
          (copy-struct World new-w
                       [World-cancel world])
          (copy-struct World new-w
                       [World-cancel     world]
                       [World-undo       world]))))
  
  ;; eval-Insert : World Loc -> World
  ;; TODO: and put it in the undo and cancel commands
  (define (eval-Insert world loc)
    (let* ([position   (eval-Loc world
                                 metric-nearest
                                 (World-cursor-position world)
                                 loc)]
           [Next-f     (lambda (new-world)
                         (eval-Insert
                          world
                          (inc-Loc-distance loc 1)))]
           [Previous-f (lambda (new-world)
                         (eval-Insert
                          world
                          (dec-Loc-distance loc 1)))])
      
      (copy-struct World (world-clear (send (current-actions) set-cursor-position world position))
                   [World-Next-f     Next-f]
                   [World-Previous-f Previous-f]
                   [World-cancel     world])))
  
  ;; eval-Search : World (pos -> metric) pos (union Loc false) (union What false) -> World
  ;; TODO: do we really mean to use the same metric for the base and the what?
  (define (eval-Search world make-metric-f loc-base loc/false what)
    (let* ([stx        (with-handlers ([voice-exn? (lambda (exn) (raise (make-voice-exn "No match")))])
                         (eval-What world
                                    make-metric-f
                                    (eval-Loc world
                                              make-metric-f
                                              loc-base
                                              loc/false)
                                    what))]
           [make-NP    (lambda (x)
                         (lambda (new-world)
                           (eval-Search world make-metric-f loc-base loc/false (inc-what-distance what x))))]
           [Next-f     (lambda (new-world)
                         (with-handlers ([voice-exn? 
                                          (lambda (exn) 
                                            (raise (make-voice-exn/world "No more match"
                                                                         (copy-struct World new-world
                                                                                      [World-Next-f (make-NP 2)]))))])
                           ((make-NP 1) new-world)))]
           [Previous-f (lambda (new-world)
                         (with-handlers ([voice-exn? 
                                          (lambda (exn) 
                                            (raise (make-voice-exn/world "No more match"
                                                                         (copy-struct World new-world
                                                                                      [World-Previous-f (make-NP -2)]))))])
                           ((make-NP -1) new-world)))])
      
      (copy-struct World (world-clear (send (current-actions) select/stx world stx))
                   [World-Next-f     Next-f]
                   [World-Previous-f Previous-f]
                   [World-cancel     world])))

  ;; eval-Select : World pos (union Loc false) (union What false) -> World
  (define (eval-Select world loc-base loc/false what)
    (let* ([make-metric-f (make-make-metric world metric-forward)]
           [what-base     (eval-Loc world make-metric-f loc-base loc/false)]
           [rank/false    (with-handlers ([voice-exn? (lambda (exn) (raise (make-voice-exn "nothing to select")))])
                            (eval-What/select world make-metric-f what-base what))]
           [_ (diva-printf "RANK: ~a~n" rank/false)]
           [what          (if rank/false
                              (inc-what-distance what rank/false)
                              what)])
      (diva-printf "HERE: ~a~n" what)
      (if rank/false
          (eval-Search world make-metric-f        0     false what)
          (eval-Search world make-metric-f loc-base loc/false what))))
  
  ;; eval-Holder : World boolean (union Loc false) (union What false) -> World
  (define (eval-Holder world backward? loc/false what/false)
    (let* ([make-metric-f (make-make-metric world (if backward? metric-backward metric-forward))]
           [loc-base      (if backward? 
                              (sub1 (World-cursor-position world))
                              (World-selection-end-position world))]
           [base          (eval-Loc world make-metric-f loc-base loc/false)])
      (if what/false
          ;; eval-What/holder : world metric What -> syntax
          (let* ([stx/false  (eval-What/holder world (make-metric-f base) what/false)]
                 [Next-f     (lambda (new-world) (eval-Holder world backward? loc/false (inc-what-distance what/false 1)))]
                 [Previous-f (lambda (new-world) (eval-Holder world backward? loc/false (dec-what-distance what/false 1)))])
            (copy-struct World (world-clear (send (current-actions) select/stx world stx/false))
                         [World-Next-f     Next-f]
                         [World-Previous-f Previous-f]
                         [World-cancel     world]))
          (copy-struct World (world-clear (send (current-actions) holder world base backward?))
                       [World-cancel     world]))))
    
  (define ((default-Next-f) world)
    (raise (make-voice-exn "Next is not supported")))
  (define ((default-Previous-f) world)
    (raise (make-voice-exn "Previous is not supported")))
  (define ((default-Magic-f) world wrap?)
    (raise (make-voice-exn "Magic is not supported")))
  (define ((default-Pass-f) world template-wrap?)
    (raise (make-voice-exn "Pass is not supported")))

  ;; eval-Next : World -> World
  (define (eval-Next world)
    ((World-Next-f world) world))

  ;; eval-Previous : World -> World
  (define (eval-Previous world)
    ((World-Previous-f world) world))
    
  ;; eval-Cancel : World -> World
  (define (eval-Cancel world)
    (if (World-cancel world)
        (copy-struct World (World-cancel world)
                     [World-redo world])
        (raise (make-voice-exn "Cancel is not supported"))))

  ;; eval-Undo : World -> World
  (define (eval-Undo world)
    (if (World-undo world)
        (copy-struct World (World-undo world)
                     [World-redo world])
        (raise (make-voice-exn "Nothing to undo"))))
  
  ;; eval-Redo : World -> World
  (define (eval-Redo world)
    (if (World-redo world)
        (World-redo world)
        (raise (make-voice-exn "Nothing to redo"))))
  
  ;; eval-Magic : World boolean -> World
  (define (eval-Magic world magic-wrap?)
    ((World-Magic-f world) world magic-wrap?))
  
  ;; eval-Pass : World boolean -> World
  (define (eval-Pass world template-wrap?)
    ((World-Pass-f world) world template-wrap?))
  
  ;; eval-Again : World -> World
  (define (eval-Again world)
    (unless (World-again world)
      (raise (make-voice-exn "Nothing to reexecute")))
    (eval-Protocol-Syntax-Tree world (World-again world)))
  
  ;; eval-Magic-Bash : World What -> World
  (define (eval-Magic-Bash world what)
    (let ([symbol (eval-What/bash what)])
      (copy-struct World (world-clear (send (current-actions) magic-bash world symbol))
                   [World-cancel     world]
                   [World-undo       world])))
    
      
  
  ;; TODO
  ;; eval-Out : World (union Loc false) (union What false) -> World
  (define (eval-Out world loc/false what/false)
    (let* ([stx/pos    (eval-Loc+What world metric-nearest loc/false what/false)]
           [Next-f (if what/false
                           (lambda (new-world) (eval-Out world loc/false (inc-what-distance what/false 1)))
                           (default-Next-f))]
           [Previous-f (if what/false
                           (lambda (new-world) (eval-Out world loc/false (dec-what-distance what/false 1)))
                           (default-Previous-f))]
           [base       (if (syntax? stx/pos)
                           (syntax-position stx/pos)
                           stx/pos)]
           
           [stx/false (find-pos base (World-syntax-list world))]

           [stx/false (if (or (not stx/false)
                              (and (= (syntax-position stx/false) (World-cursor-position world))
                                   (= (syntax-span stx/false) (World-selection-length world))))
                          (find-pos-parent base (World-syntax-list world))
                          stx/false)])
      (if stx/false
          (copy-struct World (world-clear (send (current-actions) select/stx world stx/false))
                       [World-Next-f     Next-f]
                       [World-Previous-f Previous-f]
                       [World-cancel     world])
          (raise (make-voice-exn "No containing expression")))))
  
  (define (eval-Down world)
    (eval-column-motion world false))
  
  (define (eval-Up world)
    (eval-column-motion world true))


  ;; eval-column-motion: World boolean -> World
  (define (eval-column-motion world is-up?)
    (let* ([line (line-number (World-rope world) (World-cursor-position world))]
           [target-column (or (World-target-column world)
                              (- (World-cursor-position world)
                                 (line-pos (World-rope world)
                                           (World-cursor-position world))))]
           [stx/false (find-pos-updown line target-column
                                       (World-syntax-list world) is-up?)])
      (if stx/false
          (if (World-extension-base world)
              (copy-struct World
                           (motion-extension world (World-extension-base world) stx/false)
                           [World-target-column target-column])
              
              (copy-struct World (world-clear (send (current-actions) select/stx world stx/false))
                           [World-target-column target-column]
                           [World-cancel world]))
          (eval-line-motion world (if is-up? 'up 'down)))))
  
  
  (define (eval-line-motion world direction)
    (queue-imperative-action 
     (world-clear world)
     (lambda (world window)
       (send window move-position direction)
       world)))
  
  
  
  ;; eval-Older : World -> World
  (define (eval-Older world)
    (let ([stx (find-pos-sibling-forward (World-selection-end-position world) (World-syntax-list world))])
      (if stx
          (if (World-extension-base world)
              (motion-extension world (World-extension-base world) stx)
              (copy-struct World (world-clear (send (current-actions) select/stx world stx))
                           [World-cancel world]))
          (raise (make-voice-exn "Nothing forward.")))))
    
  ;; eval-Backward : World -> World
  (define (eval-Younger world)
    (let ([stx (find-pos-sibling-backward (World-cursor-position world) (World-syntax-list world))])
      (if stx
          (if (World-extension-base world)
              (motion-extension world (World-extension-base world) stx)
              (copy-struct World (world-clear (send (current-actions) select/stx world stx))
                           [World-cancel     world]))
          (raise (make-voice-exn "Nothing backward.")))))


  ;; eval-First : World -> World
  (define (eval-First world)
    (copy-struct World (world-clear (send (current-actions) first/selection world))
                   [World-cancel world]))

  ;; eval-Last : World -> World
  (define (eval-Last world)
    (copy-struct World (world-clear (send (current-actions) last/selection world))
                   [World-cancel     world]))

  (define ((not-here?/w world) stx) ((not-here? (World-cursor-position world)) stx))
  (define ((not-here? pos) stx) (not (= pos (syntax-position stx))))
  

  ;; eval-Forward: world -> world
  (define (eval-Forward world)

    ;; eval-Forward/normal: World -> World
    (define (eval-Forward/normal world)
      (let* ([stx-list (find-all-forward (lambda args true)
                                         (World-cursor-position world)
                                         (World-syntax-list world))]
             [stx/false (cond
                          [(empty? stx-list) false]
                          [(= 0 (World-selection-length world)) (first stx-list)]
                          [else
                           (find (not-here?/w world) stx-list)])])
        (if stx/false
            (copy-struct World (world-clear (send (current-actions) select/stx world stx/false))
                         [World-cancel world])
            (eval-line-motion world 'right))))

    ;; eval-Forward/extension: World -> World
    (define (eval-Forward/extension world)
      (let* ([end-pos (World-selection-end-position world)]
             [stx-list (World-syntax-list world)]
             [forward-stx/false (or (find-pos-sibling-forward end-pos stx-list)
                                    (find-pos-parent end-pos stx-list))])
        (if forward-stx/false
            (motion-extension world
                              (World-extension-base world)
                              forward-stx/false)
            world)))
    
    (if (World-extension-base world)
        (eval-Forward/extension world)
        (eval-Forward/normal world)))
  
  
  ;; eval-Backward: world -> world
  (define (eval-Backward world)
    
    ;; eval-Backward/normal: World -> World
    (define (eval-Backward/normal world)
      (let ([stx-list (find-all-backward (lambda args true)
                                         (World-cursor-position world)
                                         (World-syntax-list world))])
        (let ([stx/false (cond
                           [(empty? stx-list) false]
                           [(= 0 (World-selection-length world)) (first stx-list)]
                           [else (find (not-here?/w world) stx-list)])])
          (if stx/false
              (copy-struct World (world-clear (send (current-actions) select/stx world stx/false))
                           [World-cancel world])
              (eval-line-motion world 'left)))))

    ;; eval-Backward/extension: World -> World
    (define (eval-Backward/extension world)
      (let* ([pos (World-cursor-position world)]
             [stx-list (World-syntax-list world)]
             [backward-stx/false (or (find-pos-sibling-backward pos stx-list)
                              (find-pos-parent pos stx-list))])
        (if backward-stx/false
            (motion-extension world (World-extension-base world) backward-stx/false)
            world)))
    
    (if (World-extension-base world)
        (eval-Backward/extension world)
        (eval-Backward/normal world)))
  
  
  ;; motion-extension: world number syntax -> world
  ;; Given a base of origin and a syntax that we've moved to, extends
  ;; the selection in that direction.
  (define (motion-extension world base moved)
    (let*-values
        ([(w1 w2) (if (< base (syntax-position moved))
                      (values base (syntax-position moved))
                      (values (syntax-position moved) base))]
         [(p1 p2)
          (greatest-distinct-parents w1 w2
                                     (World-syntax-list world))]
         [(default) (or p1 p2 (find-pos-near base (World-syntax-list world)))]
         [(pos end)
          (if (and p1 p2)
              (values (syntax-position p1) (syntax-end-position p2))
              (values (syntax-position default) (syntax-end-position default)))]
         
         [(result) (send (current-actions) select/pos+end world 
                         pos end)])
      (copy-struct World (world-clear/extend result)
                   [World-cancel world])))
                   
    

  ;; eval-Delete : World -> World
  (define (eval-Delete world)
    (copy-struct World (world-clear (send (current-actions) delete world))
                 [World-cancel     world]
                 [World-undo       world]))
  
  ;; eval-Dedouble-Ellipsis : World -> World
  (define (eval-Dedouble-Ellipsis world)
    (success-message (send (current-actions) dedouble-ellipsis world) false))
  
  ;; TODO
  ;; eval-Bring : World -> World
  (define (eval-Bring world)
    (copy-struct World (world-clear (send (current-actions) bring world))
                 [World-cancel     world]
                 [World-undo       world]))        
  
  ;; TODO
  ;; eval-Push : World -> World
  (define (eval-Push world)
    (copy-struct World (world-clear (send (current-actions) push world))
                 [World-cancel     world]
                 [World-undo       world]))        

  ;; eval-Exchange : World -> World
  (define (eval-Exchange world)
    (copy-struct World (world-clear (send (current-actions) exchange world))
                 [World-cancel     world]))        

  ;; eval-Mark : World (union Loc false) (union What false) -> World
  (define (eval-Mark world loc/false what/false)
    (if what/false
        (let* ([loc-base    (World-cursor-position world)]
               [what-base   (eval-Loc  world (make-make-metric world metric-forward)  loc-base  loc/false)]
               [stx         (eval-What world (make-make-metric world metric-forward) what-base what/false)]
               [Next-f      (lambda (new-world)
                              (eval-Mark world loc/false (inc-what-distance what/false 1)))]
               [Previous-f  (lambda (new-world)
                              (eval-Mark world loc/false (dec-what-distance what/false 1)))])
          
          (copy-struct World (world-clear (send (current-actions) mark/stx world stx))
                       [World-Next-f     Next-f]
                       [World-Previous-f Previous-f]
                       [World-cancel     world]))
        (copy-struct World (world-clear (send (current-actions) selection->mark world))
                     [World-cancel     world])))

  ;; eval-UnMark : World -> World
  (define (eval-UnMark world)
    (copy-struct World (world-clear (send (current-actions) unmark world))
                 [World-cancel     world]))

  ;; eval-Copy : World -> World
  (define (eval-Copy world)
    (copy-struct World (world-clear (send (current-actions) copy world))
                 [World-cancel     world]))

  ;; eval-Cut : World -> World
  (define (eval-Cut world)
    (copy-struct World (world-clear (send (current-actions) cut world))
                 [World-cancel     world]
                 [World-undo       world]))

  ;; eval-Paste : World -> World
  (define (eval-Paste world)
    (copy-struct World (world-clear (send (current-actions) paste world))
                 [World-cancel     world]
                 [World-undo       world]))

  ;; TODO
  ;; eval-Definition : world -> world
  (define (eval-Definition world)
    (raise (make-voice-exn "Not supported yet")))

  ;; TODO
  ;; eval-Usage : world -> world
  (define (eval-Usage world)
    (raise (make-voice-exn "Not supported yet")))
  

  ;; eval-Enter : World -> World
  (define (eval-Enter world)
    (copy-struct World (world-clear (send (current-actions) enter/selection world))
                 [World-cancel     world]
                 [World-undo       world]))

  ;; eval-Join : World -> World
  (define (eval-Join world)
    (copy-struct World (world-clear (send (current-actions) join/selection world))
                 [World-cancel     world]
                 [World-undo       world]))

  ;; eval-Indent : World -> World
  (define (eval-Indent world)
    (send (current-actions) indent/selection world))
  
  
  ;; eval-Transpose : World -> World
  (define (eval-Transpose original-world)
    (queue-imperative-action
     (world-clear original-world)
     (lambda (world window)
       (send window transpose-sexp (pos->index (World-cursor-position world)))
       (copy-struct World world
                    [World-cancel original-world]
                    [World-undo original-world]))))
  
  (define (eval-Extend-Selection world)
    (world-clear/extend
     (if (World-extension-base world)
         (copy-struct World world
                      [World-extension-base #f]
                      [World-extension-length #f])
         (copy-struct World world
                      [World-extension-base (World-cursor-position world)]
                      [World-extension-length 0]))))
  
  
  ;; eval-Tag : World what -> (union World SwitchWorld)
  ;; Evaluates a tag command.
  (define (eval-Tag ast world what)

    ;; what->string: What -> string
    ;; Extracts the query string from what.
    (define (what->string what)
      ;; FIXME: do proper pattern matching on the data.
      (symbol->string
       (Symbol-Noun-symbol (WhatN-noun what))))
  
    ;; in-same-file?: world tag -> boolean
    ;; Is the file referred to by the tag the same as the one the world
    ;; is working on?
    (define (in-same-file? world tag)
      (equal? (tag-path tag) (World-path world)))
    
    ;; apply-tag: tag world -> world
    (define (goto-tag tag world)
      (cond
        [(in-same-file? world tag)
         (let* ([positions
                 (find-all-nearest 
                  (lambda (stx)
                    (in-syntax? (tag-position tag) stx))
                  (tag-position tag)
                  (World-syntax-list world))])
           (when (empty? positions)
             (raise (make-voice-exn "Not found")))
           (send (current-actions) 
                 select/stx
                 world
                 (first positions)))]
        [else
         ;; TODO: implement SwitchWorld semantics
         (make-SwitchWorld (tag-path tag) ast)]))
    
    ;; previous: circular-list -> circular-list
    (define (previous lst)
      (let loop ([L lst])
        (if (eq? (rest L) lst) L
            (loop (rest L)))))
    
    (define (apply-tags tags world)
      (define (Next-f world)
        (apply-tags (rest tags) world))
      (define (Previous-f world) 
        (apply-tags (previous tags) world))
      (let ([new-world (goto-tag (first tags) world)])
        (if (SwitchWorld? new-world)
            new-world
            (copy-struct World new-world
                         [World-Next-f     Next-f]
                         [World-Previous-f Previous-f]
                         [World-cancel     world]
                         [World-undo       world]))))
    

    ;; sort-for-NP: (listof tag) -> (circular-listof tag)
    ;; 
    ;; impose ordering: tags in the world we're in will come first, followed
    ;; by the rest of the tags.  All tags are ordered by position.
    (define (sort-for-NP tags world)

      (define (path-cmp a b)
        (cond
          [(string<? (path->string (tag-path a)) (path->string (tag-path b)))
           -1]
          [(string>? (path->string (tag-path a)) (path->string (tag-path b)))
           1]
          [else 0]))

      (define (numeric-cmp a b)
        (cond [(< a b) -1]
              [(> a b) 1]
              [else 0]))
      
      (define (cmp a b)
        (cond 
          [(= (path-cmp a b) -1) -1]
          [(= (path-cmp a b) 1) 1]
          [else (numeric-cmp (tag-position a) (tag-position b))]))

      (define sorted (quicksort tags cmp))

      (let-values ([(here there) (partition (lambda (t) (in-same-file? world t)) sorted)])
        (apply circular-list (append here there))))
          
    
    
    (define tags 
      (tag-library-lookup (get-current-tag-library) (what->string what)))

    
    (cond [(empty? tags)
           (raise (make-voice-exn "No match"))]
          [else
           (apply-tags (sort-for-NP tags world) world)])))
