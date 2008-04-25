(module structures mzscheme
  (require (lib "contract.ss")
           (lib "etc.ss")
           (lib "list.ss")
           (lib "struct.ss")
           (lib "mred.ss" "mred")
           (only (lib "1.ss" "srfi") find)
           "struct/datatype.ss"
           "dot-processing.ss"
           "utilities.ss"
           "rope.ss")
  
  
  
  ;; if macro? is true then we do not need to say open first
  ;; Template : symbol boolean string
  (define-struct Template (id macro? content))
  
  
  
  
  ;; World
  ;; rope : rope : the current content of the buffer
  ;; syntax-list/lazy : (union #f (list of syntax)) : the current content of the buffer, or #f if not computed yet
  
  ;; The selection cannot be a syntax because when we say Insert, there is no selection after, so no syntax object.
  ;; Furthermore, somebody can select something in the definition window which is not a subtree...
  ;; cursor-position : syntax position (subset of integer) : the current cursor location
  ;; selection-length : non-negative-integer : the current selection ; 0 when no selection
  
  ;; As the selection is not a syntax element, mark is not too.
  ;; mark-position : syntax position (subset of integer)
  ;; mark-length : non-negative-integer ; 0 when no mark
  
  ;; Next-f : World -> World : the action when Next is called
  ;; Previous-f : World -> World : id
  ;; Cancel : (union World false) : restore the selection and the mark as it was before the search
  ;; TODO: In the states Next-f and Previous-f, the given World is not useful
  ;; (because we rewind and take another path). So, should we still take a World as parameter?
  
  ;; undo : (union World false) : the previous World ; set only when an buffer editing is done
  ;; redo : (union World false) : the next World ; set only when an undo is performed
  
  ;; Magic-f : World -> World : print the next completion
  ;; Pass-f : World -> World : print the next template
  
  ;; again : (union ast false) : reexecute the previous ast.
  (define-struct World (rope
                        syntax-list/lazy
                        cursor-position
                        ;; target-column represents the column we try
                        ;; snapping to on vertical movement.
                        target-column
                        selection-length
                        mark-position
                        mark-length
                        Next-f
                        Previous-f
                        cancel
                        undo
                        redo
                        Magic-f
                        Pass-f
                        again
                        success-message
                        extension
                        ;; imperative-operations is a (listof imperative-op)
                        imperative-operations
                        markers
                        path) ;; read-only
    )
  
  
  ;; Here are some default functions for the World.
  (define ((default-Next-f) world)
    (raise (make-voice-exn "Next is not supported")))
  (define ((default-Previous-f) world)
    (raise (make-voice-exn "Previous is not supported")))
  (define ((default-Magic-f) world wrap?)
    (raise (make-voice-exn "Magic is not supported")))
  (define ((default-Pass-f) world template-wrap?)
    (raise (make-voice-exn "Pass is not supported")))
  
  
  
  ;; make-fresh-world: -> world
  ;; Creates a fresh new world.
  (define (make-fresh-world)
    (make-World (string->rope "")
                empty
                (index->syntax-pos 0)
                #f
                0
                (index->syntax-pos 0)
                0
                (default-Next-f)
                (default-Previous-f)
                false
                false
                false
                (default-Magic-f)
                (default-Pass-f)
                false
                ""
                #f
                empty
                empty
                (current-directory)))
  
  
  
  (define-struct extension (base
                            puck
                            puck-length))
  
  
  ;; SwitchWorld occurs if we need to switch focus from one file to another.
  (define-struct SwitchWorld (path ast))
  
  
  
  ;; World-selection-position : World -> pos
  (define World-selection-position World-cursor-position)
  
  ;; World-cursor-index : World -> non-negative-integer
  (define (World-cursor-index world)
    (syntax-pos->index (World-cursor-position world)))
  
  ;; World-selection-index : World -> non-negative-integer (== index)
  (define World-selection-index World-cursor-index)
  
  ;; World-mark-index : World -> non-negative-integer
  (define (World-mark-index world)
    (syntax-pos->index (World-mark-position world)))
  
  ;; World-selection-end-position : World -> pos
  (define (World-selection-end-position world)
    (+ (World-cursor-position world)
       (World-selection-length world)))
  
  ;; World-mark-end-position : World -> pos
  (define (World-mark-end-position world)
    (+ (World-mark-position world)
       (World-mark-length world)))
  
  ;; World-selection-end-index : World -> index (== non-negative-integer)
  (define (World-selection-end-index world)
    (syntax-pos->index (World-selection-end-position world)))
  
  ;; World-mark-end-index : World -> index (== non-negative-integer)
  (define (World-mark-end-index world)
    (syntax-pos->index (World-mark-end-position world)))
  
  ;; World-selection : World -> (union rope false)
  (define (World-selection world)
    (and (not (= (World-selection-length world) 0))
         (get-subrope/pos+len (World-rope world)
                              (World-cursor-position world)
                              (World-selection-length world))))
  
  ;; World-mark : World -> (union rope false)
  (define (World-mark world)
    (and (not (= (World-mark-length world) 0))
         (get-subrope/pos+len (World-rope world)
                              (World-mark-position world)
                              (World-mark-length world))))
  
  
  (define world-fn/c (World? . -> . World?))
  
  
  ;; FIXME: any imperative operation that uses raw positions should
  ;; really use markers, since those are more stable.
  
  ;; Imperative operations are applied to bring the old
  ;; world's state to the new world's state.
  (define-struct imperative-op ())
  
  ;; op:indent-range: indent the region starting from the mark.
  (define-struct (imperative-op:indent-range imperative-op) (mark len))
  
  ;; imperative-op:flash-last-sexp: flash the preceding s-exp.
  ;; Fixme: this should be using a mark!
  (define-struct (imperative-op:flash-last-sexp imperative-op) ())
  
  ;; imperative-op:move-cursor-position: moves the cursor in some direction.
  (define-struct (imperative-op:move-cursor-position imperative-op) (direction))
  
  ;; imperative-op:transpose: transposes the s-expression at world cursor position.
  (define-struct (imperative-op:transpose imperative-op) (original-world))
  
  ;; imperative-op:cleanup: cleans up the whitespace of the whole buffer.
  (define-struct (imperative-op:cleanup imperative-op) ())
  
  ;; imperative-op:cleanup: cleans up the whitespace between the two marks.
  (define-struct (imperative-op:cleanup-range imperative-op) (start-mark end-mark))
  
  ;; imperative-op:delete-range: delete the range of text.
  #;(define-struct (imperative-op:delete-range imperative-op) (start-pos end-pos))
  
  ;; imperative-op:insert-rope: insert the rope at the given position.
  #;(define-struct (imperative-op:insert-rope imperative-op) (rope pos))
  
  
  
  ;; queue-imperative-operation: World (world window [fixme: other stuff] -> world) -> World
  ;; Adds an imperative action that will be evaluated at the end of
  ;; evaluation.
  (define (queue-imperative-operation world fn)
    (copy-struct World world
                 [World-imperative-operations
                  (cons fn (World-imperative-operations world))]))
  
  
  
  ;; A Marker represents a position in the world rope that should be
  ;; robust under insertion, deletion, and replacement.
  (define-struct Marker (name index) #f)
  
  
  ;; new-marker: World index -> (values World symbol)
  (define world-new-marker
    (let ([counter 0])
      (lambda (world index)
        (let ([new-marker (make-Marker (string->symbol (format "mark~a" counter)) index)])
          (set! counter (add1 counter))
          (values (copy-struct World world
                               [World-markers (cons new-marker (World-markers world))])
                  (Marker-name new-marker))))))
  
  ;; world-clear-marker: world name -> world
  ;; Removes the marker from the world.
  (define (world-clear-marker world name)
    (copy-struct World world
                 [World-markers (filter
                                 (lambda (x)
                                   (not (symbol=? name (Marker-name x))))
                                 (World-markers world))]))
  
  
  ;; world-marker-position: World symbol -> number
  (define (world-marker-position world name)
    (let ([marker (find (lambda (elt)
                          (symbol=? name (Marker-name elt)))
                        (World-markers world))])
      (and marker (Marker-index marker))))
  
  
  ;; update-marks/insert: World index number -> World
  (define (update-markers/insert world index length)
    (define (update-mark marker)
      (cond
        [(< index (Marker-index marker))
         (copy-struct Marker marker
                      [Marker-index (+ length (Marker-index marker))])]
        [else marker]))
    (copy-struct World world
                 [World-markers (map* update-mark (World-markers world))]))
  
  ;; update-marks/delete: World index number -> World
  (define (update-markers/delete world index length)
    (define (update-mark marker)
      (cond
        ;; overlapping case
        [(< index (Marker-index marker) (+ index length))
         (copy-struct Marker marker
                      [Marker-index index])]
        
        ;; nonoverlapping case
        [(< index (Marker-index marker))
         (copy-struct Marker marker
                      [Marker-index (- (Marker-index marker) length)])]
        [else marker]))
    
    (copy-struct World world
                 [World-markers (map* update-mark (World-markers world))]))
  
  
  
  ;; update-marks/replace: World index number number -> World
  (define (update-markers/replace world index length replacing-length)
    (print-mem*
     'update-markers/replace
     (update-markers/insert
      (update-markers/delete world index length)
      index
      replacing-length)))
  
  
  
  ;; world-insert-rope: World index rope -> World
  (define (world-insert-rope world index a-rope)
    (let ([new-rope (insert-rope (World-rope world) index a-rope)])
      (update-markers/insert
       (copy-struct World world
                    [World-rope new-rope]
                    [World-syntax-list/lazy #f])
       index
       (rope-length a-rope))))
  
  
  
  ;; world-delete-rope: World index length -> World
  (define (world-delete-rope world index length)
    (let ([new-rope (delete-rope (World-rope world) index length)])
      (update-markers/delete
       (copy-struct World world
                    [World-rope new-rope]
                    [World-syntax-list/lazy #f])
       index
       length)))
  
  
  
  
  ;; world-replace-rope : world index rope int -> World
  (define (world-replace-rope world index tyt len)
    (let ([new-rope (replace-rope (World-rope world) index tyt len)])
      ;; FIXME: update marks
      (update-markers/replace
       (copy-struct World world
                    [World-rope new-rope]
                    [World-syntax-list/lazy #f])
       index
       len
       (rope-length tyt))))
  
  
  
  ;; World-syntax-list: World -> (listof syntax)
  ;; Forces the computation of the syntax list from the rope.
  (define (World-syntax-list a-world)
    (cond
      [(World-syntax-list/lazy a-world) => identity]
      [else
       (set-World-syntax-list/lazy! a-world
                                    (rope-parse-syntax (World-rope a-world)))
       (World-syntax-list/lazy a-world)]))
  
  
  
  
  
  
  ;; success-message : World string -> World
  ;; Replace the success-message of the world with a given string.
  (define (success-message world message)
    (copy-struct World world
                 [World-success-message message]))
  
  ;; missings
  ;; goto-definition
  ;; move to line, move to, move here
  ;; template == on parse (read-syntax) et on cherche les define & define-syntax
  (define commands
    (list 'Open
          'Open-Square
          'Close
          
          'Insert
          
          'Select
          'Search-Forward
          'Search-Backward
          'Search-Top
          'Search-Bottom
          
          'Holder
          'Holder-Forward
          'Holder-Backward
          
          'Next
          'Previous
          'Cancel
          'Undo
          'Redo
          
          'Magic
          'Magic-Bash
          'Magic-Wrap
          'Pass
          'Pass-Wrap
          
          'Again
          
          'Out
          'Non-blank-out
          'Down
          'Up
          'Forward
          'Backward
          'Younger
          'Older
          'First
          'Last
          
          'Delete
          'Dedouble-Ellipsis
          
          'Bring
          'Push
          
          'Exchange
          'Mark
          'UnMark
          
          'Copy
          'Cut
          'Paste
          
          'Definition
          'Usage
          
          'Enter
          'Join
          'Indent
          
          'Voice-Quote
          
          'Transpose
          'Tag
          'Extend-Selection
          'Stop-Extend-Selection))
  
  (define motion-commands
    ;; commands which must manipulate the cursor position
    ;; when there is no extended selection but manipulate the puck when there is one
    (list
     
     'Search-Forward
     'Search-Backward
     'Search-Top
     'Search-Bottom
     
     'Holder
     'Holder-Forward
     'Holder-Backward
     
     'Next
     'Previous
     
     'Out
     'Non-blank-out
     'Down
     'Up
     'Forward
     'Backward
     'Younger
     'Older
     'First
     'Last))
  
  
  ;; command?: symbol -> boolean
  ;; Returns true if the symbol represents a command.
  (define (command? symbol)
    (and (member symbol commands) #t))
  
  
  ;; motion-command: symbol -> boolean
  ;; Returns true if the symbol represents a motion command.
  (define (motion-command? symbol)
    (and (member symbol motion-commands) #t))
  
  
  (define-datatype Noun
    [Symbol-Noun (symbol)]
    [Rope-Noun (rope)])
  (provide-datatype/contract Noun
                             [Symbol-Noun (symbol?)]
                             [Rope-Noun (rope?)])
  
  
  
  (define-datatype What
    [WhatN (noun)]
    [WhatDN (distance noun)])
  (provide-datatype/contract What
                             [WhatN (Noun?)]
                             [WhatDN (integer? Noun?)])
  
  
  (define-datatype Where
    [After ()]
    [Before ()])
  (provide-datatype/contract Where
                             [After ()]
                             [Before ()])
  
  
  ;; A location is either an absolute position,
  ;; or relative to some thing.
  (define-datatype Location
    [Pos (p eol)]
    [Loc (where what)])
  (provide-datatype/contract Location
                             [Pos (integer? boolean?)]
                             [Loc (Where? (or/c false/c What?))])
  
  
  
  (define-datatype Verb-Content
    [Command (command)]
    [InsertRope-Cmd (rope)])
  (provide-datatype/contract Verb-Content
                             [Command (command?)]
                             [InsertRope-Cmd (rope?)])
  
  
  
  (define-datatype Protocol-Syntax-Tree
    [Verb (content location what)]
    [No-op ()])
  
  (provide-datatype/contract
   Protocol-Syntax-Tree
   [Verb (Verb-Content? (or/c false/c Location?) (or/c false/c What?))]
   [No-op ()])
  
  
  
  
  (provide/contract
   [struct Template ([id symbol?]
                     [macro? boolean?]
                     [content (listof string?)])]
   [struct World ([rope rope?]
                  [syntax-list/lazy (or/c false/c (listof syntax?))]
                  [cursor-position number?]
                  [target-column (or/c false/c number?)]
                  [selection-length number?]
                  [mark-position number?]
                  [mark-length number?]
                  [Next-f (World? . -> . World?)]
                  [Previous-f (World? . -> . World?)]
                  [cancel (or/c false/c World?)]
                  [undo (or/c false/c World?)]
                  [redo (or/c false/c World?)]
                  [Magic-f (World? boolean? . -> . World?)]
                  [Pass-f (World? boolean? . -> . World?)]
                  [again (or/c false/c Protocol-Syntax-Tree?)]
                  [success-message string?]
                  [extension (or/c false/c extension?)]
                  [imperative-operations (listof imperative-op?)]
                  [markers (listof Marker?)]
                  [path (or/c false/c path-string?)])]
   
   [struct SwitchWorld ([path path-string?]
                        [ast Protocol-Syntax-Tree?])]
   
   [struct extension ([base number?]
                      [puck number?]
                      [puck-length number?])]
   
   [make-fresh-world (-> World?)]
   [default-Next-f (-> (World? . -> . World?))]
   [default-Previous-f (-> (World? . -> . World?))]
   [default-Magic-f (-> (World? boolean? . -> . World?))]
   [default-Pass-f (-> (World? boolean? . -> . World?))]
   
   
   [World-selection-position
    (World? . -> . number?)]
   [World-cursor-index
    (World? . -> . number?)]
   [World-selection-index
    (World? . -> . number?)]
   [World-mark-index
    (World? . -> . number?)]
   [World-selection-end-position
    (World? . -> . number?)]
   [World-mark-end-position
    (World? . -> . number?)]
   [World-selection-end-index
    (World? . -> . number?)]
   [World-mark-end-index
    (World? . -> . number?)]
   [World-selection
    (World? . -> . (or/c false/c rope?))]
   [World-mark
    (World? . -> . (or/c false/c rope?))]
   
   
   [struct imperative-op ()]
   [struct (imperative-op:indent-range imperative-op)
           ([mark symbol?]
            [len natural-number/c])]
   [struct (imperative-op:flash-last-sexp imperative-op) ()]
   [struct (imperative-op:move-cursor-position imperative-op)
           ([direction
             (one-of/c 'home 'end 'right 'left 'up 'down)])]
   [struct (imperative-op:transpose imperative-op)
           ([original-world World?])]
   [struct (imperative-op:cleanup imperative-op) ()]
   [struct (imperative-op:cleanup-range imperative-op) 
           ([start-mark symbol?] 
            [end-mark symbol?])] 
   #;[struct (imperative-op:delete-range imperative-op)
             ([start-pos natural-number/c]
              [end-pos natural-number/c])]
   #;[struct (imperative-op:insert-rope imperative-op)
             ([rope rope?]
              [pos natural-number/c])]
   
   
   [queue-imperative-operation (World? imperative-op? . -> . World?)]
   
   [struct Marker ([name symbol?] 
                   [index natural-number/c])]
   
   [world-new-marker
    ((World? number?) . ->* . (World? symbol?))]
   [world-clear-marker
    (World? symbol? . -> . World?)]
   [world-marker-position
    (World? symbol? . -> . (or/c false/c number?))]
   
   
   [world-insert-rope (World? number? rope? . -> . World?)]
   [world-delete-rope (World? number? number? . -> . World?)]
   
   [World-syntax-list (World? . -> . (listof syntax?))]
   [world-replace-rope (World? number? rope? number? . -> . World?)]
   
   [success-message (World? string? . -> . World?)]
   
   [motion-command? (symbol? . -> . boolean?)]))

