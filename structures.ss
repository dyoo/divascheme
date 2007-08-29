(module structures mzscheme
  (require (lib "contract.ss")
           (lib "etc.ss")
           (lib "list.ss")
           (lib "struct.ss")
           (only (lib "1.ss" "srfi") find)
           "datatype/datatype.ss"
           "dot-processing.ss"
           "utilities.ss")
  
  
  (define previous-inspector (current-inspector))
  (current-inspector (make-inspector))
  
  ;;(require (planet "datatype.ss" ("dherman" "struct.plt" 1 4)))
  ;;(require (path->string (build-path "datatype" "struct.ss")))
  ;;(require  "datatype/datatype.ss")
  
  
  ;; Insert-World
  ;; Type to contain information for the stateful insertion mode.
  ;; start-index : index : index of the cursor
  ;; end-index : index : index of the end of the selection ; start-index <= end-index
  ;; text : string : content of the buffer
  (define-struct Previous-State (previous-world previous-command))
  (define-struct Insert-World (start-index end-index text previous-state))
  (provide (struct Previous-State (previous-world previous-command)))
  (provide (struct Insert-World (start-index end-index text previous-state)))
  #;
  (provide/contract (struct Previous-State ([previous-world Insert-World?]
                                            [previous-command string?])))
  #;
  (provide/contract (struct Insert-World ([start-index  natural-number/c]
                                          [end-index    natural-number/c]
                                          [text         string?]
					  [previous-state (union false/c Previous-State?)])))
  
  ;; sexp-string : string
  ;; Where the string can be (read) and has its parens in the first and last position
  
  ;; sexp-string? : string -> bool
  ;; Checks if the string is an sexp-string
  ;; ie. try to read and then check first and last characters
  (define (sexp-string? str)
    (with-handlers ([(lambda args true) (lambda args false)])
      (let ([stx-list (string->syntax-list str)]
            [lst (string->list str)])
        (and (not (empty? stx-list))
               (empty? (rest stx-list))
               (eq? #\( (first lst))
               (eq? #\) (first (reverse lst)))))))

  
  (provide sexp-string?)

  ;; if macro? is true then we do not need to say open first
  ;; Template : symbol boolean string
  (define-struct Template (id macro? content))
  (provide/contract (struct Template ([id       symbol?]
                                      [macro?   boolean?]
                                      [content (listof string?)])))
  ;; World
  ;; text : text : the current content of the buffer
  ;; syntax-list : list of syntax : the current content of the buffer

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
  (define-struct   World (text
                          syntax-list
                          cursor-position
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
                          extension-length
                          extension-base
                          imperative-actions
                          markers
                          path))      ;; read-only
  
  
  (provide (struct World (text
                          syntax-list
                          cursor-position
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
                          extension-length
                          extension-base
                          imperative-actions
                          markers
                          path)))
  ;; Not working.......
  #;
  (provide/contract (struct World ([text string?]
                                   [syntax-list (listof syntax?)]
                                   [cursor-position positive?]
                                   [target-column positive?]
                                   [selection-length natural-number/c]
                                   [mark-position positive?]
                                   [mark-length natural-number/c]
                                   [Next-f (-> World? World?)]
                                   [Previous-f (-> World? World?)]
                                   [cancel (union false/c World?)]
                                   [undo (union false/c World?)]
                                   [redo (union false/c World?)]
                                   [Magic-f (-> World? World?)]
                                   [Pass-f (-> World? World?)]
                                   [again (union false/c Protocol-Syntax-Tree?)]
                                   [success-message string?]
                                   [extension-length (union false/c integer?)]
                                   [extension-base (union false/c integer?)]
                                   [imperative-actions (listof (is-a?/c text% . -> . any))]
                                   [path path?])))

  (provide World-selection-position
           World-cursor-index
           World-selection-index
           World-mark-index
           World-selection-end-position
           World-mark-end-position
           World-selection-end-index
           World-mark-end-index
           World-selection
           World-mark)


  ;; SwitchWorld occurs if we need to switch focus from one file to another.
  (define-struct SwitchWorld (path ast))
  
  (provide (struct SwitchWorld (path ast)))

  
  
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
    (+ (World-cursor-position  world)
       (World-selection-length world)))

  ;; World-mark-end-position : World -> pos
  (define (World-mark-end-position world)
    (+ (World-mark-position world)
       (World-mark-length   world)))

  ;; World-selection-end-index : World -> index (== non-negative-integer)
  (define (World-selection-end-index world)
    (syntax-pos->index (World-selection-end-position world)))

  ;; World-mark-end-index : World -> index (== non-negative-integer)
  (define (World-mark-end-index world)
    (syntax-pos->index (World-mark-end-position world)))
  
  ;; World-selection : World -> (union string false)
  (define (World-selection world)
    (and (not (= (World-selection-length world) 0))
         (get-subtext/pos+len (World-text world)
                              (World-cursor-position world)
                              (World-selection-length world))))
  
  ;; World-mark : World -> (union string false)
  (define (World-mark world)
    (and (not (= (World-mark-length world) 0))
         (get-subtext/pos+len (World-text world)
                              (World-mark-position world)
                              (World-mark-length world))))
    
  
  (provide queue-imperative-action)
  ;; queue-imperative-action: World (world window -> world) -> World
  ;; Adds an imperative action that will be evaluated at the end of
  ;; evaluation.
  (define (queue-imperative-action world fn)
    (copy-struct World world
                 [World-imperative-actions
                  (cons fn (World-imperative-actions world))]))
  
  
  
  ;; A Marker represents a position in the world text that should be
  ;; robust under insertion, deletion, and replacement.
  (define-struct Marker (name index) #f)
  
  (provide world-new-marker)
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
  (provide world-clear-marker)
  (define (world-clear-marker world name)
    (copy-struct World world
                 [World-markers (filter
                                 (lambda (x)
                                   (not (symbol=? name (Marker-name x))))
                                 (World-markers world))]))
  
  
  (provide world-marker-position)
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
  
  
  (provide world-insert-text)
  ;; world-insert-text: World index text -> World
  (define (world-insert-text world index text)
    (let ([new-text (insert-text (World-text world) index text)])
      (update-markers/insert
       (copy-struct World world
                    [World-text new-text]
                    [World-syntax-list (parse-syntax/dot new-text)])
       index
       (string-length text))))
  
  
  
  (provide world-delete-text)
  ;; world-delete-text: World index text -> World
  (define (world-delete-text world index length)
    (let ([new-text (delete-text (World-text world) index length)])
      (update-markers/delete
       (copy-struct World world
                    [World-text new-text]
                    [World-syntax-list (parse-syntax/dot new-text)])
       index
       length)))
  
  
  (provide world-replace-text)
  ;; world-replace-text : world index string int -> string
  (define (world-replace-text world index tyt len)
    (print-mem*
     'world-replace-text
     (let ([new-text (replace-text (World-text world) index tyt len)])
       ;; FIXME: update marks
       (update-markers/replace
        (copy-struct World world
                     [World-text new-text]
                     [World-syntax-list (parse-syntax/dot new-text)])
        index
        len
        (string-length tyt)))))
  
  
  (provide success-message)
  ;; success-message : World string -> World
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
          'Up
          'Down
          'Up
          'Down
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
          'Extend-Selection))
  


  (define command?
    (lambda (symbol) (member symbol commands)))


  (define-datatype Noun
    [Symbol-Noun (symbol)]
    [The-Symbol (symbol)])
  
  (provide-datatype/contract Noun
     [Symbol-Noun (symbol?)]
     [The-Symbol (symbol?)])


  (define-datatype What
    [WhatN  (noun)]
    [WhatDN (distance noun)])

  (provide-datatype/contract What
     [WhatN  (Noun?)]
     [WhatDN (integer? Noun?)])


  (define-datatype Where
    [After ()]
    [Before ()])

  (provide-datatype/contract Where
    [After ()]
    [Before ()])


  (define-datatype Location
    [Loc (where what)])

  (provide-datatype/contract Location
    [Loc (Where? (union false/c What?))])


  (define-datatype Verb-Content
    [Command (command)]
    [Symbol-Cmd (symbol)])

  (provide-datatype/contract Verb-Content
    [Command (command?)]
    [Symbol-Cmd (symbol?)])


  (define-datatype Protocol-Syntax-Tree
    [Verb (content location what)])

  (provide-datatype/contract Protocol-Syntax-Tree
    [Verb (Verb-Content? (union false/c Location?) (union false/c What?))])


  (define-struct ChangeWorld (path))
  (provide/contract (struct ChangeWorld ([path path?])))


  (current-inspector previous-inspector))