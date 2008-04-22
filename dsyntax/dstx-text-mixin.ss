(module dstx-text-mixin mzscheme
  
  ;
  ; Provides a text mixin that keeps track of an ast and allows
  ; insertions and deletions.
  ;
  ; Each element of the ast is marked with a 'local-id property.
  ; This is used to uniquely identify nodes in the dstx tree.
  ;
  ; The invariant we're trying to maintain is that the cursor that
  ; represents our structured view is up to date with what's on screen,
  ; except in the moment when unstructured edits happen.  The hard
  ; work here is turning those unstructured edits into an equivalent
  ; sequence of structured operations.
  ;
  
  
  (require (lib "class.ss")
           (lib "port.ss")
           (lib "mred.ss" "mred")
           (lib "plt-match.ss")
           (prefix parser: "parse-plt-scheme.ss")
           (prefix cursor: "cursor.ss")
           (prefix struct: "struct.ss"))
  
  
  (provide dstx-text-mixin dstx-text<%> dstx-cursor<%>)
  
  
  (define dstx-text<%> (interface (editor<%>)
                         get-toplevel-dstxs
                         
                         get-dstx-cursor-class
                         get-dstx-cursor
                         
                         dstx-parsing-enabled?
                         enable-dstx-parsing
                         disable-dstx-parsing
                         
                         decorate-new-dstx
                         
                         on-structured-insert-before
                         on-structured-insert-after
                         on-structured-delete
                         after-structured-insert-before
                         after-structured-insert-after
                         after-structured-delete))
  
  (define dstx-cursor<%> (interface ()
                           get-functional-cursor
                           
                           cursor-toplevel-dstxs
                           cursor-dstx
                           cursor-line
                           cursor-col
                           cursor-pos
                           cursor-endpos
                           property-ref
                           property-set!
                           
                           focus-in!
                           focus-in/no-snap!
                           focus-out!
                           focus-older!
                           focus-older/no-snap!
                           focus-oldest!
                           focus-younger!
                           focus-younger/no-snap!
                           focus-youngest!
                           focus-successor!
                           focus-successor/no-snap!
                           focus-predecessor!
                           focus-predecessor/no-snap!
                           focus-toplevel!
                           focus-container!
                           focus-pos!
                           
                           insert-before!
                           insert-after!
                           delete!))
  
  
  ;; This makes dstx-text-mixin and dstx-cursor friends.  The following methods
  ;; can only be called by classes defined in this module.
  (define-member-name parse-between (generate-member-key))
  (define-member-name begin-dstx-edit-sequence (generate-member-key))
  (define-member-name end-dstx-edit-sequence (generate-member-key))
  (define-member-name get-version (generate-member-key))
  (define-member-name increment-version! (generate-member-key))
  (define-member-name get-cursor-for-editing (generate-member-key))
  (define-member-name set-cursor-for-editing (generate-member-key))
  
  ;; Cursor methods.
  (define-member-name insert-before!/sync-with-text (generate-member-key))
  (define-member-name insert-after!/sync-with-text (generate-member-key))
  (define-member-name delete!/sync-with-text (generate-member-key))
  
  
  ;; next-local-id: -> number
  ;; Returns the next local clock.
  (define next-local-id
    (let ([current-local-id 0])
      (lambda ()
        (set! current-local-id (add1 current-local-id))
        current-local-id)))
  
  
  ;; dstx-attach-local-ids: dstx -> dstx
  ;; Attach the local-id property to each dstx, deeply.
  (define (dstx-attach-local-ids a-dstx)
    (struct:dstx-deepmap
     (lambda (a-dstx)
       (cond [(member 'local-id (struct:dstx-property-names a-dstx))
              a-dstx]
             [else
              (struct:dstx-property-set a-dstx 'local-id (next-local-id))]))
     a-dstx))
  
  
  
  
  
  
  
  ;; dstx-text-mixin: text% -> text%
  ;; Adds in some functionality specific to dstx maintenance.
  (define (dstx-text-mixin super%)
    (class* super% (dstx-text<%>)
      (inherit get-start-position
               get-end-position
               set-position
               can-insert?
               insert
               can-delete?
               delete
               split-snip
               find-snip
               get-snip-position
               begin-edit-sequence
               end-edit-sequence)
      
      (super-new)
      
      
      (define parsing-enabled? #f)
      
      (define/public (dstx-parsing-enabled?)
        parsing-enabled?)
      
      (define/public (enable-dstx-parsing)
        (set! parsing-enabled? #t)
        (reparse-all-dstxs!))
      
      (define/public (disable-dstx-parsing)
        (set! parsing-enabled? #f))
      
      
      (define version 0)
      (define/public (get-version)
        version)
      
      (define/public (increment-version!)
        (set! version (add1 version)))
      
      
      ;; dstx-cursors constructed by this text will
      ;; have the following class.
      (define/pubment (get-dstx-cursor-class base-class)
        (inner base-class get-dstx-cursor-class base-class))
      
      
      ;; Returns a toplevel cursor into the dstx.
      ;; Operations performed with the cursor will be reflected
      ;; back on screen.
      (define/public (get-dstx-cursor)
        (new (get-dstx-cursor-class dstx-cursor%) [text this]))
      
      
      ;; We keep a dstx-cursor that's used primarily for the
      ;; unstructured edit stuff, and for faster synchronization
      ;; with other cursors.
      ;; Warning: do NOT expose this to the outside world.
      ;; This is the very last used cursor that edited this window.
      (define cursor-for-editing (get-dstx-cursor))
      
      ;; Protected.
      (define/public (get-cursor-for-editing)
        cursor-for-editing)
      
      ;; Protected.
      (define/public (set-cursor-for-editing a-cursor)
        (set! cursor-for-editing a-cursor))
      
      
      
      ;; get-toplevel-dstxs: -> (listof dstx)
      ;; Returns the top dstxs.
      (define/public (get-toplevel-dstxs)
        (send cursor-for-editing cursor-toplevel-dstxs))
      
      ;; dstx-edit-depth: natural-number
      ;; Maintains how deeply into a begin-dstx-edit-sequence we're in.
      (define dstx-edit-depth 0)
      
      
      ;; begin-dstx-edit-sequence: -> void
      ;; Protected.
      (define/public (begin-dstx-edit-sequence)
        (set! dstx-edit-depth (add1 dstx-edit-depth)))
      
      
      ;; end-dstx-edit-sequence: -> void
      ;; Protected.
      (define/public (end-dstx-edit-sequence)
        (set! dstx-edit-depth (sub1 dstx-edit-depth)))
      
      
      ;; in-dstx-edit-sequence: -> boolean
      ;; Returns true if we're currently being edited by a cursor.
      (define (in-dstx-edit-sequence?)
        (> dstx-edit-depth 0))
      
      
      ;; decorate-new-dstx: dstx -> dstx
      ;; Add some properties to any new dstx introduced into the text.
      ;; By default, we attach local ids.  Augment this to add additional
      ;; properties.
      (define/pubment (decorate-new-dstx a-dstx)
        (let ([decorated-dstx (dstx-attach-local-ids a-dstx)])
          (inner decorated-dstx decorate-new-dstx decorated-dstx)))
      
      (define/pubment (on-structured-insert-after a-functional-cursor a-dstx)
        (inner (void) on-structured-insert-after a-functional-cursor a-dstx))
      
      (define/pubment (on-structured-insert-before a-functional-cursor a-dstx)
        (inner (void) on-structured-insert-before a-functional-cursor a-dstx))
      
      (define/pubment (on-structured-delete a-functional-cursor)
        (inner (void) on-structured-delete a-functional-cursor))
      
      (define/pubment (after-structured-insert-before a-functional-cursor)
        (inner (void) after-structured-insert-before a-functional-cursor))
      
      (define/pubment (after-structured-insert-after a-functional-cursor)
        (inner (void) after-structured-insert-after a-functional-cursor))
      
      (define/pubment (after-structured-delete a-functional-cursor)
        (inner (void) after-structured-delete a-functional-cursor))
      
      
      ;; after-delete: number number -> void
      (define/augment (after-delete start-pos len)
        (inner (void) after-delete start-pos len)
        (cond
          [(not parsing-enabled?)
           (void)]
          [(in-dstx-edit-sequence?)
           (void)]
          [else
           (handle-possibly-unstructured-delete start-pos len)]))
      
      
      ;; after-insert: number number -> void
      (define/augment (after-insert start-pos len)
        (inner (void) after-insert start-pos len)
        (cond
          [(not parsing-enabled?)
           (void)]
          [(in-dstx-edit-sequence?)
           (void)]
          [else
           #;(printf "after-insert ~a ~a: ~s~n" start-pos len (send this get-text start-pos (+ start-pos len)))
           (handle-possibly-unstructured-insert start-pos len)]))
      
      
      ;; with-unstructured-editing-repair: (-> X) -> X
      ;; wrapper to prevent recurrence of unstructured edit handlers
      ;; This tells the system that whatever textual changes are occuring
      ;; internally.
      (define (with-unstructured-editing-repair thunk)
        (dynamic-wind (lambda ()
                        (begin-edit-sequence)
                        (begin-dstx-edit-sequence))
                      thunk
                      (lambda ()
                        (end-edit-sequence)
                        (end-dstx-edit-sequence))))
      
      
      ;; handle-possibly-unstructured-delete: number number -> void
      ;; Given some unstructured insert, try to translate the effect
      ;; into a sequence of structured inserts and deletes.
      (define (handle-possibly-unstructured-delete start-pos len)
        (with-unstructured-editing-repair
         (lambda ()
           (define-values
             (original-start-position original-end-position)
             (values (get-start-position) (get-end-position)))
           (let loop ([len len])
             (when (> len 0)
               (send cursor-for-editing focus-container! start-pos)
               (let* ([deleted-start (max start-pos
                                          (send cursor-for-editing cursor-pos))]
                      [deleted-end (min (+ start-pos len)
                                        (send cursor-for-editing cursor-endpos))]
                      [deleted-width (- deleted-end deleted-start)])
                 (let ([new-dstxs
                        (parse-between (send cursor-for-editing cursor-pos)
                                       (+ (send cursor-for-editing cursor-pos)
                                          (- (send cursor-for-editing cursor-endpos)
                                             (send cursor-for-editing cursor-pos)
                                             deleted-width)))])
                   (for-each (lambda (a-dstx)
                               (send cursor-for-editing
                                     insert-after!/sync-with-text a-dstx #f)
                               (send cursor-for-editing focus-younger/no-snap!))
                             (reverse new-dstxs))
                   (send cursor-for-editing delete!/sync-with-text #f)
                   (loop (- len deleted-width))))))
           (set-position original-start-position
                         original-end-position
                         #f #f 'local))))
      
      
      
      
      
      ;; handle-possibly-unstructured-insert: number number -> void
      ;; When the text changes without explicit structured operations, we
      ;; must maintain semi-structure.
      (define (handle-possibly-unstructured-insert start-pos len)
        (define-values
          (original-start-position original-end-position)
          (values (get-start-position) (get-end-position)))
        (with-unstructured-editing-repair
         (lambda ()
           (cond [(inserting-at-buffer-end? cursor-for-editing start-pos)
                  (handle-unstructured-insertion-at-end cursor-for-editing start-pos len)]
                 [else
                  (handle-unstructured-insertion-in-container cursor-for-editing start-pos len)])
           (set-position original-start-position original-end-position #f #f 'local))))
      
      
      
      
      ;; inserting-at-buffer-end?: cursor number -> boolean
      ;; True only if we're inserting at the very end of something.
      ;; The only time that happens is if focus-container fails.
      (define (inserting-at-buffer-end? a-cursor insert-start-pos)
        (let ([fcursor (send a-cursor get-functional-cursor)])
          (cond [(cursor:focus-container fcursor insert-start-pos)
                 #f]
                [else #t])))
      
      
      ;; insert-new-dstxs-after/no-sync: cursor (listof dstx) -> void
      ;; Insert the sequence of dstxs after the current focus, preserving
      ;; original focus.  Does not adjust on-screen text.
      (define (insert-new-dstxs-after/no-sync a-cursor new-dstxs)
        (for-each (lambda (new-dstx)
                    (send a-cursor insert-after!/sync-with-text new-dstx #f)
                    (send a-cursor focus-younger/no-snap!))
                  (reverse new-dstxs)))
      
      
      ;; insert-new-dstxs-before/no-sync: cursor (listof dstx) -> void
      ;; Insert a sequence of dstxs before the current focus, preserving
      ;; original focus.  Does not adjust on-screen text.
      (define (insert-new-dstxs-before/no-sync a-cursor new-dstxs)
        (for-each (lambda (new-dstx)
                    (send a-cursor insert-before!/sync-with-text new-dstx #f)
                    (send a-cursor focus-older/no-snap!))
                  new-dstxs))
      
      
      ;; handle-unstructured-insertion-at-end: cursor number number -> void
      ;; Given an ad-hoc insertion at the end of the buffer, account
      ;; for that and adjust our structures accordingly.
      (define (handle-unstructured-insertion-at-end a-cursor start-pos len)
        (send a-cursor focus-toplevel!)
        (send a-cursor focus-oldest!)
        (cond
          ;; subtle: if the focus is on an atom, do some special
          ;; processing.
          [(dstx-atomic? (send a-cursor cursor-dstx))
           (unstructured-insert-on-focused-atom a-cursor start-pos len)]
          [else
           (let ([new-dstxs (parse-between start-pos (+ start-pos len))])
             (insert-new-dstxs-after/no-sync a-cursor new-dstxs))]))
      
      
      ;; dstx-atomic?: dstx -> boolean
      ;; Returns true if the dstx is either an atom or a special atom.
      (define (dstx-atomic? a-dstx)
        (or (struct:atom? a-dstx)
            (struct:special-atom? a-dstx)))
      
      
      ;; Assuming focus is currently on the atom, do the insert that
      ;; best preserves the atom.  We don't try to preserve if
      ;; the focused atom is a special unparsed atom, though.
      (define (unstructured-insert-on-focused-atom a-cursor start-pos len)
        (cond
          ;; If we're adding whitespace before us, just add it before us.
          [(and (= start-pos (send a-cursor cursor-pos))
                (not (special-atom-unparsed? (send a-cursor cursor-dstx)))
                (all-whitespace-between? start-pos (+ start-pos len)))
           (let ([new-dstxs (parse-between start-pos (+ start-pos len))])
             (insert-new-dstxs-before/no-sync a-cursor new-dstxs))]
          
          ;; And if the whitespace is after us, do a similar thing.
          [(and (= start-pos (send a-cursor cursor-endpos))
                (not (special-atom-unparsed? (send a-cursor cursor-dstx)))
                (all-whitespace-between? start-pos (+ start-pos len)))
           (let ([new-dstxs (parse-between start-pos (+ start-pos len))])
             (insert-new-dstxs-after/no-sync a-cursor new-dstxs))]
          
          ;; Otherwise, delete the old atom, and introduce a reparsed thing in its place
          [else
           (let ([new-dstxs (parse-between (send a-cursor cursor-pos)
                                           (+ (send a-cursor cursor-endpos)
                                              len))])
             (insert-new-dstxs-after/no-sync a-cursor new-dstxs)
             (send a-cursor delete!/sync-with-text #f))]))
      
      ;; handle-unstructured-insertion-in-container: cursor number number -> void
      (define (handle-unstructured-insertion-in-container a-cursor start-pos len)
        (send a-cursor focus-container! start-pos)
        (match (send a-cursor cursor-dstx)
          [(struct struct:atom (props content))
           (unstructured-insert-on-focused-atom a-cursor start-pos len)]
          
          [(struct struct:special-atom (props content width))
           (unstructured-insert-on-focused-atom a-cursor start-pos len)]
          
          [(struct struct:space (props content))
           ;; Subtle: if the very previous expression is an atom, the insert is at its end,
           ;; and the insertion was not whitespace, then update that atom and reparse it.
           ;; Otherwise, do an insert-before, preserving existing dstxs.
           (let ([fcursor (send a-cursor get-functional-cursor)])
             (cond [(and (cursor:focus-younger/no-snap fcursor)
                         (dstx-atomic? (struct:cursor-dstx (cursor:focus-younger/no-snap fcursor)))
                         (= (cursor:cursor-endpos (cursor:focus-younger/no-snap fcursor))
                            start-pos))
                    (send a-cursor focus-younger/no-snap!)
                    (unstructured-insert-on-focused-atom a-cursor start-pos len)]
                   [else
                    (let ([new-dstxs (parse-between start-pos (+ start-pos len))])
                      (insert-new-dstxs-before/no-sync a-cursor new-dstxs))]))]
          
          [(struct struct:fusion (props prefix children suffix))
           (let ([fcursor (send a-cursor get-functional-cursor)])
             (cond
               ;; if they're editing at the front, right next to an atom,
               ;; do the edit on that atom instead.
               [(and (cursor:focus-younger/no-snap fcursor)
                     (dstx-atomic? (struct:cursor-dstx (cursor:focus-younger/no-snap fcursor)))
                     (= (cursor:cursor-endpos (cursor:focus-younger/no-snap fcursor))
                        start-pos))
                (send a-cursor focus-younger/no-snap!)
                (unstructured-insert-on-focused-atom a-cursor start-pos len)]
               
               ;; If their insertion is at the last child of this fusion, add them
               ;; as a child of us, after our previous oldest element.
               [(and (cursor:focus-oldest (cursor:focus-in/no-snap fcursor))
                     (= (cursor:cursor-endpos (cursor:focus-oldest (cursor:focus-in/no-snap fcursor)))
                        start-pos))
                (cond
                  [(dstx-atomic? (struct:cursor-dstx (cursor:focus-oldest (cursor:focus-in/no-snap fcursor))))
                   (send a-cursor focus-in/no-snap!)
                   (send a-cursor focus-oldest!)
                   (unstructured-insert-on-focused-atom a-cursor start-pos len)]
                  [else
                   (let ([new-dstxs (parse-between start-pos (+ start-pos len))])
                     (send a-cursor focus-in/no-snap!)
                     (send a-cursor focus-oldest!)
                     (insert-new-dstxs-after/no-sync a-cursor new-dstxs))])]
               
               ;; otherwise, just parse the new structure and insert before us.
               [else
                (let ([new-dstxs (parse-between start-pos (+ start-pos len))])
                  (insert-new-dstxs-before/no-sync a-cursor new-dstxs))]))]))
      
      
      ;; all-whitespace-between?: number number -> boolean
      ;; Returns true if everything between the start-pos and end-pos is
      ;; whitespace.
      (define (all-whitespace-between? start-pos end-pos)
        (andmap struct:space? (parse-between start-pos end-pos)))
      
      
      
      
      ;; load-file: string -> void
      (define/override load-file
        (case-lambda
          [(filename)
           (general-load-file filename 'guess #t)]
          [(filename format)
           (general-load-file filename format #t)]
          [(filename format show-errors?)
           (general-load-file filename format show-errors?)]))
      
      ;; general-load-file: string symbol boolean -> void
      (define (general-load-file filename format show-errors?)
        (dynamic-wind (lambda () (begin-dstx-edit-sequence))
                      (lambda () (super load-file filename format show-errors?))
                      (lambda () (end-dstx-edit-sequence)))
        (cond [(not parsing-enabled?)
               (void)]
              [else
               (reparse-all-dstxs!)]))
      
      
      
      ;; reparse-all-dstxs!: -> void
      ;; reparses the entire buffer.
      (define/public (reparse-all-dstxs!)
        (increment-version!)
        (send cursor-for-editing reparse!))
      
      
      ;; Returns true if this atom is special and unparsed
      (define (special-atom-unparsed? a-dstx)
        (struct:dstx-property-ref a-dstx 'unparsed (lambda () #f)))
      
      
      ;; parse-between: number number -> (listof dstx)
      ;; Parse the text between start and end.  If the content
      ;; is unparseable, return a list containing a new fusion
      ;; marked with the property 'unparsed.
      (define/public (parse-between start end)
        (with-handlers ([exn:fail? (lambda (exn)
                                     (parse-between/unparsed start end))])
          (let* ([ip (parser:open-input-text this start end)]
                 [dstxs (parser:parse-port ip)])
            (map (lambda (a-dstx) (decorate-new-dstx a-dstx)) dstxs))))
      
      
      
      ;; parse-between/unparsed: start end -> (listof dstx)
      ;; Returns a list of dstx objects that represent the unparsed
      ;; elements.  This is a catch-all for cases where we have no idea how to
      ;; parse something.
      (define (parse-between/unparsed start end)
        (let ([result
               (reverse (map (lambda (a-snip)
                               (struct:dstx-property-set
                                (decorate-new-dstx
                                 (struct:new-special-atom
                                  a-snip
                                  (send a-snip get-count)))
                                'unparsed
                                #t))
                             (get-snips/rev start end)))])
          result))
      
      
      ;; get-snips/rev: start end -> (listof snip)
      ;; Returns a list of copied snips in reverse order between
      ;; start and end.
      (define (get-snips/rev start end)
        (split-snip start)
        (split-snip end)
        (let loop ([snips/rev '()]
                   [a-snip
                    (find-snip start 'after-or-none)])
          (cond
            [(or (not a-snip)
                 (>= (get-snip-position a-snip)
                     end))
             snips/rev]
            [else
             (loop (cons (send a-snip copy) snips/rev)
                   (send a-snip next))])))))
  
  
  ;; make-toplevel-functional-cursor: dstx-text<%> -> dstx-cursor
  ;; Creates the toplevel cursor, ensuring that every dstx in there has a local id.
  (define (make-toplevel-functional-cursor a-text)
    (let* ([dstxs (send a-text parse-between 0 (send a-text last-position))]
           [a-cursor (cursor:make-toplevel-cursor dstxs)])
      (cursor:replace
       a-cursor
       (send a-text decorate-new-dstx (struct:cursor-dstx a-cursor)))))
  
  
  ;; empty-toplevel-functional-cursor: dstx-text<%> -> dstx-cursor
  ;; Creates an empty toplevel cursor.
  (define (empty-toplevel-functional-cursor a-text)
    (let ([f-cursor (cursor:make-toplevel-cursor '())])
      (cursor:replace
       f-cursor
       (send a-text decorate-new-dstx (struct:cursor-dstx f-cursor)))))
  
  
  ;; a dstx-cursor% provides a mutable interface to the functions
  ;; defined in cursor.ss.  Changes made with this dstx-cursor will
  ;; reflect onto the text.
  (define dstx-cursor%
    (class* object% (dstx-cursor<%>)
      (super-new)
      
      (init text)
      (define current-text text)
      
      (define current-version 0)
      
      ;; f-cursor is a functional cursor that we reassign for all the
      ;; operations here.
      (define f-cursor (empty-toplevel-functional-cursor current-text))
      
      (define/public (reparse!)
        (set! f-cursor (make-toplevel-functional-cursor current-text))
        (mark-this-cursor-as-up-to-date-editor!))
      
      
      (define (reuse-editing-fcursor!)
        (set! f-cursor
              (send (send current-text get-cursor-for-editing)
                    get-functional-cursor)))
      
      
      ;; resync!: -> void
      ;; Refresh the cursor's view of the AST, trying our best to preserve
      ;; the focus.
      ;;
      ;; All of the public-facing functions should first call this.
      ;;
      ;; When the AST is modified, we need to correct our out-of-date view
      ;; of the AST.  We assume the last-editing cursor is the most up-to-date,
      ;; although it still might be out-of-sync with the content of the text buffer
      ;; if unstructured edits are happening.
      ;;
      ;; Protected.
      (define/public (resynchronize-with-main-editing-cursor!)
        (when (and (send current-text dstx-parsing-enabled?)
                   (not (= current-version (send current-text get-version))))
          (let ([old-local-id (cursor:property-ref f-cursor 'local-id)]
                [old-pos (cursor:cursor-pos f-cursor)])
            ;; If the previous set is unsound, let's go back to the
            ;; slow-but-safe option.
            (cond
              [(eq? this (send current-text get-cursor-for-editing))
               (void)]
              [else
               ;; optimization: try to reuse the cursor that was last used for editing.
               (reuse-editing-fcursor!)])
            (cond
              [(cursor:focus-find-dstx
                f-cursor
                (lambda (a-dstx)
                  (= (struct:dstx-property-ref a-dstx 'local-id)
                     old-local-id)))
               =>
               (lambda (new-cursor)
                 (set! f-cursor new-cursor))]
              
              [(cursor:focus-pos f-cursor old-pos)
               =>
               (lambda (new-cursor)
                 (set! f-cursor new-cursor))])
            (set! current-version (send current-text get-version)))))
      
      
      (define/public (get-functional-cursor)
        (resynchronize-with-main-editing-cursor!)
        f-cursor)
      
      ;; Getters
      (define/public (cursor-dstx)
        (resynchronize-with-main-editing-cursor!)
        (struct:cursor-dstx f-cursor))
      
      (define/public (cursor-line)
        (resynchronize-with-main-editing-cursor!)
        (cursor:cursor-line f-cursor))
      
      (define/public (cursor-col)
        (resynchronize-with-main-editing-cursor!)
        (cursor:cursor-col f-cursor))
      
      (define/public (cursor-pos)
        (resynchronize-with-main-editing-cursor!)
        (cursor:cursor-pos f-cursor))
      
      (define/public (cursor-endpos)
        (resynchronize-with-main-editing-cursor!)
        (cursor:cursor-endpos f-cursor))
      
      (define/public (cursor-toplevel-dstxs)
        (resynchronize-with-main-editing-cursor!)
        (cursor:cursor-toplevel-dstxs f-cursor))
      
      ;; Property get and set
      (define/public (property-ref a-name)
        (resynchronize-with-main-editing-cursor!)
        (cursor:property-ref f-cursor a-name))
      
      (define/public (property-set! a-name a-val)
        (resynchronize-with-main-editing-cursor!)
        (cursor:property-set f-cursor a-name a-val))
      
      
      (define-syntax (set-cursor/success stx)
        (syntax-case stx ()
          [(_ a-cursor new-cursor-val)
           (syntax/loc stx
             (begin (unless new-cursor-val
                      (error 'set-cursor "movement failed"))
                    (set! a-cursor new-cursor-val)))]))
      
      
      ;; Focusers
      (define/public (focus-in!)
        (resynchronize-with-main-editing-cursor!)
        (set-cursor/success f-cursor (cursor:focus-in f-cursor)))
      
      (define/public (focus-in/no-snap!)
        (resynchronize-with-main-editing-cursor!)
        (set-cursor/success f-cursor (cursor:focus-in/no-snap f-cursor)))
      
      (define/public (focus-out!)
        (resynchronize-with-main-editing-cursor!)
        (set-cursor/success f-cursor (cursor:focus-out f-cursor)))
      
      (define/public (focus-older!)
        (resynchronize-with-main-editing-cursor!)
        (set-cursor/success f-cursor (cursor:focus-older f-cursor)))
      
      (define/public (focus-older/no-snap!)
        (resynchronize-with-main-editing-cursor!)
        (set-cursor/success f-cursor (cursor:focus-older/no-snap f-cursor)))
      
      (define/public (focus-oldest!)
        (resynchronize-with-main-editing-cursor!)
        (set-cursor/success f-cursor (cursor:focus-oldest f-cursor)))
      
      (define/public (focus-younger!)
        (resynchronize-with-main-editing-cursor!)
        (set-cursor/success f-cursor (cursor:focus-younger f-cursor)))
      
      (define/public (focus-younger/no-snap!)
        (resynchronize-with-main-editing-cursor!)
        (set-cursor/success f-cursor (cursor:focus-younger/no-snap f-cursor)))
      
      (define/public (focus-youngest!)
        (resynchronize-with-main-editing-cursor!)
        (set-cursor/success f-cursor (cursor:focus-youngest f-cursor)))
      
      (define/public (focus-successor!)
        (resynchronize-with-main-editing-cursor!)
        (set-cursor/success f-cursor (cursor:focus-successor f-cursor)))
      
      (define/public (focus-successor/no-snap!)
        (resynchronize-with-main-editing-cursor!)
        (set-cursor/success f-cursor (cursor:focus-successor/no-snap f-cursor)))
      
      (define/public (focus-predecessor!)
        (resynchronize-with-main-editing-cursor!)
        (set-cursor/success f-cursor (cursor:focus-predecessor f-cursor)))
      
      (define/public (focus-predecessor/no-snap!)
        (resynchronize-with-main-editing-cursor!)
        (set-cursor/success f-cursor (cursor:focus-predecessor/no-snap f-cursor)))
      
      (define/public (focus-toplevel!)
        (resynchronize-with-main-editing-cursor!)
        (set-cursor/success f-cursor (cursor:focus-toplevel f-cursor)))
      
      (define/public (focus-container! a-pos)
        (resynchronize-with-main-editing-cursor!)
        (set-cursor/success f-cursor (cursor:focus-container f-cursor a-pos)))
      
      (define/public (focus-pos! a-pos)
        (resynchronize-with-main-editing-cursor!)
        (set-cursor/success f-cursor (cursor:focus-pos f-cursor a-pos)))
      
      (define/public (focus-endpos a-pos)
        (resynchronize-with-main-editing-cursor!)
        (set-cursor/success f-cursor (cursor:focus-endpos f-cursor a-pos)))
      
      
      ;; pretty-print-to-text: dstx -> void
      ;; Write out the dstx content to the text at the current position,
      ;; not scrolling.
      (define (pretty-print-to-text a-dstx)
        (define (insert-in-place a-thing)
          (cond [(send current-text can-insert?
                       (send current-text get-start-position)
                       (if (string? a-thing)
                           (string-length a-thing)
                           (send a-thing get-count)))
                 (send current-text insert
                       a-thing
                       (send current-text get-start-position)
                       'same
                       #f)]
                [else
                 ;; fixme: If this happens, we have to do something!
                 (error 'pretty-print-to-text)]))
        (cond
          [(struct:space? a-dstx)
           (insert-in-place (struct:space-content a-dstx))]
          [(struct:atom? a-dstx)
           (insert-in-place (struct:atom-content a-dstx))]
          [(struct:special-atom? a-dstx)
           (insert-in-place (struct:special-atom-content a-dstx))]
          [(struct:fusion? a-dstx)
           (insert-in-place (struct:fusion-prefix a-dstx))
           (for-each (lambda (sub-dstx)
                       (pretty-print-to-text sub-dstx))
                     (struct:fusion-children a-dstx))
           (insert-in-place (struct:fusion-suffix a-dstx))]))
      
      
      ;; makr-this-cursor-as-up-to-date-editor!
      ;; We tell the current text that this cursor is the one that
      ;; is most up-to-date.
      (define (mark-this-cursor-as-up-to-date-editor!)
        (send current-text set-cursor-for-editing this)
        (send current-text increment-version!)
        (set! current-version (send current-text get-version)))
      
      
      
      ;; with-structured-editing: (-> X) -> X
      ;; Wraps a thunk with the necessary dynamic winds we need
      ;; when doing structured edits.
      (define (with-structured-editing thunk)
        (dynamic-wind
         (lambda ()
           (send current-text begin-dstx-edit-sequence)
           (send current-text begin-edit-sequence))
         thunk
         (lambda ()
           (send current-text end-edit-sequence)
           (send current-text end-dstx-edit-sequence))))
      
      
      ;; Editors
      
      ;; insert-before!: dstx -> void
      ;; Insert a dstx before the current focus.
      (define/public (insert-before! a-dstx)
        (insert-before!/sync-with-text a-dstx #t))
      
      
      ;; insert-before!/sync-with-text: cursor boolean -> void
      ;; Inserts a dstx before the current focus.  If sync? is false,
      ;; doesn't reflect textual change to the current-text.
      ;; Protected.
      (define/public (insert-before!/sync-with-text a-dstx sync?)
        (resynchronize-with-main-editing-cursor!)
        (send current-text on-structured-insert-before
              (get-functional-cursor) a-dstx)
        (let ([a-dstx (send current-text decorate-new-dstx a-dstx)])
          (with-structured-editing
           (lambda ()
             (when sync?
               (send current-text set-position (cursor-pos) 'same #f #f 'local)
               (pretty-print-to-text a-dstx))
             (set! f-cursor (cursor:insert-before f-cursor a-dstx))
             (mark-this-cursor-as-up-to-date-editor!)
             (send current-text after-structured-insert-before f-cursor)))))
      
      
      ;; insert-after!: dstx -> void
      ;; Insert a dstx after the current focus.
      (define/public (insert-after! a-dstx)
        (insert-after!/sync-with-text a-dstx #t))
      
      
      ;; insert-after!/sync-with-text: dstx boolean -> void
      ;; Insert a dstx after the current focus.  If sync is false,
      ;; doesn't keep in sync with current-text.
      ;; Protected.
      (define/public (insert-after!/sync-with-text a-dstx sync?)
        (resynchronize-with-main-editing-cursor!)
        (send current-text on-structured-insert-after
              (get-functional-cursor) a-dstx)
        (let ([a-dstx (send current-text decorate-new-dstx a-dstx)])
          (with-structured-editing
           (lambda ()
             (when sync?
               (send current-text set-position (cursor-endpos) 'same #f #f 'local)
               (pretty-print-to-text a-dstx))
             (set! f-cursor (cursor:insert-after f-cursor a-dstx))
             (mark-this-cursor-as-up-to-date-editor!)
             (send current-text after-structured-insert-after f-cursor)))))
      
      
      ;; delete! -> void
      ;; Delete the dstx at the current focus.  Focus moves preferably to the next
      ;; oldest sibling.
      (define/public (delete!)
        (delete!/sync-with-text #t))
      
      
      ;; delete!/sync-with-text: boolean -> void
      ;; Delete the dstx at the current focus.  Focus moves preferably to the next
      ;; oldest sibling.
      ;; If sync is false, does not keep current-text in sync.
      ;; Protected.
      (define/public (delete!/sync-with-text sync?)
        (resynchronize-with-main-editing-cursor!)
        (send current-text on-structured-delete (get-functional-cursor))
        (with-structured-editing
         (lambda ()
           (let ([deletion-length
                  (- (struct:loc-pos (cursor:loc-after
                                      (struct:cursor-loc f-cursor)
                                      (cursor-dstx)))
                     (cursor-pos))])
             (when sync?
               (cond [(send current-text can-delete? (cursor-pos) deletion-length)
                      (send current-text delete
                            (cursor-pos) (+ (cursor-pos) deletion-length) #f)]
                     [else
                      ;; fixme: I've got to do something here!
                      (error 'delete!)]))
             (set! f-cursor (cursor:delete f-cursor))
             (set! f-cursor (cursor:replace
                             f-cursor
                             (send current-text
                                   decorate-new-dstx
                                   (struct:cursor-dstx f-cursor))))
             (mark-this-cursor-as-up-to-date-editor!)
             (send current-text after-structured-delete f-cursor))))))))
