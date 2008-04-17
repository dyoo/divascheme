(module dstx-text-mixin mzscheme
  ; Provides a text mixin that keeps track of an ast and allows
  ; insertions and deletions.
  ;
  ; Each element of the ast is marked with a 'local-id property.
  ;
  ; When text is modified behind our back, we generate the operation
  ; corresponding to that change, and adjust our ast accordingly.
  
  (require (lib "class.ss")
           (lib "port.ss")
           (lib "mred.ss" "mred")
           (prefix parser: "parse-plt-scheme.ss")
           (prefix cursor: "cursor.ss")
           (prefix struct: "struct.ss"))
  
  
  (provide dstx-text-mixin dstx-text<%> dstx-cursor<%>)
  
  
  (define dstx-text<%> (interface (editor<%>)
                         get-top-dstxs
                         get-dstx-cursor
                         dstx-parsing-enabled?
                         enable-dstx-parsing
                         disable-dstx-parsing))
  
  (define dstx-cursor<%> (interface ()
                           get-functional-cursor
                           resync!
                           
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
  
  
  
  
  ;; Made dstx-text-mixin and dstx-cursor friends.  The following methods
  ;; can only be called by classes defined in this module.
  (define-member-name set-top-dstxs (generate-member-key))
  (define-member-name begin-dstx-edit-sequence (generate-member-key))
  (define-member-name end-dstx-edit-sequence (generate-member-key))
  (define-member-name get-version (generate-member-key))
  (define-member-name get-cursor-for-editing (generate-member-key))
  (define-member-name set-cursor-for-editing (generate-member-key))
  
  
  ;; dstx-text-mixin: text% -> text%
  ;; Adds in some functionality specific to dstx maintenance.
  (define (dstx-text-mixin super%)
    (class* super% (dstx-text<%>)
      (inherit last-position
               get-text
               get-start-position
               get-end-position
               set-position
               insert
               delete
               split-snip
               find-snip
               get-snip-position
               begin-edit-sequence
               end-edit-sequence)
      
      (super-new)
      
      ;; top-dstxs: (listof dstx)
      ;; The toplevel dstx elements.
      (define top-dstxs (list (dstx-attach-local-ids (struct:new-space ""))))
      
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
      
      ;; Returns a toplevel cursor into the dstx.
      ;; Operations performed with the cursor will be reflected
      ;; back on screen.
      (define/public (get-dstx-cursor)
        (new dstx-cursor% [text this]))
      
      
      ;; We keep a dstx-cursor that's used primarily for the
      ;; unstructured edit stuff, and for faster synchronization
      ;; with other cursors.
      ;; Warning: do NOT expose this to the outside world.
      ;; This is the very last used cursor that edited this window.
      (define cursor-for-editing (get-dstx-cursor))
      
      (define/public (get-cursor-for-editing)
        cursor-for-editing)
      
      (define/public (set-cursor-for-editing a-cursor)
        (set! cursor-for-editing a-cursor))
      
      
      
      ;; set-top-dstxs: (listof dstx) -> void
      ;; Sets the top dstxs.  Assumption: the dstxs is already
      ;; colored with a local id.
      ;; Protected.
      (define/public (set-top-dstxs dstxs)
        (set! top-dstxs dstxs)
        (set! version (add1 version)))
      
      ;; get-top-dstxs: -> (listof dstx)
      ;; Returns the top dstxs.
      (define/public (get-top-dstxs)
        top-dstxs)
      
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
      
      
      ;; after-delete: number number -> void
      (define/augment (after-delete start-pos len)
        (inner #f after-delete start-pos len)
        (cond
          [(not parsing-enabled?)
           (void)]
          [(in-dstx-edit-sequence?)
           (void)]
          [else
           (handle-possibly-unstructured-delete start-pos len)]))
      
      
      ;; after-insert: number number -> void
      (define/augment (after-insert start-pos len)
        (inner #f after-insert start-pos len)
        (cond
          [(not parsing-enabled?)
           (void)]
          [(in-dstx-edit-sequence?)
           (void)]
          [else
           (handle-possibly-unstructured-insert start-pos len)]))
      
      
      ;; handle-possibly-unstructured-delete: number number -> void
      ;;
      (define (handle-possibly-unstructured-delete start-pos len)
        ;; temporarily-fill-hole: number number -> void
        ;; temporarily put something in the deleted text's hole
        ;; to make textual parsing and deletion work.
        (define (temporarily-fill-hole deleted-start deleted-end)
          (dynamic-wind (lambda ()
                          (begin-dstx-edit-sequence))
                        (lambda ()
                          (insert (make-string (- deleted-end deleted-start) #\X)
                                  deleted-start
                                  'same
                                  #f))
                        (lambda ()
                          (end-dstx-edit-sequence))))
        
        (define-values
          (original-start-position original-end-position)
          (values (get-start-position) (get-end-position)))
        
        (dynamic-wind
         (lambda ()
           (begin-edit-sequence))
         (lambda ()
           (send cursor-for-editing resync!)
           (let loop ([len len])
             (when (> len 0)
               (send cursor-for-editing focus-container! start-pos)
               (let ([deleted-start (max start-pos (send cursor-for-editing cursor-pos))]
                     [deleted-end (min (+ start-pos len)
                                       (send cursor-for-editing cursor-endpos))])
                 ;; There's a bug here: I'm seeing deleted-end < deleted-start
                 ;; in some case.
                 (temporarily-fill-hole deleted-start deleted-end)
                 (let ([new-dstxs
                        (parse-with-hole (send cursor-for-editing cursor-pos)
                                         deleted-start
                                         deleted-end
                                         (send cursor-for-editing cursor-endpos))])
                   (for-each (lambda (a-dstx)
                               (send cursor-for-editing insert-after! a-dstx)
                               (send cursor-for-editing focus-younger/no-snap!))
                             (reverse new-dstxs))
                   (send cursor-for-editing delete!)
                   (loop (- len (- deleted-end deleted-start)))))))
           (set-position original-start-position original-end-position #f #f 'local))
         (lambda ()
           (end-edit-sequence))))
      
      
      ;; deletion-spans-whole-focus? dstx-cursor number number -> boolean
      ;; Returns true if the unstructured delete spans across the whole focused dstx.
      (define (deletion-spans-whole-focus? a-cursor start-pos len)
        (and (<= start-pos (send a-cursor start-pos))
             (>= len (- (send a-cursor cursor-endpos)
                        (send a-cursor cursor-pos)))))
      
      ;; deletion-overlaps-focus? dstx-cursor number number -> booelan
      ;; Returns true if there's an overlap between the focus and the deletion.
      (define (deletion-overlaps-focus? a-cursor start-pos len)
        (and (<= (send a-cursor cursor-pos) start-pos)
             (< start-pos (send a-cursor cursor-endpos))))
      
      
      
      
      ;; handle-possibly-unstructured-insert: number number -> void
      ;; When the text changes without explicit structured operations, we
      ;; must maintain semi-structure.
      (define (handle-possibly-unstructured-insert start-pos len)
        ;; not-in-something? cursor
        ;; True only if we're inserting at the very end of something
        (define (inserting-at-end? a-cursor)
          (let ([fcursor (send a-cursor get-functional-cursor)])
            (cond [(cursor:focus-container fcursor start-pos)
                   #f]
                  [else #t])))
        
        (define (delete-introduced-text)
          (dynamic-wind (lambda () (begin-dstx-edit-sequence))
                        (lambda () (delete start-pos (+ start-pos len) #f))
                        (lambda () (end-dstx-edit-sequence))))
        
        (define (insert-new-dstxs-after a-cursor new-dstxs)
          (for-each (lambda (new-dstx)
                      (send a-cursor insert-after! new-dstx)
                      (send a-cursor focus-younger/no-snap!))
                    (reverse new-dstxs)))
        
        (define (insert-at-end a-cursor)
          (send a-cursor focus-toplevel!)
          (send a-cursor focus-oldest!)
          (let ([fcursor (send a-cursor get-functional-cursor)])
            (cond
              ;; subtle: if the focus is an atom, attach to it
              ;; instead of inserting after it.
              [(and (struct:atom? (struct:cursor-dstx fcursor))
                    (= (cursor:cursor-endpos fcursor) start-pos))
               ;; Delete the old, introduce the new.
               (let ([new-dstxs
                      (parse-between (send a-cursor cursor-pos)
                                     (+ (send a-cursor cursor-endpos)
                                        len))])
                 (delete-introduced-text)
                 (insert-new-dstxs-after a-cursor new-dstxs)
                 (send a-cursor delete!))]
              [else
               (let ([new-dstxs (parse-between start-pos (+ start-pos len))])
                 (delete-introduced-text)
                 (insert-new-dstxs-after a-cursor new-dstxs))])))
        
        (define (insert-within-something a-cursor)
          (send a-cursor focus-container! start-pos)
          (let ([fcursor (send a-cursor get-functional-cursor)])
            ;; subtle: if the very previous expression is an atom, attach to it
            ;; instead.
            (when (and (cursor:focus-younger/no-snap fcursor)
                       (struct:atom? (struct:cursor-dstx (cursor:focus-younger/no-snap fcursor)))
                       (= (cursor:cursor-endpos (cursor:focus-younger/no-snap fcursor))
                          start-pos))
              (send a-cursor focus-younger/no-snap!)))
          ;; Delete the old, introduce the new.
          (let ([new-dstxs (parse-between (send a-cursor cursor-pos)
                                          (+ (send a-cursor cursor-endpos)
                                             len))])
            (delete-introduced-text)
            (insert-new-dstxs-after a-cursor new-dstxs)
            (send a-cursor delete!)))
        
        (define-values
          (original-start-position original-end-position)
          (values (get-start-position) (get-end-position)))
        (dynamic-wind
         (lambda ()
           (begin-edit-sequence))
         (lambda ()
           (send cursor-for-editing resync!)
           (cond [(inserting-at-end? cursor-for-editing)
                  (insert-at-end cursor-for-editing)]
                 [else
                  (insert-within-something cursor-for-editing)])
           (set-position original-start-position original-end-position #f #f 'local))
         (lambda ()
           (end-edit-sequence))))
      
      
      
      
      
      
      ;; edit-within-focused-dstx? dstx-cursor number -> boolean
      (define (edit-within-focused-dstx? a-cursor a-pos)
        (and ((send a-cursor cursor-pos) . < . a-pos)
             (a-pos . < . (send a-cursor cursor-endpos))))
      
      
      ;; edit-bordering-focused-dstx? dstx-cursor number -> boolean
      (define (edit-bordering-focused-dstx? a-cursor a-pos)
        (or ((send a-cursor cursor-pos) . = . a-pos)
            (a-pos . = . (send a-cursor cursor-endpos))))
      
      
      
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
        (let* ([dstxs (parse-between 0 (last-position))])
          (set-top-dstxs (map dstx-attach-local-ids dstxs))
          (send cursor-for-editing resync!)))
      
      
      ;; parse-between: number number -> (listof dstx)
      ;; Parse the text between start and end.  If the content
      ;; is unparseable, return a list containing a new fusion
      ;; marked with the property 'unparsed.
      (define (parse-between start end)
        (with-handlers ([exn:fail? (lambda (exn)
                                     (parse-between/unparsed start end))])
          (let* ([ip (parser:open-input-text this start end)]
                 [dstxs (parser:parse-port ip)])
            dstxs)))
      
      
      ;; parse-with-hole: number number number number -> (listof dstx)
      ;; Parses between [start, hole-start], [hole-end, end].
      (define (parse-with-hole start hole-start hole-end end)
        (with-handlers ([exn:fail? (lambda (exn)
                                     (map (lambda (a-snip)
                                            (struct:dstx-property-set
                                             (dstx-attach-local-ids
                                              (struct:new-special-atom
                                               a-snip
                                               (send a-snip get-count)))
                                             'unparsed #t))
                                          (append (reverse (get-snips/rev start hole-start))
                                                  (reverse (get-snips/rev hole-end end)))))])
          (let* ([ip1 (parser:open-input-text this start hole-start)]
                 [ip2 (parser:open-input-text this hole-end end)]
                 [ip (input-port-append #t ip1 ip2)]
                 [dstxs (parser:parse-port ip)])
            dstxs)))
      
      
      
      ;; parse-between/unparsed: start end -> (listof dstx)
      ;; Returns a list of dstx objects that represent the unparsed
      ;; elements.  This is a catch-all for cases where we have no idea how to
      ;; parse something.
      (define (parse-between/unparsed start end)
        (let ([result
               (reverse (map (lambda (a-snip)
                               (struct:dstx-property-set
                                (dstx-attach-local-ids
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
  
  
  ;; make-toplevel-functional-cursor: text -> dstx-cursor
  ;; Creates the toplevel cursor, ensuring that every dstx in there has a local id.
  (define (make-toplevel-functional-cursor a-text)
    (let ([a-cursor (cursor:make-toplevel-cursor
                     (send a-text get-top-dstxs))])
      (cursor:replace
       a-cursor
       (dstx-attach-local-ids (struct:cursor-dstx a-cursor)))))
  
  
  ;; a dstx-cursor% provides a mutable interface to the functions
  ;; defined in cursor.ss.  Changes made with this dstx-cursor will
  ;; reflect onto the text.
  (define dstx-cursor%
    (class* object% (dstx-cursor<%>)
      (super-new)
      
      (init text)
      
      (define current-text text)
      (define current-version (send text get-version))
      
      ;; f-cursor is a functional cursor that we reassign for all the
      ;; operations here.
      (define f-cursor (make-toplevel-functional-cursor current-text))
      
      (define-syntax (set-cursor/success stx)
        (syntax-case stx ()
          [(_ a-cursor new-cursor-val)
           (syntax/loc stx
             (begin (unless new-cursor-val
                      (error 'set-cursor "movement failed"))
                    (set! a-cursor new-cursor-val)))]))
      
      
      ;; resync!: -> void
      ;; Refresh the cursor's view of the AST, trying our best to preserve
      ;; the focus.
      ;; When the AST is modified, we need to correct our out-of-date view
      ;; of the AST.
      ;; Protected.
      (define/public (resync!)
        (when (and (send current-text dstx-parsing-enabled?)
                   (not (= current-version (send current-text get-version))))
          (let ([old-local-id (property-ref 'local-id)]
                [old-pos (cursor-pos)])
            ;; If the previous set is unsound, let's go back to the
            ;; slow-but-safe option.
            (cond
              [(eq? this (send current-text get-cursor-for-editing))
               (set! f-cursor (make-toplevel-functional-cursor current-text))]
              [else
               ;; optimization: try to reuse the cursor that was last used for editing.
               (set! f-cursor
                     (send (send current-text get-cursor-for-editing)
                           get-functional-cursor))])
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
        f-cursor)
      
      ;; Getters
      (define/public (cursor-dstx)
        (struct:cursor-dstx f-cursor))
      
      (define/public (cursor-line)
        (cursor:cursor-line f-cursor))
      
      (define/public (cursor-col)
        (cursor:cursor-col f-cursor))
      
      (define/public (cursor-pos)
        (cursor:cursor-pos f-cursor))
      
      (define/public (cursor-endpos)
        (cursor:cursor-endpos f-cursor))
      
      
      ;; Property get and set
      (define/public (property-ref a-name)
        (cursor:property-ref f-cursor a-name))
      
      (define/public (property-set! a-name a-val)
        (cursor:property-set f-cursor a-name a-val))
      
      
      ;; Focusers
      (define/public (focus-in!)
        (set-cursor/success f-cursor (cursor:focus-in f-cursor)))
      
      (define/public (focus-in/no-snap!)
        (set-cursor/success f-cursor (cursor:focus-in/no-snap f-cursor)))
      
      (define/public (focus-out!)
        (set-cursor/success f-cursor (cursor:focus-out f-cursor)))
      
      (define/public (focus-older!)
        (set-cursor/success f-cursor (cursor:focus-older f-cursor)))
      
      (define/public (focus-older/no-snap!)
        (set-cursor/success f-cursor (cursor:focus-older/no-snap f-cursor)))
      
      (define/public (focus-oldest!)
        (set-cursor/success f-cursor (cursor:focus-oldest f-cursor)))
      
      (define/public (focus-younger!)
        (set-cursor/success f-cursor (cursor:focus-younger f-cursor)))
      
      (define/public (focus-younger/no-snap!)
        (set-cursor/success f-cursor (cursor:focus-younger/no-snap f-cursor)))
      
      (define/public (focus-youngest!)
        (set-cursor/success f-cursor (cursor:focus-youngest f-cursor)))
      
      (define/public (focus-successor!)
        (set-cursor/success f-cursor (cursor:focus-successor f-cursor)))
      
      (define/public (focus-successor/no-snap!)
        (set-cursor/success f-cursor (cursor:focus-successor/no-snap f-cursor)))
      
      (define/public (focus-predecessor!)
        (set-cursor/success f-cursor (cursor:focus-predecessor f-cursor)))
      
      (define/public (focus-predecessor/no-snap!)
        (set-cursor/success f-cursor (cursor:focus-predecessor/no-snap f-cursor)))
      
      (define/public (focus-toplevel!)
        (set-cursor/success f-cursor (cursor:focus-toplevel f-cursor)))
      
      (define/public (focus-container! a-pos)
        (set-cursor/success f-cursor (cursor:focus-container f-cursor a-pos)))
      
      (define/public (focus-pos! a-pos)
        (set-cursor/success f-cursor (cursor:focus-pos f-cursor a-pos)))
      
      (define/public (focus-endpos a-pos)
        (set-cursor/success f-cursor (cursor:focus-endpos f-cursor a-pos)))
      
      
      ;; pretty-print-to-text: dstx -> void
      ;; Write out the dstx content to the text at the current position,
      ;; not scrolling.
      (define (pretty-print-to-text a-dstx)
        (define (insert-in-place a-thing)
          (send current-text insert
                a-thing
                (send current-text get-start-position)
                'same
                #f))
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
      
      
      
      ;; Editors
      (define/public (insert-before! a-dstx)
        (resync!)
        (let ([a-dstx (dstx-attach-local-ids a-dstx)])
          (dynamic-wind
           (lambda ()
             (send current-text begin-dstx-edit-sequence)
             (send current-text begin-edit-sequence))
           (lambda ()
             (send current-text set-position (cursor-pos) 'same #f #f 'local)
             (pretty-print-to-text a-dstx)
             (set! f-cursor (cursor:insert-before f-cursor a-dstx))
             (send current-text set-top-dstxs (cursor:cursor-toplevel-dstxs f-cursor))
             (send current-text set-cursor-for-editing this)
             (set! current-version (send current-text get-version)))
           (lambda ()
             (send current-text end-edit-sequence)
             (send current-text end-dstx-edit-sequence)))))
      
      
      (define/public (insert-after! a-dstx)
        (resync!)
        (let ([a-dstx (dstx-attach-local-ids a-dstx)])
          (dynamic-wind
           (lambda ()
             (send current-text begin-dstx-edit-sequence)
             (send current-text begin-edit-sequence))
           (lambda ()
             (send current-text set-position (cursor-endpos) 'same #f #f 'local)
             (pretty-print-to-text a-dstx)
             (set! f-cursor (cursor:insert-after f-cursor a-dstx))
             (send current-text set-top-dstxs (cursor:cursor-toplevel-dstxs f-cursor))
             (send current-text set-cursor-for-editing this)
             (set! current-version (send current-text get-version)))
           (lambda ()
             (send current-text end-edit-sequence)
             (send current-text end-dstx-edit-sequence)))))
      
      
      (define/public (delete!)
        (resync!)
        (dynamic-wind
         (lambda ()
           (send current-text begin-dstx-edit-sequence)
           (send current-text begin-edit-sequence))
         (lambda ()
           (let ([deletion-length
                  (- (struct:loc-pos (cursor:loc-after
                                      (struct:cursor-loc f-cursor)
                                      (cursor-dstx)))
                     (cursor-pos))])
             (send current-text delete (cursor-pos) (+ (cursor-pos) deletion-length) #f)
             (set! f-cursor (cursor:delete f-cursor))
             (set! f-cursor (cursor:replace
                             f-cursor
                             (dstx-attach-local-ids (struct:cursor-dstx f-cursor))))
             (send current-text set-top-dstxs (cursor:cursor-toplevel-dstxs f-cursor))
             (send current-text set-cursor-for-editing this)
             (set! current-version (send current-text get-version))))
         (lambda ()
           (send current-text end-edit-sequence)
           (send current-text end-dstx-edit-sequence)))))))