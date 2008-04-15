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
           (prefix parser: "parse-plt-scheme.ss")
           (prefix cursor: "cursor.ss")
           (prefix struct: "struct.ss")
           "weak-set.ss")
  
  
  (provide dstx-text-mixin dstx-text<%> dstx-cursor<%>)
  
  
  (define dstx-text<%> (interface () get-top-dstxs get-dstx-cursor))
  (define dstx-cursor<%> (interface ()
                           get-functional-cursor
                           resync
                           cursor-dstx
                           cursor-line
                           cursor-col
                           cursor-pos
                           cursor-endpos
                           cursor-dstx-property-ref
                           focus-in
                           focus-in/no-snap
                           focus-out
                           focus-older
                           focus-older/no-snap
                           focus-younger
                           focus-younger/no-snap
                           focus-successor
                           focus-successor/no-snap
                           focus-predecessor
                           focus-predecessor/no-snap
                           focus-toplevel
                           focus-pos
                           cursor-insert-before
                           cursor-insert-after
                           cursor-delete))
  
  
  ;; Macro for iterating a certain number of times.
  ;; (repeat 5 body) will repeat body five times.
  (define-syntax (repeat stx)
    (syntax-case stx ()
      [(_ num body ...)
       (syntax/loc stx
         (let ([N num])
           (let loop ([i 0])
             (when (< i N)
               body ...
               (loop (add1 i))))))]))
  
  
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
  (define-member-name resyncronize (generate-member-key))
  (define-member-name get-version (generate-member-key))
  
  
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
      (define top-dstxs '())
      
      (define version 0)
      (define/public (get-version)
        version)
      
      
      ;; set-top-dstxs: (listof dstx) -> void
      ;; Sets the top dstxs.
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
          [(in-dstx-edit-sequence?)
           (void)]
          [else
           (handle-possibly-unstructured-delete start-pos len)]))
      
      
      ;; after-insert: number number -> void
      (define/augment (after-insert start-pos len)
        (inner #f after-insert start-pos len)
        (cond
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
           (let loop ([start-pos start-pos]
                      [len len])
             (when (> len 0)
               (let ([a-cursor (get-dstx-cursor)])
                 (send a-cursor focus-container start-pos)
                 (let ([deleted-start (max start-pos (send a-cursor cursor-pos))]
                       [deleted-end (min (+ start-pos len)
                                         (send a-cursor cursor-endpos))])
                   ;; There's a bug here: I'm seeing deleted-end < deleted-start
                   ;; in some case.
                   (temporarily-fill-hole deleted-start deleted-end)
                   (let ([new-dstxs
                          (parse-with-hole (send a-cursor cursor-pos)
                                           deleted-start
                                           deleted-end
                                           (send a-cursor cursor-endpos))])
                     (for-each (lambda (a-dstx)
                                 (send a-cursor cursor-insert-after a-dstx)
                                 (send a-cursor focus-younger))
                               (reverse new-dstxs))
                     (send a-cursor cursor-delete)
                     (loop start-pos (- len (- deleted-end deleted-start))))))))
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
        ;; position-focus-on-pos: number -> void
        ;; Puts focus on the dstx that will be
        (define (position-focus-on-pos a-cursor start-pos)
          (send a-cursor focus-pos start-pos)
          (let ([fcursor (send a-cursor get-functional-cursor)])
            ;; subtle: if the very previous expression is an atom, attach to it
            ;; instead.
            (when (and (cursor:focus-younger fcursor)
                       (struct:atom? (struct:cursor-dstx (cursor:focus-younger fcursor)))
                       (= (cursor:cursor-endpos (cursor:focus-younger fcursor))
                          start-pos))
              (send a-cursor focus-younger))))
        
        (define-values
          (original-start-position original-end-position)
          (values (get-start-position) (get-end-position)))
        
        (dynamic-wind
         (lambda ()
           (begin-edit-sequence))
         (lambda ()
           (let ([a-cursor (get-dstx-cursor)])
             (position-focus-on-pos a-cursor start-pos)
             ;; Delete the old, introduce the new.
             (let ([new-dstxs
                    (parse-between (send a-cursor cursor-pos)
                                   (+ (send a-cursor cursor-endpos)
                                      len))])
               (dynamic-wind (lambda () (begin-dstx-edit-sequence))
                             (lambda () (delete start-pos (+ start-pos len)))
                             (lambda () (end-dstx-edit-sequence)))
               (for-each (lambda (new-dstx)
                           (send a-cursor cursor-insert-before new-dstx))
                         (reverse new-dstxs))
               (repeat (length new-dstxs)
                       (send a-cursor focus-older/no-snap))
               (send a-cursor cursor-delete)))
           
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
      (define/override (load-file filename)
        (dynamic-wind (lambda () (begin-dstx-edit-sequence))
                      (lambda () (super load-file filename))
                      (lambda () (end-dstx-edit-sequence)))
        (let* ([dstxs (parse-between 0 (last-position))])
          (set-top-dstxs (map dstx-attach-local-ids dstxs))))
      
      
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
                                            (dstx-attach-local-ids
                                             (struct:new-special-atom a-snip)))
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
                               (dstx-attach-local-ids (struct:new-special-atom
                                                       a-snip
                                                       (send a-snip get-count))))
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
                   (send a-snip next))])))
      
      
      ;; Returns a toplevel cursor into the dstx.
      ;; Operations performed with the cursor will be reflected
      ;; back on screen.
      (define/public (get-dstx-cursor)
        (new dstx-cursor% [text this]))))
  
  
  ;; a dstx-cursor% provides a mutable interface to the functions
  ;; defined in cursor.ss.  Changes made with this dstx-cursor will
  ;; reflect onto the text.
  (define dstx-cursor%
    (class* object% (dstx-cursor<%>)
      (super-new)
      
      (init text)
      
      (define current-text text)
      (define current-version (send text get-version))
      
      (define a-cursor
        (cursor:make-toplevel-cursor (send current-text get-top-dstxs)))
      
      (define-syntax (set-cursor/success stx)
        (syntax-case stx ()
          [(_ a-cursor new-cursor-val)
           (syntax/loc stx
             (begin (unless new-cursor-val
                      (error 'set-cursor "movement failed"))
                    (set! a-cursor new-cursor-val)))]))
      
      
      ;; resync: -> void
      ;; Refresh the cursor's view of the AST, trying our best to preserve
      ;; the focus.
      ;; When the AST is modified, we need to correct our out-of-date view
      ;; of the AST.
      ;; Protected.
      (define/public (resync)
        (when (not (= current-version (send current-text get-version)))
          (let ([old-local-id (cursor-dstx-property-ref 'local-id)]
                [old-pos (cursor-pos)])
            (set! a-cursor (cursor:make-toplevel-cursor
                            (send current-text get-top-dstxs)))
            (cond
              [(cursor:focus-find-dstx
                a-cursor
                (lambda (a-dstx)
                  (= (struct:dstx-property-ref a-dstx 'local-id)
                     old-local-id)))
               =>
               (lambda (new-cursor)
                 (set! a-cursor new-cursor))]
              
              [(cursor:focus-pos a-cursor old-pos)
               =>
               (lambda (new-cursor)
                 (set! a-cursor new-cursor))])
            (set! current-version (send current-text get-version)))))
      
      
      (define/public (get-functional-cursor)
        a-cursor)
      
      ;; Getters
      (define/public (cursor-dstx)
        (struct:cursor-dstx a-cursor))
      
      (define/public (cursor-line)
        (cursor:cursor-line a-cursor))
      
      (define/public (cursor-col)
        (cursor:cursor-col a-cursor))
      
      (define/public (cursor-pos)
        (cursor:cursor-pos a-cursor))
      
      (define/public (cursor-endpos)
        (cursor:cursor-endpos a-cursor))
      
      (define/public (cursor-dstx-property-ref a-name)
        (cursor:cursor-dstx-property-ref a-cursor a-name))
      
      ;; Focusers
      (define/public (focus-in)
        (set-cursor/success a-cursor (cursor:focus-in a-cursor)))
      
      (define/public (focus-in/no-snap)
        (set-cursor/success a-cursor (cursor:focus-in/no-snap a-cursor)))
      
      (define/public (focus-out)
        (set-cursor/success a-cursor (cursor:focus-out a-cursor)))
      
      (define/public (focus-older)
        (set-cursor/success a-cursor (cursor:focus-older a-cursor)))
      
      (define/public (focus-older/no-snap)
        (set-cursor/success a-cursor (cursor:focus-older/no-snap a-cursor)))
      
      (define/public (focus-younger)
        (set-cursor/success a-cursor (cursor:focus-younger a-cursor)))
      
      (define/public (focus-younger/no-snap)
        (set-cursor/success a-cursor (cursor:focus-younger/no-snap a-cursor)))
      
      (define/public (focus-successor)
        (set-cursor/success a-cursor (cursor:focus-successor a-cursor)))
      
      (define/public (focus-successor/no-snap)
        (set-cursor/success a-cursor (cursor:focus-successor/no-snap a-cursor)))
      
      (define/public (focus-predecessor)
        (set-cursor/success a-cursor (cursor:focus-predecessor a-cursor)))
      
      (define/public (focus-predecessor/no-snap)
        (set-cursor/success a-cursor (cursor:focus-predecessor/no-snap a-cursor)))
      
      (define/public (focus-toplevel)
        (set-cursor/success a-cursor (cursor:focus-toplevel a-cursor)))
      
      (define/public (focus-pos a-pos)
        (set-cursor/success a-cursor (cursor:focus-pos a-cursor a-pos)))
      
      (define/public (focus-container a-pos)
        (set-cursor/success a-cursor (cursor:focus-container a-cursor a-pos)))
      
      
      ;; pretty-print-to-text: dstx -> void
      ;; Write out the dstx content to the text
      (define (pretty-print-to-text a-dstx)
        (cond
          [(struct:space? a-dstx)
           (send current-text insert (struct:space-content a-dstx))]
          [(struct:atom? a-dstx)
           (send current-text insert (struct:atom-content a-dstx))]
          [(struct:special-atom? a-dstx)
           ;; fixme: we should see if it's a snip.
           (send current-text insert (struct:special-atom-content a-dstx))]
          [(struct:fusion? a-dstx)
           (send current-text insert (struct:fusion-prefix a-dstx))
           (for-each (lambda (sub-dstx)
                       (pretty-print-to-text sub-dstx))
                     (struct:fusion-children a-dstx))
           (send current-text insert (struct:fusion-suffix a-dstx))]))
      
      
      
      ;; Editors
      (define/public (cursor-insert-before a-dstx)
        (let ([a-dstx (dstx-attach-local-ids a-dstx)])
          (dynamic-wind
           (lambda ()
             (send current-text begin-dstx-edit-sequence)
             (send current-text begin-edit-sequence))
           (lambda ()
             (send current-text set-position (cursor-pos) 'same #f #f 'local)
             (pretty-print-to-text a-dstx)
             (set! a-cursor (cursor:cursor-insert-before a-cursor a-dstx))
             (send current-text set-top-dstxs (cursor:cursor-toplevel-dstxs a-cursor))
             (set! current-version (send current-text get-version)))
           (lambda ()
             (send current-text end-edit-sequence)
             (send current-text end-dstx-edit-sequence)))))
      
      
      (define/public (cursor-insert-after a-dstx)
        (let ([a-dstx (dstx-attach-local-ids a-dstx)])
          (dynamic-wind
           (lambda ()
             (send current-text begin-dstx-edit-sequence)
             (send current-text begin-edit-sequence))
           (lambda ()
             (send current-text set-position (cursor-endpos) 'same #f #f 'local)
             (pretty-print-to-text a-dstx)
             (set! a-cursor (cursor:cursor-insert-after a-cursor a-dstx))
             (send current-text set-top-dstxs (cursor:cursor-toplevel-dstxs a-cursor))
             (set! current-version (send current-text get-version)))
           (lambda ()
             (send current-text end-edit-sequence)
             (send current-text end-dstx-edit-sequence)))))
      
      
      (define/public (cursor-delete)
        (dynamic-wind
         (lambda ()
           (send current-text begin-dstx-edit-sequence)
           (send current-text begin-edit-sequence))
         (lambda ()
           (let ([deletion-length
                  (- (struct:loc-pos (cursor:loc-after
                                      (struct:cursor-loc a-cursor)
                                      (cursor-dstx)))
                     (cursor-pos))])
             (send current-text delete (cursor-pos) (+ (cursor-pos) deletion-length) #f)
             (set! a-cursor (cursor:cursor-delete a-cursor))
             (set! a-cursor (cursor:cursor-replace
                             a-cursor
                             (dstx-attach-local-ids (struct:cursor-dstx a-cursor))))
             (send current-text set-top-dstxs
                   (cursor:cursor-toplevel-dstxs a-cursor))
             (set! current-version (send current-text get-version))))
         (lambda ()
           (send current-text end-edit-sequence)
           (send current-text end-dstx-edit-sequence)))))))