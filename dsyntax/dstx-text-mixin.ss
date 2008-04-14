(module dstx-text-mixin mzscheme
  ; Provides a text mixin that keeps track of an ast and allows
  ; insertions and deletions.
  ;
  ; Each element of the ast is marked with a 'local-id property.
  ;
  ; When text is modified behind our back, we generate the operation
  ; corresponding to that change, and adjust our ast accordingly.
  
  (require (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "plt-match.ss")
           (prefix parser: "parse-plt-scheme.ss")
           (prefix cursor: "cursor.ss")
           (prefix struct: "struct.ss"))
  
  
  (provide dstx-text-mixin)
  
  ;; TODO: distill interface
  
  
  
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
    (struct:dstx-deepmap (lambda (a-dstx)
                           (struct:dstx-property-set a-dstx 'local-id (next-local-id)))
                         a-dstx))
  
  
  ;; Made dstx-text-mixin and dstx-cursor friends.  The following methods
  ;; can only be called by classes defined in this module.
  (define-member-name set-top-dstxs (generate-member-key))
  (define-member-name begin-dstx-edit-sequence (generate-member-key))
  (define-member-name end-dstx-edit-sequence (generate-member-key))
  
  
  ;; dstx-text-mixin: text% -> text%
  ;; Adds in some functionality specific to dstx maintenance.
  (define (dstx-text-mixin super%)
    (class super%
      (inherit begin-edit-sequence
               end-edit-sequence
               in-edit-sequence?
               get-start-position
               last-position
               erase
               insert)
      
      ;; top-dstxs: (listof dstx)
      ;; The toplevel dstx elements.
      (define top-dstxs '())
      
      ;; set-top-dstxs: (listof dstx) -> void
      ;; Sets the top dstxs.
      ;; Protected.
      (define/public (set-top-dstxs dstxs)
        (set! top-dstxs dstxs))
      
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
        (set! dstx-edit-depth (add1 dstx-edit-depth)))
      
      
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
      
      
      (define (handle-possibly-unstructured-delete start-pos len)
        ;; Possibly unstructured edit.
        ;; fixme!
        (void))
      
      
      ;; handle-possibly-unstructured-insert: number number -> void
      ;; When the text changes without explicit structured operations, we
      ;; must maintain semi-structure.
      (define (handle-possibly-unstructured-insert start-pos len)
        
        (define (edit-within-focused-dstx? a-cursor)
          (and ((send a-cursor cursor-pos) . < . start-pos)
               (start-pos . < . (send a-cursor cursor-endpos))))
        
        (define (edit-bordering-focused-dstx? a-cursor)
          (or ((send a-cursor cursor-pos) . = . start-pos)
              (start-pos . = . (send a-cursor cursor-endpos))))
        
        ;; Treat an unstructured insertion as an atom with the
        ;; 'unstructured property.
        (let ([a-cursor (get-dstx-cursor)])
          (send a-cursor focus-pos start-pos)
          (cond
            [(edit-within-focused-dstx? a-cursor)
             (match (send a-cursor cursor-dstx)
               [(struct struct:atom (props content))
                ;; Delete the atom, introduce a new atom with
                ;; the same content.
                (void)]
               [(struct struct:special-atom (props content width))
                ;; This should not be possible
                (void)]
               [(struct struct:space (props content))
                ;; split up
                (void)]
               [(struct struct:fusion (props prefix children suffix))
                ;; Delete the fusion, reparse it.
                (void)])]
            
            [(edit-bordering-focused-dstx? a-cursor)
             (match (send a-cursor cursor-dstx)
               [(struct struct:atom (props content))
                (void)]
               [(struct struct:special-atom (props content))
                (void)]
               [(struct struct:space (props content))
                (void)]
               [(struct struct:fusion (props prefix children suffix))
                (void)])])))
      
      
      
      ;; load-file: string -> void
      (define/override (load-file filename)
        (dynamic-wind (lambda () (begin-dstx-edit-sequence))
                      (lambda () (super load-file filename))
                      (lambda () (end-dstx-edit-sequence)))
        (let* ([ip (get-input-port-after-insert 0 (last-position))]
               [dstxs (parser:parse-port ip)])
          (set-top-dstxs dstxs)))
      
      
      ;; Returns a toplevel cursor into the dstx.
      ;; Operations performed with the cursor will be reflected
      ;; back on screen.
      (define/public (get-dstx-cursor)
        (new dstx-cursor% [text this]))
      
      
      ;; get-input-port-after-insert: number number -> input-port
      ;; Helper: returns an input port to let us grab the content
      ;; of the insertion.
      (define (get-input-port-after-insert start len)
        (open-input-text-editor this start (+ start len)
                                (lambda (snip) (box snip))
                                #f #f))
      
      (super-new)))
  
  
  ;; a dstx-cursor% provides a mutable interface to the functions
  ;; defined in cursor.ss.  Changes made with this dstx-cursor will
  ;; reflect onto the text.
  (define dstx-cursor%
    (class object%
      (init text)
      (define current-text text)
      
      (define a-cursor
        (cursor:make-toplevel-cursor (send current-text get-top-dstxs)))
      
      
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
      
      (define-syntax (set-cursor/success stx)
        (syntax-case stx ()
          [(_ a-cursor new-cursor-val)
           (syntax/loc stx
             (begin (unless new-cursor-val
                      (error 'set-cursor "movement failed"))
                    (set! a-cursor new-cursor-val)))]))
      
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
      
      (define/public (focus-predecessor)
        (set-cursor/success a-cursor (cursor:focus-predecessor a-cursor)))
      
      (define/public (focus-toplevel)
        (set-cursor/success a-cursor (cursor:focus-toplevel a-cursor)))
      
      (define/public (focus-pos a-pos)
        (set-cursor/success a-cursor (cursor:focus-pos a-cursor a-pos)))
      
      
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
             (send current-text begin-dstx-edit-sequence))
           (lambda ()
             (send current-text set-position (cursor-pos) 'same #f #f 'local)
             (pretty-print-to-text a-dstx)
             (set! a-cursor (cursor:cursor-insert-before a-cursor a-dstx))
             (send current-text set-top-dstxs (cursor:cursor-toplevel-dstxs a-cursor)))
           (lambda ()
             (send current-text end-dstx-edit-sequence)))))
      
      
      (define/public (cursor-insert-after a-dstx)
        (let ([a-dstx (dstx-attach-local-ids a-dstx)])
          (dynamic-wind
           (lambda ()
             (send current-text begin-dstx-edit-sequence))
           (lambda ()
             (send current-text set-position (cursor-endpos) 'same #f #f 'local)
             (pretty-print-to-text a-dstx)
             (set! a-cursor (cursor:cursor-insert-after a-cursor a-dstx))
             (send current-text set-top-dstxs (cursor:cursor-toplevel-dstxs a-cursor)))
           (lambda ()
             (send current-text end-dstx-edit-sequence)))))
      
      
      (define/public (cursor-delete)
        (dynamic-wind
         (lambda ()
           (send current-text begin-dstx-edit-sequence))
         (lambda ()
           (let ([deletion-length
                  (- (struct:loc-pos (cursor:loc-after
                                      (struct:cursor-loc a-cursor)))
                     (cursor-pos))])
             (send current-text delete (cursor-pos) deletion-length #f)
             (set! a-cursor (cursor:cursor-delete a-cursor))
             (send current-text set-top-dstxs (cursor:cursor-toplevel-dstxs a-cursor))))
         (lambda ()
           (send current-text end-dstx-edit-sequence))))
      
      
      
      (super-new))))