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
           (prefix cursor: "cursor.ss"))
  
  
  ;; next-local-clock: -> number
  ;; Returns the next local clock.
  (define next-local-clock
    (let ([current-local-clock 0])
      (lambda ()
        (set! current-local-clock (add1 current-local-clock)))))
  
  
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
           ;; Possibly unstructured edit.
           ;; fixme!
           (void)]))
      
      
      ;; after-insert: number number -> void
      (define/augment (after-insert start-pos len)
        (inner #f after-insert start-pos len)
        (cond
          [(in-dstx-edit-sequence?)
           (void)]
          [else
           ;; Possibly unstructured edit.
           ;; fixme!
           (void)]))
      
      
      ;; Returns a toplevel cursor into the dstx.
      ;; Operations performed with the cursor will be reflected
      ;; back on screen.
      (define/public (get-dstx-cursor)
        (new cursor% [text this]))
      
      
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
      (init-field text)
      
      (define a-cursor
        (cursor:make-toplevel-cursor (send text get-top-dstxs)))
      
      
      ;; Getters
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
        (set! a-cursor (cursor:focus-in a-cursor)))
      
      (define/public (focus-in/no-snap)
        (set! a-cursor (cursor:focus-in/no-snap a-cursor)))
      
      (define/public (focus-out)
        (set! a-cursor (cursor:focus-out a-cursor)))
      
      (define/public (focus-older)
        (set! a-cursor (cursor:focus-older a-cursor)))
      
      (define/public (focus-older/no-snap)
        (set! a-cursor (cursor:focus-older/no-snap a-cursor)))
      
      (define/public (focus-younger)
        (set! a-cursor (cursor:focus-younger a-cursor)))
      
      (define/public (focus-younger/no-snap)
        (set! a-cursor (cursor:focus-younger/no-snap a-cursor)))
      
      (define/public (focus-successor)
        (set! a-cursor (cursor:focus-successor a-cursor)))
      
      (define/public (focus-predecessor)
        (set! a-cursor (cursor:focus-predecessor a-cursor)))
      
      (define/public (focus-toplevel)
        (set! a-cursor (cursor:focus-toplevel a-cursor)))
      
      (define/public (focus-pos a-pos)
        (set! a-cursor (cursor:focus-pos a-cursor a-pos)))
      
      
      (define (pretty-print a-dstx a-pos)
        ;; fixme!
        (void))
      
      
      ;; Editors
      (define/public (cursor-insert-before a-dstx)
        (dynamic-wind
         (lambda ()
           (send text begin-dstx-edit-sequence))
         (lambda ()
           (pretty-print a-dstx (cursor-pos))
           (set! a-cursor (cursor:cursor-insert-before a-cursor a-dstx))
           (send text set-top-dstxs (cursor:cursor-toplevel-dstxs a-cursor)))
         (lambda ()
           (send text end-dstx-edit-sequence))))
      
      
      (define/public (cursor-insert-after a-dstx)
        (dynamic-wind
         (lambda ()
           (send text begin-dstx-edit-sequence))
         (lambda ()
           (pretty-print a-dstx (cursor-endpos))
           (set! a-cursor (cursor:cursor-insert-after a-cursor a-dstx))
           (send text set-top-dstxs (cursor:cursor-toplevel-dstxs a-cursor)))
         (lambda ()
           (send text end-dstx-edit-sequence))))
      
      
      (define/public (cursor-delete)
        (void))
      
      
      
      (super-new))))