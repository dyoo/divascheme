(module struct mzscheme
  ;; Provides fundamental structures for dstx objects.
  
  (require (lib "contract.ss")
           (lib "plt-match.ss")
           (prefix table: (planet "table.ss" ("soegaard" "galore.plt" 3))))
  
  
  ;; dstx's
  ;;
  ;; NOTE: dstx's must be immutable for us to take advantage of eq?
  ;; and the sharing going on with the hash-struct stuff.  So take
  ;; special care not to expose mutators to the outside world.
  ;;
  ;; A dstx is a generalization of an sexp.  Basically, s-expressions
  ;; are either:
  ;;
  ;;   * atom
  ;;   * special-atom
  ;;   * space
  ;;   * fusion
  
  (define-struct dstx (properties) #f)
  (define-struct (atom dstx) (content) #f)
  (define-struct (special-atom dstx) (content) #f)
  (define-struct (space dstx) (content) #f)
  (define-struct (fusion dstx) (prefix children suffix) #f)
  
  
  ;; new-atom: string -> atom
  ;; Constructor with default empty properties.
  (define (new-atom content)
    (make-atom empty-table content))
  
  ;; new-special-atom: any -> special-atom
  ;; Constructor with default empty properties.
  (define (new-special-atom content)
    (make-special-atom empty-table content))
  
  ;; new-space: string -> space
  ;; Constructor with default empty properties.
  (define (new-space content)
    (make-space empty-table content))
  
  ;; new-fusion: string (listof dstx?) string -> fusion
  ;; Constructor with default empty properties.
  (define (new-fusion prefix children suffix)
    (make-fusion empty-table prefix children suffix))
  
  
  ;; dstx-property-names: dstx -> (listof symbol)
  ;; Returns the list of property names attached to this dstx.
  (define (dstx-property-names a-dstx)
    (table:keys (dstx-properties a-dstx)))
  
  
  ;; dstx-get-property: dstx symbol -> any
  (define (dstx-get-property a-dstx a-sym)
    (table:lookup a-sym (dstx-properties a-dstx)))
  
  
  ;; dstx-set-property: dstx symbol any -> dstx
  ;; Nondestructively set a property.
  (define (dstx-set-property a-dstx a-sym a-val)
    (let ([new-properties
           (table:insert a-sym a-val (dstx-properties a-dstx))])
      (match a-dstx
        [(struct atom (_ content))
         (make-atom new-properties content)]
        [(struct special-atom (_ content))
         (make-special-atom new-properties content)]
        [(struct space (_ content))
         (make-space new-properties content)]
        [(struct fusion (_ prefix children suffix))
         (make-fusion new-properties prefix children suffix)])))
  
  
  
  ;; Cursors.  Zipper structure for efficient movement within a dstx,
  ;; also keeping track of our cursor position.
  (define-struct cursor
    (dstx loc parent youngers-rev youngers-loc-rev olders) #f)
  
  
  ;; location: line and column.
  (define-struct loc (line col pos) #f)
  
  
  
  ;; symbol-cmp: symbol symbol -> (or/c -1 0 1)
  (define (symbol-cmp sym-1 sym-2)
    (let ([s1 (symbol->string sym-1)]
          [s2 (symbol->string sym-2)])
      (cond
        [(string<? s1 s2) -1]
        [(string>? s1 s2) 1]
        [else 0])))
  
  ;; make-empty-table
  ;; -> table
  ;; Creates an empty ordered table specialized for symbol keys.
  (define empty-table
    (table:make-ordered symbol-cmp))
  
  
  
  (define key/value (list/c symbol? any/c))
  (provide/contract
   [struct dstx
           ([properties (listof key/value)])]
   [struct (atom dstx)
           ([properties (listof key/value)]
            [content string?])]
   [struct (special-atom dstx)
           ([properties (listof key/value)]
            [content any/c])]
   [struct (space dstx)
           ([properties (listof key/value)]
            [content string?])]
   [struct (fusion dstx)
           ([properties (listof key/value)]
            [prefix string?]
            [children (listof dstx?)]
            [suffix string?])]
   
   [new-atom (string? . -> . atom?)]
   [new-special-atom (any/c . -> . special-atom?)]
   [new-space (string? . -> . space?)]
   [new-fusion (string? (listof dstx?) string? . -> . fusion?)]
   
   [struct cursor ((dstx dstx?)
                   (loc loc?)
                   (parent (or/c cursor? false/c))
                   (youngers-rev (listof dstx?))
                   (youngers-loc-rev (listof loc?))
                   (olders (listof dstx?)))]
   
   [struct loc ((line natural-number/c)
                (col natural-number/c)
                (pos natural-number/c))]))