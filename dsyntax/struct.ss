(module struct mzscheme
  ;; Provides fundamental structures for dstx objects.
  
  (require (lib "contract.ss")
           (lib "plt-match.ss")
           (lib "list.ss")
           (lib "serialize.ss")
           "weak-memoize.ss")
  
  
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
  
  (define-serializable-struct dstx (properties) #f)
  (define-serializable-struct (atom dstx) (content) #f)
  (define-serializable-struct (special-atom dstx) (content width) #f)
  (define-serializable-struct (space dstx) (content) #f)
  (define-serializable-struct (sentinel-space space) () #f)
  (define-serializable-struct (fusion dstx) (prefix children suffix) #f)
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Some constants:
  
  ;; An empty ordered table specialized for symbol keys.
  (define empty-property-map
    '())
  
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
  ;; new-atom: string -> atom
  ;; Constructor with default empty properties.
  (define new-atom
    (weak-memoize/equal
     (lambda (content) (make-atom empty-property-map content))))
  
  ;; new-special-atom: any -> special-atom
  ;; Constructor with default empty properties and default width 1.
  (define new-special-atom
    (case-lambda [(content)
                  (make-special-atom empty-property-map content 1)]
                 [(content width)
                  (make-special-atom empty-property-map content width)]))
  
  ;; new-space: string -> space
  ;; Constructor with default empty properties.
  (define new-space
    (weak-memoize/equal
     (lambda (content)
       (make-space empty-property-map content))))
  
  
  ;; The empty space atom is sometimes used as a sentinel, so let's
  ;; keep one here.  The first element of a toplevel cursor always points
  ;; to a sentinel space, and the first child of every fusion is
  ;; a sentinel space.
  (define a-sentinel-space (make-sentinel-space empty-property-map ""))
  
  
  ;; new-fusion: string (listof dstx?) string -> fusion
  ;; Constructor with default empty properties.
  (define (new-fusion prefix children suffix)
    (cond
      [(empty? children)
       (make-fusion empty-property-map prefix (list a-sentinel-space) suffix)]
      [else
       (make-fusion empty-property-map prefix (cons a-sentinel-space children)
                    suffix)]))
  
  
  ;; dstx-property-names: dstx -> (listof symbol)
  ;; Returns the list of property names attached to this dstx.
  (define (dstx-property-names a-dstx)
    (map first (dstx-properties a-dstx)))
  
  
  ;; dstx-property-ref: dstx symbol -> any
  ;; Looks up a property.  If the lookup fails, calls the fail thunk
  (define dstx-property-ref
    (case-lambda
      [(a-dstx a-sym fail-f)
       (cond [(assq a-sym (dstx-properties a-dstx))
              =>
              second]
             [else
              (fail-f)])]
      [(a-dstx a-sym)
       (cond [(assq a-sym (dstx-properties a-dstx))
              =>
              second]
             [else
              (error 'dstx-property-ref "Can't find ~s~n" a-sym)])]))
  
  
  ;; symbol-cmp: symbol symbol -> (or/c -1 0 1)
  (define (symbol-cmp sym-1 sym-2)
    (let ([s1 (symbol->string sym-1)]
          [s2 (symbol->string sym-2)])
      (cond
        [(string<? s1 s2) -1]
        [(string>? s1 s2) 1]
        [else 0])))
  
  
  ;; property-update: property-table/c symbol any -> property-table/c
  ;; Extends a property table, keeping the keys sorted.
  (define (property-update props a-sym a-val)
    (let loop ([props props])
      (cond
        [(empty? props)
         (list (list a-sym a-val))]
        [(eq? (first (first props)) a-sym)
         (cons (list a-sym a-val)
               (rest props))]
        [(= (symbol-cmp a-sym (first (first props)))
            1)
         (cons (list a-sym a-val)
               props)]
        [else
         (cons (first props)
               (loop (rest props)))])))
  
  ;; property-remove: property-table/c symbol -> property-table/c
  (define (property-remove props a-sym)
    (let loop ([props props])
      (cond
        [(empty? props)
         props]
        [(eq? (first (first props)) a-sym)
         (rest props)]
        [(= (symbol-cmp a-sym (first (first props)))
            1)
         props]
        [else
         (cons (first props)
               (loop (rest props)))])))
  
  
  ;; dstx-property-set: dstx symbol any -> dstx
  ;; Nondestructively set a property.
  (define (dstx-property-set a-dstx a-sym a-val)
    (let ([new-properties (property-update (dstx-properties a-dstx) a-sym a-val)])
      (match a-dstx
        [(struct atom (_ content))
         (make-atom new-properties content)]
        [(struct special-atom (_ content width))
         (make-special-atom new-properties content width)]
        [(struct space (_ content))
         (make-space new-properties content)]
        [(struct fusion (_ prefix children suffix))
         (make-fusion new-properties prefix children suffix)])))
  
  
  ;; dstx-property-remove: dstx symbol -> dstx
  ;; Strip a property from a dstx.
  (define (dstx-property-remove a-dstx a-sym)
    (let ([new-properties (property-remove (dstx-properties a-dstx) a-sym)])
      (match a-dstx
        [(struct atom (_ content))
         (make-atom new-properties content)]
        [(struct special-atom (_ content width))
         (make-special-atom new-properties content width)]
        [(struct space (_ content))
         (make-space new-properties content)]
        [(struct fusion (_ prefix children suffix))
         (make-fusion new-properties prefix children suffix)])))
  
  
  ;; dstx-deepmap: (dstx -> dstx) dstx -> dstx
  ;; Deeply apply f to transform a dstx into another dstx.
  (define (dstx-deepmap f a-dstx)
    (match a-dstx
      [(struct atom (prop content))
       (f a-dstx)]
      [(struct special-atom (prop content width))
       (f a-dstx)]
      [(struct space (prop content))
       (f a-dstx)]
      [(struct fusion (prop prefix children suffix))
       (f (make-fusion prop
                       prefix
                       (map (lambda (a-dstx)
                              (dstx-deepmap f a-dstx))
                            children)
                       suffix))]))
  
  
  ;; Cursors.  Zipper structure for efficient movement within a dstx,
  ;; also keeping track of our cursor position.
  (define-struct cursor
    (dstx loc parent youngers-rev youngers-loc-rev olders) #f)
  
  ;; location: line and column.
  (define-struct loc (line col pos) #f)
  
  
  (define (nelistof x)
    (and/c (not/c null?)
           (listof x)))
  
  
  (define property-map/c (listof (list/c symbol? any/c)))
  
  (provide/contract
   [empty-property-map property-map/c]
   
   [struct dstx
           ([properties property-map/c])]
   [struct (atom dstx)
           ([properties property-map/c]
            [content string?])]
   [struct (special-atom dstx)
           ([properties property-map/c]
            [content any/c]
            [width natural-number/c])]
   [struct (space dstx)
           ([properties property-map/c]
            [content string?])]
   [struct (sentinel-space space)
           ([properties property-map/c]
            [content string?])]
   [struct (fusion dstx)
           ([properties property-map/c]
            [prefix string?]
            [children (nelistof dstx?)]
            [suffix string?])]
   
   [a-sentinel-space sentinel-space?]
   
   [dstx-property-names (dstx? . -> . (listof symbol?))]
   [dstx-property-ref (case-> (dstx? symbol? . -> . any)
                              (dstx? symbol? (-> any) . -> . any))]
   [dstx-property-set (dstx? symbol? any/c . -> . dstx?)]
   [dstx-property-remove (dstx? symbol? . -> . dstx?)]
   
   [new-atom (string? . -> . atom?)]
   [new-special-atom (case-> (any/c . -> . special-atom?)
                             (any/c number? . -> . special-atom?))]
   [new-space (string? . -> . space?)]
   [new-fusion (string? (listof dstx?) string? . -> . fusion?)]
   
   [dstx-deepmap ((dstx? . -> . dstx?) dstx? . -> . dstx?)]
   
   [struct cursor ((dstx dstx?)
                   (loc loc?)
                   (parent (or/c cursor? false/c))
                   (youngers-rev (listof dstx?))
                   (youngers-loc-rev (listof loc?))
                   (olders (listof dstx?)))]
   
   [struct loc ((line natural-number/c)
                (col natural-number/c)
                (pos natural-number/c))]))