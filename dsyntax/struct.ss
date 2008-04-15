(module struct mzscheme
  ;; Provides fundamental structures for dstx objects.
  
  (require (lib "contract.ss")
           (lib "plt-match.ss")
           (lib "list.ss")
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
  (define-struct (special-atom dstx) (content width) #f)
  (define-struct (space dstx) (content) #f)
  (define-struct (fusion dstx) (prefix children suffix) #f)
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Some constants:
  
  ;; An empty ordered table specialized for symbol keys.
  (define empty-table
    (let ()
      ;; symbol-cmp: symbol symbol -> (or/c -1 0 1)
      (define (symbol-cmp sym-1 sym-2)
        (let ([s1 (symbol->string sym-1)]
              [s2 (symbol->string sym-2)])
          (cond
            [(string<? s1 s2) -1]
            [(string>? s1 s2) 1]
            [else 0])))
      (table:make-ordered symbol-cmp)))
  
  
  ;; The empty space atom is sometimes used as a sentinel, so let's
  ;; keep one here.
  (define empty-space-atom (make-space empty-table ""))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
  ;; new-atom: string -> atom
  ;; Constructor with default empty properties.
  (define (new-atom content)
    (make-atom empty-table content))
  
  ;; new-special-atom: any -> special-atom
  ;; Constructor with default empty properties and default width 1.
  (define new-special-atom
    (case-lambda [(content)
                  (make-special-atom empty-table content 1)]
                 [(content width)
                  (make-special-atom empty-table content width)]))
  
  ;; new-space: string -> space
  ;; Constructor with default empty properties.
  (define (new-space content)
    (make-space empty-table content))
  
  
  ;; new-fusion: string (listof dstx?) string -> fusion
  ;; Constructor with default empty properties.
  (define (new-fusion prefix children suffix)
    (cond
      [(empty? children)
       (make-fusion empty-table prefix (list empty-space-atom) suffix)]
      [else
       (make-fusion empty-table prefix (cons empty-space-atom children)
                    suffix)]))
  
  
  ;; dstx-property-names: dstx -> (listof symbol)
  ;; Returns the list of property names attached to this dstx.
  (define (dstx-property-names a-dstx)
    (table:keys (dstx-properties a-dstx)))
  
  
  ;; dstx-property-ref: dstx symbol -> any
  (define (dstx-property-ref a-dstx a-sym)
    (table:lookup a-sym (dstx-properties a-dstx)))
  
  
  ;; dstx-property-set: dstx symbol any -> dstx
  ;; Nondestructively set a property.
  (define (dstx-property-set a-dstx a-sym a-val)
    (let ([new-properties
           (table:insert a-sym a-val (dstx-properties a-dstx))])
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
  
  
  (define property-map/c (table:table-of/c symbol? any/c))
  
  (provide/contract
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
   [struct (fusion dstx)
           ([properties property-map/c]
            [prefix string?]
            [children (nelistof dstx?)]
            [suffix string?])]
   
   [dstx-property-names (dstx? . -> . (listof symbol?))]
   [dstx-property-ref (dstx? symbol? . -> . any)]
   [dstx-property-set (dstx? symbol? any/c . -> . dstx?)]
   
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