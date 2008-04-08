(module struct mzscheme
  ;; Provides fundamental structures.

  (require (lib "contract.ss")
           #;(lib "etc.ss")
           #;(planet "string-intern.ss" ("dyoo" "weak-map.plt" 1 0))
           #;(planet "make-hash-struct.ss" ("dyoo" "hash-cons.plt" 1 0)))
  
  
  ;; location: line and column.
  (define-struct loc (line col pos) #f)
  (provide/contract [struct loc ((line natural-number/c)
                                 (col natural-number/c)
                                 (pos natural-number/c))])
  
  
  
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
  
  (define-struct dstx () #f)
  (define-struct (atom dstx) (content) #f)
  (define-struct (special-atom dstx) (content) #f)
  (define-struct (space dstx) (content) #f)
  (define-struct (fusion dstx) (prefix children suffix) #f)
  
  
  (provide dstx struct:dstx
           atom struct:atom
           special-atom struct:special-atom
           space struct:space
           fusion struct:fusion)
  
  (provide/contract
   [dstx? (any/c . -> . boolean?)]
   [atom? (any/c . -> . boolean?)]
   [special-atom? (any/c . -> . boolean?)]
   [space? (any/c . -> . boolean?)]
   [fusion? (any/c . -> . boolean?)]
   [atom-content (atom? . -> . string?)]
   [special-atom-content (special-atom? . -> . any)]
   [space-content (space? . -> . string?)]
   [fusion-prefix (fusion? . -> . string?)]
   [fusion-suffix (fusion? . -> . string?)]
   [fusion-children (fusion? . -> . (listof dstx?))])
  
  (provide/contract
   [make-atom (string? . -> . atom?)]
   [make-special-atom (any/c . -> . special-atom?)]
   [make-space (string? . -> . space?)]
   [make-fusion (string? (listof dstx?) string? . -> . fusion?)])
  
  
  
  #|
  ;; Currently disabled, but I'm thinking that it would be interesting
  ;; to see if this really relieves memory pressure that DivaScheme
  ;; stresses on the system by experimenting with hash-consing.

  (define -make-atom
    (make-hash-struct make-atom 1 ('foobar) (set-atom-content!)))
  (define -make-space
    (make-hash-struct make-space 1 ("") (set-space-content!)))
  (define -make-fusion
    (local ((define hasher
              (make-hash-struct make-fusion 3 ("(" '() ")")
                                (set-fusion-prefix!
                                 set-fusion-children!
                                 set-fusion-suffix!))))
      (lambda (l-paren children r-paren)
        (hasher (string-intern l-paren) children (string-intern r-paren)))))
  (provide/contract
   [rename -make-atom make-atom (string? . -> . atom?)]
   [rename -make-space make-space (string? . -> . space?)]
   [rename -make-fusion make-fusion (string? (listof dstx?) string? . -> . fusion?)])
  |#
  
  
  ;; Cursors.  Zipper structure for efficient movement within a dstx,
  ;; also keeping track of our cursor position.
  (define-struct cursor
    (dstx loc parent youngers-rev youngers-loc-rev olders) #f)
  
  (provide/contract
   [struct cursor ((dstx dstx?)
                   (loc loc?)
                   (parent (or/c cursor? false/c))
                   (youngers-rev (listof dstx?))
                   (youngers-loc-rev (listof loc?))
                   (olders (listof dstx?)))])
  )