(module move mzscheme
  (require "struct.ss"
           (lib "plt-match.ss")
           (lib "etc.ss")
           (lib "list.ss")
           (lib "lex.ss" "parser-tools")
           (lib "contract.ss"))
  
  ;; Currently unexposed: it breaks the caching we do.
  #;(provide current-tab-break-length
             current-line-break-mode)
  
  
  ;; A move is one of the following:
  (define-struct move () #f)
  (define-struct (move:no-op move) () #f)
  (define-struct (move:tab move) () #f)
  (define-struct (move:newline&forward move) (n f p) #f)
  (define-struct (move:seq move) (next first) #f)
  
  ;; Moves are meant to be opaque, though, so we do not export most of the move
  ;; stuff outside.
  
  (provide/contract
   [move?
    (any/c . -> . boolean?)]
   [apply-move
    (move? loc? . -> . loc?)]
   [move-compose
    (move? move? . -> . move?)]
   
   [after-displayed-string
    (loc? string? . -> . loc?)]
   [get-move-after-displayed-string
    (input-port? . -> . move?)]
   [get-move-after-dstx
    (dstx? . -> . move?)])
  
  
  
  ;; current-tab-break-length: the tab break length we use when we
  ;; see a #\tab.
  (define current-tab-break-length (make-parameter 8))
  
  
  ;; current-line-break-mode: (parameterof
  ;;                           (union 'linefeed 'return
  ;;                                  'return-linefeed
  ;;                                  'any 'any-one)
  ;; Tells the line-breaking-lexer what interpretation
  ;; to choose for line breakers, similar to what read-line uses.
  (define current-line-break-mode (make-parameter 'any))
  
  
  ;; move-compose: move move -> move
  ;;
  ;; Composes two moves together, assuming that the first move is applied
  ;; first, and then the next move afterward.
  (define (move-compose next-move first-move)
    (match (list next-move first-move)
      [(list _ (struct move:no-op ()))
       next-move]
      
      [(list (struct move:no-op ()) _)
       first-move]
      
      [(list (struct move:newline&forward (n1 f1 p1))
             (struct move:newline&forward (n2 f2 p2)))
       (cond [(= n1 0)
              (make-move:newline&forward n2 (+ f1 f2) (+ p1 p2))]
             [else
              (make-move:newline&forward (+ n1 n2) f1 (+ p1 p2))])]
      
      [(list (struct move:newline&forward (n1 f1 p1))
             (struct move:seq ((struct move:newline&forward (n2 f2 p2))
                               rest-move)))
       (cond [(= n1 0)
              (make-move:seq (make-move:newline&forward n2 (+ f1 f2) (+ p1 p2))
                             rest-move)]
             [else
              (make-move:seq (make-move:newline&forward (+ n1 n2) f1 (+ p1 p2))
                             rest-move)])]
      [else
       (make-move:seq next-move first-move)]))
  
  
  ;; apply-move: move loc -> loc
  ;;
  ;; Applies a move on a-loc.
  ;;
  (define (apply-move a-move a-loc)
    (local ((define (multiple-nearest n mul)
              (* mul (quotient n mul))))
      (match a-move
        [(struct move:no-op ())
         a-loc]
        [(struct move:tab ())
         (make-loc (loc-line a-loc)
                   (multiple-nearest
                    (+ (loc-col a-loc) (current-tab-break-length))
                    (current-tab-break-length))
                   (add1 (loc-pos a-loc)))]
        [(struct move:newline&forward (n f p))
         (cond [(= n 0)
                (make-loc (loc-line a-loc)
                          (+ (loc-col a-loc) f)
                          (+ p (loc-pos a-loc)))]
               [else
                (make-loc (+ n (loc-line a-loc))
                          f
                          (+ p (loc-pos a-loc)))])]
        [(struct move:seq (next first))
         (apply-move next (apply-move first a-loc))])))
  
  
  ;; get-move-after-displayed-string: input-port -> move
  ;;
  ;; Returns the move we should do after seeing the content in ip.
  (define (get-move-after-displayed-string ip)
    (let loop ([a-move (begin-lifted (make-move:no-op))])
      (local ((define next-move (line-breaking-lexer ip)))
        (cond
          [next-move
           (loop (move-compose next-move a-move))]
          [else a-move]))))

  
  
  ;; after-printed-string: loc string -> loc
  ;;
  ;; Convenience function.  Returns the position after a-string, if it had
  ;; been displayed starting at the given a-loc.
  (define (after-displayed-string a-loc a-str)
    (apply-move (get-move-after-displayed-string (open-input-string a-str))
                a-loc))
  
  
  
  ;; line-breaking-lexer: input-port -> (union move #f)
  ;; Consumes a unit of text from the input port, and computes an
  ;; appropriate next moving action on a position, or #f if
  ;; we're all done.
  (define line-breaking-lexer
    (local
        ((define FORWARD (make-move:newline&forward 0 1 1))
         (define NL (make-move:newline&forward 1 0 1))
         (define NLNL (make-move:newline&forward 2 0 2))
         (define NL-FORWARD (make-move:newline&forward 1 1 2))
         (define TAB (make-move:tab))
         )
      (lexer
       ("\r\n"
        (case (current-line-break-mode)
          [(linefeed) NL]
          [(return) NL-FORWARD]
          [(return-linefeed) NL]
          [(any) NL]
          [(any-one) NLNL]))
       ("\n"
        (case (current-line-break-mode)
          [(linefeed) NL]
          [(return) FORWARD]
          [(return-linefeed) NL]
          [(any) NL]
          [(any-one) NL]))
       ("\r"
        (case (current-line-break-mode)
          [(linefeed) FORWARD]
          [(return) NL]
          [(return-linefeed) FORWARD]
          [(any) NL]
          [(any-one) NL]))
       ("\t"
        TAB)
       ((repetition 1 +inf.0 (char-complement (char-set "\n\r\t")))
        (make-move:newline&forward 0
                                   (string-length lexeme)
                                   (string-length lexeme)))
       ((eof) #f))))
  
  
  
  
  ;; weak-memoize: (X -> X) -> (X -> X)
  ;; Applies a weak memoization to see if we can speed up get-move-after-dstx.
  (define (weak-memoize f)
    (let ([ht (make-hash-table 'weak)])
      (lambda (x)
        (cond
          [(hash-table-get ht x #f)
           =>
           (lambda (x)
             x)]
          [else
           (let ([result (f x)])
             (hash-table-put! ht x result)
             result)]))))
  
  
  ;; get-move-after-dstx: dstx -> move
  ;;
  ;; Computes the move we want to apply after passing across a-dstx.
  (define get-move-after-dstx
    (weak-memoize
     (lambda (a-dstx)
       (cond
         [(atom? a-dstx)
          (after-atom-or-space (atom-content a-dstx))]
         [(special-atom? a-dstx)
          (make-move:newline&forward 0
                                     (special-atom-width a-dstx)
                                     (special-atom-width a-dstx))]
         [(space? a-dstx)
          (after-atom-or-space (space-content a-dstx))]
         [(fusion? a-dstx)
          (after-fusion a-dstx)]))))
  
  ;; after-atom-or-space: string -> move
  (define (after-atom-or-space a-str)
    (get-move-after-displayed-string (open-input-string a-str)))
  
  ;; after-fusion: fusion -> move
  (define (after-fusion an-sexp)
    (local ((define move-after-lparen
              (get-move-after-displayed-string
               (open-input-string (fusion-prefix an-sexp))))
            
            (define move-before-rparen
              (foldl (lambda (stx previous-move)
                       (move-compose (get-move-after-dstx stx)
                                     previous-move))
                     move-after-lparen
                     (fusion-children an-sexp)))
            
            (define move-after-rparen
              (move-compose (get-move-after-displayed-string
                             (open-input-string
                              (fusion-suffix an-sexp)))
                            move-before-rparen)))
      move-after-rparen)))