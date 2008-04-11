(module cursor mzscheme
  (require "struct.ss"
           "move.ss"
           (lib "contract.ss"))
  
  
  ;; cursor-line: cursor -> natural-number
  ;;
  ;; Returns the line where the cursor is focused.
  (define (cursor-line a-cursor)
    (loc-line (cursor-loc a-cursor)))
  
  
  ;; cursor-col: cursor -> natural-number
  ;;
  ;; Returns the column of the leftmost position where
  ;; the cursor is focused.
  (define (cursor-col a-cursor)
    (loc-col (cursor-loc a-cursor)))
  
  
  (define (cursor-pos a-cursor)
    (loc-pos (cursor-loc a-cursor)))
  
  (define (cursor-endloc a-cursor)
    (apply-move (get-move-after-dstx (cursor-dstx a-cursor))
                (cursor-loc a-cursor)))
  
  (define (cursor-endpos a-cursor)
    (loc-pos (cursor-endloc a-cursor)))
  
  (provide/contract [cursor-line (cursor? . -> . natural-number/c)]
                    [cursor-col (cursor? . -> . natural-number/c)]
                    [cursor-pos (cursor? . -> . natural-number/c)]
                    [cursor-endloc (cursor? . -> . loc?)]
                    [cursor-endpos (cursor? . -> . natural-number/c)]))