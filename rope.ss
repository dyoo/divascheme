(module rope mzscheme
  (require (planet "rope.ss" ("dyoo" "rope.plt" 2 2))
           (lib "contract.ss")
           (lib "etc.ss"))
  
  
  ;; rope->vector: rope -> (vectorof char-or-special)
  ;; Given a rope, returns a vector containing all of its items.
  (define (rope->vector a-rope)
    (local ((define vec (make-vector (rope-length a-rope))))
      (rope-fold (lambda (char-or-special index)
                   (vector-set! vec index char-or-special)
                   (add1 index))
                 0
                 a-rope)
      vec))
  
  ;; rope=?: rope rope -> boolean
  ;; Returns true if the two ropes have the same content.
  (define (rope=? rope-1 rope-2)
    (and (= (rope-length rope-1)
            (rope-length rope-2))
         (andmap (lambda (x y)
                   (cond
                     [(and (char? x) (char? y))
                      (char=? x y)]
                     ;; for the moment, treat snips as equal regardless
                     [(and (not (char? x)) (not (char? y)))
                      #t]))
                 (vector->list (rope->vector rope-1))
                 (vector->list (rope->vector rope-2)))))
  
  
  
  ;; The references to pos here is a number starting from 1.
  ;; Indexes start from zero.
  (define pos/c (and/c integer? (>/c 0)))
  (define index/c natural-number/c)
  
  (define (index->pos index) (+ index 1))
  (define (pos->index pos) (- pos 1))
  
  ;; line-index: rope index -> index
  ;; Returns the index of the beginning of the line
  ;; that contains the input index.
  (define (line-index text index)
    (let loop ([i 0]
               [result 0])
      (cond [(= i (rope-length text))
             result]
            [(= i index)
             result]
            [(and (char? (rope-ref text i))
                  (char=? #\newline (rope-ref text i)))
             (loop (add1 i) (add1 i))]
            [else
             (loop (add1 i) result)])))
  
  
  ;; line-pos : rope pos -> pos
  ;; Returns the position at the beginning of the line containing pos.
  (define (line-pos text pos)
    (index->pos (line-index text (pos->index pos))))
  
  
  ;; line-end-index : rope index -> index
  (define (line-end-index text index)
    (let loop ([i index])
      (cond
        [(= i (rope-length text)) i]
        [(and (char? (rope-ref text i))
              (char=? (rope-ref text i) #\newline))
         i]
        [else
         (loop (add1 i))])))
  
  
  ;; line-end-pos : rope pos -> pos
  (define (line-end-pos text pos)
    (index->pos (line-end-index text (pos->index pos))))
  
  
  ;; line-rope/index : rope index -> rope
  ;; returns the line of text that contains index.
  (define (line-rope/index text index)
    (subrope text
          (line-index text index)
          (line-end-index text index)))
  
  
  ;; line-rope/pos : rope pos -> rope
  (define (line-rope/pos text pos)
    (line-rope/index text (pos->index pos)))
  
  
  ;; line-number: rope number -> number
  ;; computes line number at pos, starting at line one.
  (define (line-number text pos)
    (let loop ([i 0]
               [count 1])
      (cond [(= i (pos->index pos))
             count]
            [(and (char? (rope-ref text i))
                  (char=? (rope-ref text i) #\newline))
             (loop (add1 i) (add1 count))]
            [else
             (loop (add1 i) count)])))
  
  
  
  
  (provide (all-from (planet "rope.ss" ("dyoo" "rope.plt" 2 2))))
  
  
  (provide/contract
   [rope->vector (rope? . -> . vector?)]
   [rope=? (rope? rope? . -> . boolean?)]
   
   [line-index (rope? index/c . -> . index/c)]
   [line-pos (rope? pos/c . -> . pos/c)]
   [line-end-index (rope? index/c . -> . index/c)]
   [line-end-pos (rope? pos/c . -> . pos/c)]
   [line-rope/index (rope? index/c . -> . rope?)]
   [line-rope/pos (rope? pos/c . -> . rope?)]
   [line-number (rope? pos/c . -> . pos/c)]))