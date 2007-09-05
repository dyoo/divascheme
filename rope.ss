(module rope mzscheme
  (require (all-except (planet "rope.ss" ("dyoo" "rope.plt" 2 3))
                       open-input-rope)
           (only (lib "13.ss" "srfi") string-count string-fold)
           (lib "contract.ss")
           (lib "etc.ss")
           (lib "port.ss")
           (lib "list.ss"))
  
  ;; open-input-rope: rope -> input-port
  (define (open-input-rope a-rope)
    (local (;; pipe-f: -> (values inp outp)
            ;; Builds a pipe for input and output. We do some logic here
            ;; because make-pipe is faster: if we don't have specials, then
            ;; we can take advantage of it.
            (define pipe-f
              (cond
                [(rope-has-special? a-rope)
                 make-pipe-with-specials]
                [else
                 make-pipe]))
            (define-values (inp outp)
              (pipe-f)))
      (-rope-fold/leaves (lambda (string/special _)
                           (cond
                             [(string? string/special)
                              (when (> (string-length string/special) 0)
                                (display string/special outp))]
                             [else
                              (write-special string/special outp)]))
                         #f
                         a-rope)
      (close-output-port outp)
      inp))
  
  
  ;; rope->vector: rope -> (vectorof char-or-special)
  ;; Given a rope, returns a vector containing all of its items.
  (define (rope->vector a-rope)
    (local ((define vec (make-vector (rope-length a-rope))))
      (-rope-fold (lambda (char-or-special index)
                    (vector-set! vec index char-or-special)
                    (add1 index))
                  0
                  a-rope)
      vec))
  
  
  ;; vector->rope: (vectorof char special) -> rope
  ;; Inverts rope->vector.
  (define (vector->rope a-vec)
    (let loop ([i 0]
               [acc rope-empty])
      (cond [(= i (vector-length a-vec))
             acc]
            [(char? (vector-ref a-vec i))
             (loop (add1 i)
                   (rope-append
                    acc
                    (string->rope (string (vector-ref a-vec i)))))]
            [else
             (loop (add1 i)
                   (rope-append
                    acc
                    (special->rope (vector-ref a-vec i))))])))
  
  
  ;; rope=?: rope rope -> boolean
  ;; Returns true if the two ropes have the same content.
  (define (rope=? rope-1 rope-2)
    (or (eq? rope-1 rope-2)
        (and (= (rope-length rope-1)
                (rope-length rope-2))
             (begin
               (equal? (rope->vector rope-1)
                       (rope->vector rope-2))))))
  
  
  
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
    (let loop ([i (sub1 (min index (rope-length text)))])
      (cond
        [(< i 0) 0]
        [else
         (local ((define ch/special (rope-ref text i)))
           (cond
             [(and (char? ch/special)
                   (char=? ch/special #\newline))
              (add1 i)]
             [else
              (loop (sub1 i))]))])))
  
  
  ;; line-pos : rope pos -> pos
  ;; Returns the position at the beginning of the line containing pos.
  (define (line-pos text pos)
    (index->pos (line-index text (pos->index pos))))
  
  
  ;; line-end-index : rope index -> index
  ;; retusn the position at the end of the line containing index.
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
    (-subrope text
              (line-index text index)
              (line-end-index text index)))
  
  
  ;; line-rope/pos : rope pos -> rope
  (define (line-rope/pos text pos)
    (line-rope/index text (pos->index pos)))
  
  
  ;; line-number: rope number -> number
  ;; computes line number at pos, starting at line one.
  (define (line-number a-rope pos)
    (local ((define (accum-line-count string/special acc)
              (cond [(string? string/special)
                     (+ acc
                        (string-count string/special
                                      (lambda (x)
                                        (char=? x #\newline))))]
                    [else acc])))
      (rope-fold/leaves accum-line-count
                        1
                        (subrope a-rope 0 (pos->index pos)))))
  
  
  (define -subrope
    (case-lambda
      [(a-rope start end)
       (subrope a-rope start end)]
      [(a-rope start)
       (subrope a-rope start)]))
  
  (define (-rope-fold f acc a-rope)
    (rope-fold f acc a-rope))
  
  (define (-rope-fold/leaves f acc a-rope)
    (rope-fold/leaves f acc a-rope))
  
  
  ;; rope-count-whitespace: rope -> natural-number
  ;; Returns the number of whitespace characters in a rope.
  (define (rope-count-whitespace a-rope)
    (local ((define (f string-or-special current-count)
              (cond
                [(string? string-or-special)
                 (+ current-count
                    (count-whitespace-in-string string-or-special))]
                [else
                 current-count]))
            
            (define (count-whitespace-in-string a-str)
              (string-fold (lambda (ch acc)
                             (case ch
                               [(#\space #\tab #\newline #\return)
                                (add1 acc)]
                               [else
                                acc]))
                           0
                           a-str)))
      (rope-fold/leaves f 0 a-rope)))
  
  
  ;; rope-leading-whitespace: rope -> rope
  ;; Returns the leading whitespace at the head of a-rope.
  (define (rope-leading-whitespace a-rope)
    (let/ec return
      (rope-fold/leaves
       (lambda (string/special acc)
         (cond [(string? string/special)
                (cond
                  [(regexp-match #rx"^[ \t\n]*$" string/special)
                   (rope-append acc (string->rope string/special))]
                  [(regexp-match #rx"^[ \t\n]*" string/special)
                   =>
                   (lambda (result)
                     (return
                      (rope-append acc (string->rope (first result)))))])]
               [else
                (return acc)]))
       (string->rope "")
       a-rope)))
  
  
  (define rope-space (string->rope " "))
  (define rope-empty (string->rope ""))
  
  
  (provide (all-from-except (planet "rope.ss" ("dyoo" "rope.plt" 2 3))
                            subrope
                            rope-fold
                            rope-fold/leaves))
  
  
  (provide/contract
   [open-input-rope (rope? . -> . input-port?)]
   
   [rename -subrope subrope
           (case->
            (rope? natural-number/c natural-number/c . -> . rope?)
            (rope? natural-number/c . -> . rope?))]
   [rename -rope-fold rope-fold
           (any/c any/c rope? . -> . any)]
   [rename -rope-fold/leaves rope-fold/leaves
           (any/c any/c rope? . -> . any)]
   
   [rope->vector (rope? . -> . vector?)]
   [vector->rope (vector? . -> . rope?)]
   [rope=? (rope? rope? . -> . boolean?)]
   
   [line-index (rope? index/c . -> . index/c)]
   [line-pos (rope? pos/c . -> . pos/c)]
   [line-end-index (rope? index/c . -> . index/c)]
   [line-end-pos (rope? pos/c . -> . pos/c)]
   [line-rope/index (rope? index/c . -> . rope?)]
   [line-rope/pos (rope? pos/c . -> . rope?)]
   [line-number (rope? pos/c . -> . pos/c)]
   
   [rope-count-whitespace (rope? . -> . natural-number/c)]
   [rope-leading-whitespace (rope? . -> . rope?)]
   
   [rope-space rope?]
   [rope-empty rope?]))
