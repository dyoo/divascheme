(module rope mzscheme
  (require (planet "rope.ss" ("dyoo" "rope.plt" 3))
           (only (lib "13.ss" "srfi") string-count string-fold)
           (lib "contract.ss")
           (lib "etc.ss")
           (lib "port.ss")
           (lib "list.ss")
           (lib "plt-match.ss"))
  
  
  
  
  
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
    (subrope text
             (line-index text index)
             (line-end-index text index)))
  
  
  ;; line-rope/pos : rope pos -> rope
  (define (line-rope/pos text pos)
    (line-rope/index text (pos->index pos)))
  
  
  ;; line-number: rope number -> number
  ;; computes line number at pos, starting at line one.
  (define (line-number a-rope pos)
    (local ((define (accum-line-count string/special acc)
              (match string/special
                [(struct rope:string (s))
                 (+ acc
                    (string-count s (lambda (x) (char=? x #\newline))))]
                [(struct rope:special (s))
                 acc])))
      (rope-fold/leaves accum-line-count
                        1
                        (subrope a-rope 0 (pos->index pos)))))
  
  
  
  ;; rope-count-whitespace: rope -> natural-number
  ;; Returns the number of whitespace characters in a rope.
  (define (rope-count-whitespace a-rope)
    (local ((define (f string-or-special current-count)
              (match string-or-special
                [(struct rope:string (s))
                 (+ current-count
                    (count-whitespace-in-string s))]
                [(struct rope:special (s))
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
         (match string/special
           [(struct rope:string (s))
            (cond
              [(regexp-match #rx"^[ \t\n]*$" s)
               (rope-append acc string/special)]
              [(regexp-match #rx"^[ \t\n]*" s)
               =>
               (lambda (result)
                 (return
                  (rope-append acc (string->rope (first result)))))])]
           [(struct rope:special (s))
            (return acc)]))
       (string->rope "")
       a-rope)))
  
  
  (define rope-space (string->rope " "))
  (define rope-empty (string->rope ""))
  
  
  (provide (all-from (planet "rope.ss" ("dyoo" "rope.plt" 3))))
  
  
  (provide/contract
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
