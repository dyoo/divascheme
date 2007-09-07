(module old-cleanup-whitespace mzscheme
  (require (lib "list.ss")
           (lib "contract.ss")
           "rope.ss")
  
  ;; The original cleanup code.
  
  
  ;; cleanup-whitespace : rope index (index list) -> (values rope (index list))
  (define (cleanup-whitespace a-rope index markers)
    (if (= (rope-length a-rope) 0)
        (values a-rope markers)
        (let-values
            ([(c m)
              (eat-whitespace index (vector->list (rope->vector a-rope)) markers)])
          (values (vector->rope (list->vector c)) m))))
  
  
  (define (non-line-breaking-whitespace? achar)
    (and (char? achar)
         (char-whitespace? achar)
         (not (char=? achar #\newline))))
  
  (define (skip-to index chars markers fn)
    (if (empty? (rest chars))
        (values empty (decrease> index markers))
        (fn index (rest chars) (decrease> index markers))))
  
  (define (output-to index chars markers fn)
    (if (empty? (rest chars))
        (values chars markers)
        (let-values ([(c m) (fn (add1 index) (rest chars) markers)])
          (values (cons (first chars) c) m))))
  
  (define (decrease> index markers)
    (map (lambda (m) (if (> m index) (sub1 m) m)) markers))
  
  (define (next-state chars)
    (cond [(non-line-breaking-whitespace? (first chars))
           eat-whitespace]
          [(open-paren? (first chars)) eat-whitespace]
          [(pipe? (first chars)) pipe]
          [(double-quote? (first chars)) double-quote]
          [else textchar]))
  
  (define (eat-whitespace index chars markers)
    (cond [(non-line-breaking-whitespace? (first chars))
           (skip-to index chars markers eat-whitespace)]
          [else (output-to index chars markers (next-state chars))]))
  
  (define (pipe index chars markers)
    (cond [(pipe? (first chars)) (output-to index chars markers textchar)]
          [else (output-to index chars markers pipe)]))
  
  (define (double-quote index chars markers)
    (cond [(double-quote? (first chars)) (output-to index chars markers textchar)]
          [else (output-to index chars markers double-quote)]))
  
  (define (textchar index chars markers)
    (cond [(not (non-line-breaking-whitespace? (first chars)))
           (output-to index chars markers (next-state chars))]
          [(empty? (rest chars))
           (skip-to index chars markers (next-state chars))]
          [(non-line-breaking-whitespace? (second chars))
           (skip-to index chars markers textchar)]
          [(close-paren? (second chars))
           (skip-to index chars markers textchar)]
          [else (output-to index chars markers eat-whitespace)]))
  
  
  (define (open-paren? achar)
    (and (char? achar)
         (or (char=? achar #\()
             (char=? achar #\[)
             (char=? achar #\{))))
  
  (define (pipe? achar)
    (and (char? achar)
         (char=? achar #\|))) 
  
  (define (double-quote? achar)
    (and (char? achar)
         (char=? achar #\"))) 
  
  (define (close-paren? achar)
    (and (char? achar)
         (or (char=? achar #\))
             (char=? achar #\])
             (char=? achar #\}))))
  
  
  (provide/contract [cleanup-whitespace ((rope? natural-number/c (listof natural-number/c))
                                         . ->* .
                                         (rope? (listof natural-number/c)))]))