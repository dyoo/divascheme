(module long-prefix mzscheme
  (require (lib "list.ss"))
  (provide (all-defined))
  
  ;; long-prefix: string string -> string
  ;; Returns longest prefix shared by s1 and s2, matching case-insensitively.
  (define (long-prefix-ci s1 s2)
    (let loop ([i 0])
      (cond
        [(or (<= (string-length s1) i)
             (<= (string-length s2) i))
         (substring s1 0 i)]
        [(char-ci=? (string-ref s1 i)
                    (string-ref s2 i))
         (loop (add1 i))]
        [else (substring s1 0 i)])))
  
  
  ;; common-prefix-length: X X (X -> number) (X -> Y) (Y Y -> boolean) -> number
  ;; Returns the length of the common longest prefix between seq1 and seq2.
  (define (common-prefix-length seq1 seq2 len-f ref-f =?)
    (let ([l1 (len-f seq1)]
          [l2 (len-f seq2)])
      (let loop ([i 0])
        (cond
          [(or (<= l1 i) (<= l2 i))
           i]
          [(=? (ref-f seq1 i) (ref-f seq2 i))
           (loop (add1 i))]
          [else i]))))



  ;; common-suffix-length: X X (X -> number) (X -> Y) (Y Y -> boolean) -> number
  ;; Returns the length of the common longest suffix between seq1 and seq2.
  (define (common-suffix-length seq1 seq2 len-f ref-f =?)
    (let ([l1 (len-f seq1)]
          [l2 (len-f seq2)])
      (let loop ([i 0])
        (cond
          [(or (<= l1 i) (<= l2 i))
           i]
          [(=? (ref-f seq1 (- l1 i 1))
               (ref-f seq2 (- l2 i 1)))
           (loop (add1 i))]
          [else i]))))

  
  (define (common-prefix&suffix-lengths seq1 seq2 len-f ref-f =?)
    (let* ([suffix-length (common-suffix-length seq1 seq2 len-f ref-f =?)]
           [prefix-length (common-prefix-length seq1 seq2 len-f ref-f =?)]
           [real-prefix-length (min prefix-length
                                    (- (len-f seq1) suffix-length)
                                    (- (len-f seq2) suffix-length))])
      (values real-prefix-length suffix-length)))
  
  
  
  ;; common-long-prefix: (nonempty-listof string) -> string
  (define (common-long-prefix-ci strs)
    (let/ec exit
      (foldl (lambda (s1 acc)
               (if (= (string-length acc) 0)
                   (exit "")
                   (long-prefix-ci s1 acc)))
             (first strs) (rest strs)))))