(module traversal mzscheme
  (require (lib "plt-match.ss")
           (lib "etc.ss")
           (lib "list.ss")
           (only (lib "1.ss" "srfi") last find)
           "utilities.ss"
           "templates.ss")
  
  (provide in-syntax?
           in-syntax-strict?
           syntax-list-last-position
           metric-nearest
           metric-forward
           metric-backward
           metric-magic
           find-all
           find-all/not-first
           sort/metric
           find-all/metric
           find-all-forward
           find-all-backward
           find-all-nearest
           find-all-magic
           find-pos
           find-pos-near
           find-pos-parent
           find-pos-spine
           positions-within-least-common-parent
           find-pos-updown
           find-ellipsis
           find-siblings-ellipsis
           find-all-ellipsis
           find-pos-sibling-forward
           find-pos-sibling-backward
           find-pos-mark-forward
           find-all-pos-mark-forward
           find-placeholder
           find-placeholder/symbol
           find-distance/metric
           find-pos-fill-forward
           find-rank)
  
  
  (define voice-debug false)
  (define (voice-printf . args)
    (when voice-debug
      (apply printf args)))
  
  
  ;; in-syntax? : pos syntax -> boolean
  (define (in-syntax? pos stx)
    (and (<= (syntax-position stx) pos)
         (<  pos (syntax-end-position stx))))
  
  ;; in-syntax-strict? : pos syntax -> boolean
  (define (in-syntax-strict? pos stx)
    (and (< (syntax-position stx) pos)
         (< pos (syntax-end-position stx))))
  
  ;; find-all : (syntax -> boolean) (syntax list) -> (syntax list)
  (define (find-all pred stx-list)
    (define (aux stx acc)
      (cond
        [(empty? (stx->lst stx))
         (if (pred stx) (cons stx acc) acc)]
        [(pred stx) (foldl aux (cons stx acc) (stx->lst stx))]
        [else (foldl aux acc (stx->lst stx))]))
    
    (reverse (foldl aux empty stx-list)))
  
  ;; find-all/not-first : (syntax -> boolean) (syntax list) -> (syntax list)
  (define (find-all/not-first pred stx-list)
    ;; aux : syntax -> (syntax list)
    (define ((aux is-first?) stx)
      (let ([here (if (and (not is-first?) (pred stx))
                      (list stx)
                      empty)]
            [childs (if (empty? (stx->lst stx))
                        empty
                        (cons ((aux #t) (first (stx->lst stx)))
                              (map (aux #f) (rest (stx->lst stx)))))])
        (apply append here childs)))
    (apply append (map (aux #f) stx-list)))
  
  ;; find-pos : pos (syntax list) -> syntax/false
  (define (find-pos pos stx-list)
    (define (aux stx)
      (and (in-syntax? pos stx)
           (if (or (= (syntax-position stx) pos)
                   (atomic/stx? stx))
               stx
               (ormap aux (syntax->list stx)))))
    (ormap aux stx-list))
  
  (define (find-pos-near pos stx-list)
    (or (find-pos pos stx-list)
        (find-pos-sibling-backward pos stx-list)))
  
  ;; find-pos-parent : pos (syntax list) -> syntax/false
  (define (find-pos-parent pos stx-list)
    (define target (find-pos pos stx-list))
    (define (eq-target? stx) (eq? stx target))
    (define (aux stx)
      (and (in-syntax? pos stx)
           (if (or* (gmap eq-target? stx))
               stx
               (or* (gmap aux stx)))))
    (if target
        (ormap aux stx-list)
        (find-blank-parent pos stx-list)))

  
  ;; find-blank-parent : pos (syntax list) -> syntax/false
  ;; Finds the s-expression enclosing the position, assuming
  ;; there is no s-expression at that position.
  (define (find-blank-parent pos stx-list)
    (define (aux stx)
      (and (in-syntax? pos stx)
           (or (or* (gmap aux stx))
               stx)))
    (ormap aux stx-list))
  
  
  ;; find-pos-spline: pos (listof syntax) -> (listof syntax)
  ;; Returns the list of parent syntaxes, starting with the root,
  ;; including the syntax at pos, if it exists.
  (define (find-pos-spine pos stx-list)
    (define (aux stx)
      (and (in-syntax? pos stx)
           (let ([sub (or* (gmap aux stx))])
             (if sub
                 (cons stx sub)
                 (list stx)))))
    (or (ormap aux stx-list) '()))
  
  (define (find-pos-spine/pos pos stx-list)
    (let ([s (find-pos-spine pos stx-list)])
      (if (find-pos pos stx-list)
          s
          (append s (list pos)))))
  
  ;; positions-within-least-common-parent: pos pos (listof syntax) -> (values syntax/int syntax/int)
  (define (positions-within-least-common-parent pos1 pos2 stx-list)
    (let ([spine1 (find-pos-spine/pos pos1 stx-list)]
          [spine2 (find-pos-spine/pos pos2 stx-list)])
      
      (let loop ([s1 spine1]
                 [s2 spine2]
                 [p #f])
        (cond [(or (empty? s1)
                   (empty? s2)) (values p p)]
              [(eq? (first s1) (first s2))
               (loop (rest s1) (rest s2) (first s1))]
              [else (values (first s1) (first s2))]))))
  
  
  (define (find-pos-on-line line stx-list)
    (define (aux stx)
      (if (< line (syntax-line stx))
          empty
          (let ([sub (apply append (gmap aux stx))])
            (if (= line (syntax-line stx))
                (cons stx sub)
                sub))))
    (apply append (map aux stx-list)))
  
  
  (define (find-pos-updown line column stx-list is-up?)
    (find-line-column
     column
     (find-pos-on-line ((if is-up? sub1 add1) line) stx-list)))
  
  
  (define (find-line-column column all-stx)
    (define (compare a b)
      (let ([da (- (syntax-column a) column)]
            [db (- (syntax-column b) column)])
        (if (eq? (positive? da) (positive? db))
            (< (abs da) (abs db))
            (< da db))))
    (and (not (empty? all-stx))
         (first (mergesort all-stx compare))))
  
  
  ;; syntax-list-last-position : (syntax list) -> pos
  (define (syntax-list-last-position stx-list)
    (if (empty? stx-list)
        (index->syntax-pos 0)
        (syntax-last-position (first (reverse stx-list)))))
  
  ;; syntax-last-position : syntax -> pos
  (define syntax-last-position syntax-end-position)
  (define (syntax-last-position-old stx)
    (let ([lst (stx->lst stx)])
      (if (empty? lst)
          (syntax-end-position stx)
          (syntax-list-last-position lst))))
  
  ;; metric-forward : pos pos -> syntax -> non-negative-integer
  (define ((metric-forward base last) stx)
    (let ([p (syntax-position stx)])
      (if (>= p base)
          (- p base)
          (+ (- last base)
             (- p (index->syntax-pos 0))))))
  
  ;; metric-backward : pos pos -> syntax -> non-negative-integer
  (define ((metric-backward base last) stx)
    (let ([p (syntax-position stx)])
      (if (<= p base)
          (- base p)
          (+ (- base (index->syntax-pos 0))
             (- last p)))))
  
  ;; metric-nearest : pos -> syntax -> non-negative-integer
  (define ((metric-nearest base) stx)
    (abs (- base (syntax-position stx))))
  
  ;; metric-magic : pos -> syntax -> non-negative-integer
  (define ((metric-magic base) stx)
    (let ([p (syntax-position stx)])
      (if (< p base)
          (- base p)
          (add1 (- p syntax-first)))))
  
  ;; sort/metric : (syntax -> non-negative-integer) (syntax list) -> (syntax list)
  (define (sort/metric m stx-list)
    (define (m< a b) (< (m a) (m b)))
    (mergesort stx-list m<))
  
  ;; find-all/metric : (syntax -> boolean) (syntax -> non-negative-integer) (syntax list) -> (syntax list)
  (define (find-all/metric pred m stx-list)
    (sort/metric m  (find-all pred stx-list)))
  
  ;; find-all-forward  : (syntax -> boolean) pos (syntax list) -> (syntax list)
  (define (find-all-forward pred metric-base stx-list)
    (find-all/metric pred (metric-forward metric-base (syntax-list-last-position stx-list)) stx-list))
  
  ;; find-all-backward : (syntax -> boolean) pos (syntax list) -> (syntax list)
  (define (find-all-backward pred metric-base stx-list)
    (find-all/metric pred (metric-backward metric-base (syntax-list-last-position stx-list)) stx-list))
  
  ;; find-all-nearest  : (syntax -> boolean) pos (syntax list) -> (syntax list)
  (define (find-all-nearest pred metric-base stx-list)
    (find-all/metric pred (metric-nearest metric-base) stx-list))
  
  ;; find-all-magic : (syntax -> boolean) pos (syntax list) -> (syntax list)
  (define (find-all-magic pred metric-base stx-list)
    (find-all/metric pred (metric-magic metric-base) stx-list))
  
  ;; find-ellipsis : pos (syntax list) -> syntax/false
  (define (find-ellipsis pos stx-list)
    (let ([ellipsis-pair (find-siblings-ellipsis pos stx-list)])
      (and ellipsis-pair (first ellipsis-pair))))

  ;; find-siblings-ellipsis : pos (syntax list) -> (union (syntax x syntax) #f)
  (define (find-siblings-ellipsis pos stx-list)
    (let ([all (find-all-ellipsis pos stx-list)])
      (and (not (empty? all))
           (last all))))
  
  ;; find-all-ellipsis : pos (syntax list) -> (syntax syntax list)
  (define (find-all-ellipsis pos stx-list)
        
    (let loop ([stx-list stx-list])
      (cond
        [(empty? stx-list) empty]
        [(empty? (rest stx-list)) (loop (stx->lst (first stx-list)))]
        [(in-syntax? pos (first stx-list))
         (if (ellipsis/stx? (second stx-list))
             (cons (list (first stx-list)
                         (second stx-list))
                   (loop (stx->lst (first stx-list))))
             (loop (stx->lst (first stx-list))))]
        [else (loop (rest stx-list))])))
  
  ;; find-pos-sibling-forward: pos (syntax list) -> syntax/false
  ;; Returns the sibling syntax forward of the given position.
  (define (find-pos-sibling-forward pos stx-list)
    (define (after? stx) (<= pos (syntax-position stx)))
    (let* ([parent (find-pos-parent pos stx-list)])
      (if parent
          (find after? (stx->lst parent))
          (find after? stx-list))))
  
  
  
  ;; find-pos-sibling-forward: pos (syntax list) -> syntax/false
  ;; Returns the sibling syntax backward of the given position.
  (define (find-pos-sibling-backward pos stx-list)
    (define (before? stx) (< (syntax-position stx) pos))
    (let* ([parent (find-pos-parent pos stx-list)])
      (if parent
          (find before? (reverse (stx->lst parent)))
          (find before? (reverse stx-list)))))
  
  ;; find-placeholder : boolean pos (syntax list) -> syntax
  (define (find-placeholder backward? pos stx-list)
    (let ([lst ((if backward? find-all-backward find-all-forward) placeholder/stx? pos stx-list)])
      (if (empty? lst)
          #f
          (first lst))))
  
  ;; find-placeholder/symbol : symbol non-negative-integer (metric : syntax -> non-negative-integer) (syntax list) -> syntax
  (define (find-placeholder/symbol symbol number metric stx-list)
    (match (find-all-placeholder/symbol symbol metric stx-list)
      [(list) (raise (make-voice-exn "Unable to find such a placeholder."))]
      [lst (list-ref/safe lst number)]))
  
  ;; find-all-placeholder/symbol : symbol metric (syntax list) -> (syntax list)
  (define (find-all-placeholder/symbol symbol metric stx-list)
    (define (pred stx)
      (and (placeholder/stx? stx)
           (symbol=? (placeholder-e/stx stx) symbol)))
    (find-all/metric pred metric stx-list))
  
  ;; find-distance/metric : (syntax -> boolean) (syntax -> non-negative-integer) (syntax list) integer -> syntax
  (define (find-distance/metric pred metric stx-list distance)
    (list-ref/safe (find-all/metric pred metric stx-list) (sub1 distance)))
  
  ;; find-pos-mark-forward : pos (syntax list) -> (union syntax false)
  (define (find-pos-mark-forward pos stx-list)
    (match (find-all-pos-mark-forward pos stx-list)
      [(list)               false]
      [(list stx tail ...)    stx]))
  
  ;; find-all-pos-mark-forward : pos (syntax list) -> (syntax list)
  (define (find-all-pos-mark-forward pos stx-list)
    (let loop ([stx-list stx-list])
      (match stx-list
        [(list) empty]
        [(list stx tail ...) (cond
                               [(>= pos (syntax-end-position stx)) (loop tail)]
                               [(holder/ellipsis-tree? stx)        (loop tail)]
                               [(<= pos (syntax-position stx)) (cons stx tail)]
                               [else (append (loop (stx->lst stx)) tail)])])))
  
  ;; find-pos-fill-forward : pos (syntax list) -> (union syntax false)
  ;; Used by bring and push.
  (define (find-pos-fill-forward pos stx-list)
    (define (pred stx)
      (and (not (atomic/stx? stx))
           (in-syntax-strict? pos stx)))
    (match (find-all-nearest pred pos stx-list)
      [(list)               false]
      [(list stx lst ...)  (find-pos-mark-forward (syntax-end-position stx) stx-list)]))
  
  
  
  ;; find-rank-old : (syntax -> boolean) pos (syntax -> non-negative-integer) (syntax list) -> non-negative-integer
  (define (find-rank-old pred base metric stx-list)
    ;; aux : ((syntax list) syntax cons) -> non-negative-integer
    (define (aux lst)
      (match lst
        [(list _) 1]
        [(list stx sty tail ...)
         (if (<= base (syntax-position sty))
             0
             (add1 (aux (rest lst))))])) 
    (let* ([stx-list (find-all/metric pred metric stx-list)])
      (cond
        [(empty? stx-list) (raise (make-voice-exn "Unable to find this thing."))]
        [(<= base (syntax-position (first stx-list))) 0]
        [else
         (let ([len (length stx-list)]
               [n   (aux stx-list)])
           (voice-printf "LEN: ~a N: ~a ~n" len n)
           (cond
             [(n . >= . len) (sub1 len)]
             [else
              (let* ([stx1 (list-ref/safe stx-list n)]
                     [stx2 (list-ref/safe stx-list (add1 n))]
                     [d1   (- base (syntax-position stx1))]
                     [d2   (- (syntax-position stx2) base)])
                (cond
                  [(< d1 d2) n]
                  [(> d1 d2) (add1 n)]
                  [else n]))]))])))
  
  
  ;; find-rank : pos (syntax list) -> non-negative-integer
  (define (find-rank base stx-list)
    (define (aux stx-list)
      (cond
        [(empty? stx-list) 1]
        [(<= base (syntax-position (first stx-list))) 0]
        [else (add1 (aux (rest stx-list)))]))
    (let ([len (length stx-list)]
          [n   (aux stx-list)])
      (cond
        [(= n 0) 0]
        [(>= n len) (sub1 len)]
        [else
         (let* ([stx1 (list-ref stx-list (sub1 n))]
                [stx2 (list-ref stx-list       n )]
                [d1 (- base (syntax-position stx1))]
                [d2 (- (syntax-position stx2) base)])
           (cond
             [(< d1 d2) (sub1 n)]
             [(> d1 d2)       n ]
             [else n]))])))
  
  
  )