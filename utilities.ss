(module utilities mzscheme
  (require (lib "etc.ss")
           (lib "list.ss")
           (lib "plt-match.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "errortrace-lib.ss" "errortrace"))
  
  (define voice-debug false)
  (define (voice-printf . args)
    (when voice-debug
      (apply printf args)))
  
  (provide print-current-stack-trace)
  (define (print-current-stack-trace)
    (with-handlers ([exn:fail? 
                     (lambda (exn)
                       (print-error-trace (current-output-port) exn))])
      (error 'print-current-stack-trace)))
  
  (provide trim-whitespace-prefix)
  (define (trim-whitespace-prefix text)
    (let ([m (regexp-match " *(.*)" text)])
      (if m
          (second m)
          text)))


  (provide end-cons)
  ;; end-cons : ('a list) 'a -> ('a list)
  (define (end-cons l a)
    (reverse (cons a (reverse l))))

  (provide list-gcd)

  ;; list-gcd : (('a list) list) -> ('a list)
  (define (list-gcd lst)
    (define ?
      (lambda args
        (if (empty? args)
            true
            (let* ([a (first args)]
                   [? (lambda (b) (equal? a b))])
              (andmap ? (rest args))))))

    (if (empty? lst)
        empty
        (let loop ([lst lst])
          (if (ormap empty? lst)
              empty
              (let ([a (map first lst)]
                    [b (map rest lst)])
                (if (apply ? a)
                    (cons (first a) (loop b))
                    empty))))))
  
  
  (provide filter-double)
  (define (filter-double xs)
    (define ht (make-hash-table 'equal))
    (define (seen? x) 
      (hash-table-get ht x (lambda () #f)))
    (define (mark! x)
      (hash-table-put! ht x #t))
    
    (let loop ([xs xs])
      (cond 
        [(empty? xs) '()]
        [(seen? (first xs)) 
         (loop (rest xs))]
        [else 
         (mark! (first xs))
         (cons (first xs) (loop (rest xs)))])))

  (provide list-ref/safe)
  ;; list-ref/safe : ('a list) int -> 'a
  ;; Calls list-ref with error.
  ;; i can be negative because of back. We do not control the value.
  (define (list-ref/safe lst i)
    (if (and (>= i 0) (< i (length lst)))
        (list-ref lst i)
        (raise (make-voice-exn (format "there are only ~a matches" (length lst))))))
  
  
  (provide line-index line-pos
           line-end-index line-end-pos
           line-text/index line-text/pos
           line-number)


  ;; line-index: string index -> index
  ;; Returns the index of the beginning of the line
  ;; that contains the input index.
  (define (line-index text index)
    (let loop ([i 0]
               [result 0])
      (cond [(= i (string-length text))
             result]
            [(= i index)
             result]
            [(char=? #\newline (string-ref text i))
             (loop (add1 i) (add1 i))]
            [else
             (loop (add1 i) result)])))
  
  
  ;; line-pos : string pos -> pos
  ;; Returns the position at the beginning of the line containing pos.
  (define (line-pos text pos)
    (index->pos (line-index text (pos->index pos))))

  
  
  ;; line-end-index : string index -> index
  (define (line-end-index text index)
    (let loop ([i index])
      (cond
        [(= i (string-length text)) i]
        [(char=? (string-ref text i) #\newline) i]
        [else
         (loop (add1 i))])))
  
  
  ;; line-end-pos : string pos -> pos
  (define (line-end-pos text pos)
    (index->pos (line-end-index text (pos->index pos))))

  
  ;; line-text/index : string index -> string
  ;; returns the line of text that contains index.
  (define (line-text/index text index)
    (substring text 
               (line-index text index)
               (line-end-index text index)))

    
  ;; line-text/pos : string pos -> string
  (define (line-text/pos text pos)
    (line-text/index text (pos->index pos)))
  
  
  
  
  ;; line-number: string number -> number
  ;; computes current line number, starting at line one.
  (define (line-number text pos)
    (let loop ([i 0]
               [count 1])
      (cond [(= i (pos->index pos))
             count]
            [(char=? (string-ref text i) #\newline)
             (loop (add1 i) (add1 count))]
            [else
             (loop (add1 i) count)])))
          
                 
  

  (provide compute-new-start-index/insert
           compute-new-end-index/insert
           compute-new-selection/insert
           compute-new-index/delete
           compute-new-selection/delete
           compute-new-selection/replace)

  ;; compute-new-start-index/insert : index index non-negative-integer -> index
  ;; We want to compute the new index of the 'current-index' knowing
  ;; we are inserting a string of length 'insertion-length' at
  ;; index 'insertion-index'.
  (define (compute-new-start-index/insert current-index insertion-index insertion-length)
    (cond
     [(< current-index insertion-index) current-index]
     [else (+ current-index insertion-length)]))

  ;; compute-new-end-index/insert : index index non-negative-integer -> index
  ;; This function differs from the start one on the behavior
  ;; if the current-index is equal to the insertion-index,
  ;; and in this case the recult of compute-new-end-index can be lesser then compute-new-start-index, so becareful.
  ;; This should be understood in the context of inserting text in a selection.
  (define (compute-new-end-index/insert current-index insertion-index insertion-length)
    (cond
     [(<= current-index insertion-index) current-index]
     [else (+ current-index insertion-length)]))

  ;; compute-new-selection/insert : index non-negative-integer index non-negative-integer -> (index non-negative-integer values)
  ;; Given a selection and an inserting text, it returns the new selection.
  (define (compute-new-selection/insert current-index current-length insertion-index insertion-length)
    (let* ([new-index     (compute-new-start-index/insert current-index insertion-index insertion-length)]
           [new-end-index (compute-new-end-index/insert (+ current-index current-length) insertion-index insertion-length)])
      (if (<= new-index new-end-index) ; the false case only happens when current-index == insertion-index and current-length == 0
          (values new-index (- new-end-index new-index))
          (values new-index 0))))

  ;; compute-new-index/delete : index index non-negative-integer -> index
  (define (compute-new-index/delete current-index deletion-index deletion-length)
    (cond
     [(<  current-index deletion-index) current-index]
     [(and (>= current-index deletion-index)
           (<= current-index (+ deletion-index deletion-length))) deletion-index]
     [else (- current-index deletion-length)]))

  ;; compute-new-selection/delete : index non-negative-integer index non-negative-integer -> (index non-negative-integer values)
  (define (compute-new-selection/delete current-index current-length deletion-index deletion-length)
    (let* ([new-start-index (compute-new-index/delete current-index deletion-index deletion-length)]
           [new-end-index   (compute-new-index/delete (+ current-index current-length) deletion-index deletion-length)])
      (values new-start-index (- new-end-index new-start-index))))

  ;; compute-new-selection/replace : index non-negative-integer index non-negative-integer non-negative-integer -> (index non-negative-integer values)
  (define (compute-new-selection/replace current-index current-length replace-index deletion-length insertion-length)
    (let-values ([(new-index new-length) (compute-new-selection/delete current-index current-length replace-index deletion-length)])
      (compute-new-selection/insert new-index new-length replace-index insertion-length)))


  (provide symbol/stx? prefix/string? prefix/symbol?)

  ;; symbol/stx? : syntax -> boolean
  (define (symbol/stx? stx)
    (symbol? (syntax-e stx)))

  ;; prefix/symbol? : symbol -> symbol -> boolean
  (define ((prefix/symbol? a) b)
    ((prefix/string? (symbol->string a)) (symbol->string b)))

  ;; prefix/string? : string -> string -> boolean
  (define ((prefix/string? a) b)
    (let ([a-len (string-length a)]
          [b-len (string-length b)])
      (and (<= a-len b-len)
           (string=? a (substring b 0 a-len)))))
  
  (provide syntax-begins-with/is-symbol? syntax-is-symbol? syntax-begins-with? identifier-match? tokenize-identifier)

  ;; syntax-begins-with/is-symbol? : symbol -> syntax -> boolean
  (define ((syntax-begins-with/is-symbol? symbol) stx)
    (or ((syntax-begins-with? symbol) stx)
        ((syntax-is-symbol?   symbol) stx)))
  
  ;; syntax-is-symbol? : symbol -> syntax -> boolean
  (define ((syntax-is-symbol? symbol) stx)
    (let ([stx-e (syntax-e stx)])
      (and (atomic? stx-e)
           (if (symbol? stx-e)
               (identifier-match? symbol stx-e)
               (eq? symbol (string->symbol (format (cond
                                                    [(string? stx-e) "\"~a\""]
                                                    [(char?   stx-e)  "#\\~a"]
                                                    [else                "~a"]) stx-e)))))))

  ;; syntax-begins-with? : symbol -> syntax -> boolean
  (define ((syntax-begins-with? symbol) stx)
    (let ([lst (stx->lst stx)])
      (and (not (empty? lst))
           ((syntax-is-symbol? symbol) (first lst)))))
  
  ;; This function returns true if the first parameter matched the rule up given the second parameter.
  ;; identifier-match? : symbol symbol -> boolean
  (define (identifier-match? symbol element)
    (or ((prefix/symbol? symbol) element)
        ;;(and (member (symbol->string symbol) (tokenize-identifier (symbol->string element))) true)))
        (let ([tokens (tokenize-identifier (symbol->string element))])
          (and (not (empty? tokens))
               ((prefix/string? (symbol->string symbol)) (first tokens))))))

  ;; tokenize-identifier : string -> (string list)
  (define (tokenize-identifier str)
    ;; count : (char -> boolean) (char list) -> non-negative-integer
    (define (count pred lst)
      (let loop ([lst lst])
        (cond
         [(empty? lst) 0]
         [(pred (first lst)) (add1 (loop (rest lst)))]
         [else 0])))
    (let loop ([lst (string->list str)])
      (cond
       [(empty? lst) empty]
       [else
        (let* ([nb-alpha-char (count char-alphabetic? lst)]
               [sub-alpha-str (substring (list->string lst) 0 nb-alpha-char)]
               [lst (string->list (substring (list->string lst) nb-alpha-char (length lst)))]
               [nb-non-alpha-char (count (lambda (a) (not (char-alphabetic? a))) lst)]
               [lst (string->list (substring (list->string lst) nb-non-alpha-char (length lst)))])
          (if (= 0 nb-alpha-char)
              (loop lst)
              (cons sub-alpha-str (loop lst))))])))
    
             
  
  
  (provide get-clipboard-content set-clipboard-content)
  
  ;; get-clipboard-content : void -> (union string false)
  (define (get-clipboard-content)
    (send the-clipboard get-clipboard-string 0))
  
  ;; set-clipbord-content : (union string false) -> void
  (define (set-clipboard-content text)
    (when text
      (send the-clipboard set-clipboard-string text 0)))
  
  
  
  
  (define print-mem-labels '())
  (provide print-mem)
  (define (print-mem label thunk)
    (set! print-mem-labels (cons label print-mem-labels))
    (let* ([a (current-memory-use)]
           [_1 (collect-garbage)]
           [b (current-memory-use)]
           [t1 (current-inexact-milliseconds)]
           [result (call-with-values thunk (lambda args args))]
           [t2 (current-inexact-milliseconds)]
           [c (current-memory-use)]
           [_2 (collect-garbage)]
           [d (current-memory-use)])
      (printf "PM ~a: before ~a,  after ~a, time ~a~n"
              print-mem-labels
              (round (/ (- a b) 1000))
              (round (/ (- c d) 1000))
              (- t2 t1))
      (set! print-mem-labels (rest print-mem-labels))
      (apply values result)))
  
  
  (provide print-mem*)
  (define-syntax (print-mem* stx)
    (syntax-case stx ()
      [(_ label e ...)
       (syntax/loc stx
         (print-mem label (lambda () e ...)))]))
  
  
  (provide reverse-take)
  ;; reverse-take: (listof X) number -> (listof X)
  ;; Returns the first n elements of lst in reverse order.
  (define (reverse-take lst n)
    (let loop ([lst lst]
               [n n]
               [acc empty])
      (cond
        [(= n 0) acc]
        [else
         (loop (rest lst) (sub1 n) (cons (first lst) acc))])))
  
  (provide map*)
  ;; map*: (X -> Y) (listof X) -> (listof Y)
  ;; map, but with some care to avoid generating garbage.
  (define (map* fn elts)
    (define (fast-path lst n)
      (cond
        [(empty? lst) elts]
        [else
         (let ([result (fn (first lst))])
           (cond
             [(eq? result (first lst))
              (fast-path (rest lst) (add1 n))]
             [else (slow-path (rest lst)
                              (cons result (reverse-take elts n)))]))]))
    
    (define (slow-path lst acc)
      (cond
        [(empty? lst) (reverse acc)]
        [else
         (slow-path (rest lst)
                    (cons (fn (first lst)) acc))]))
    (fast-path elts 0))
  
  
  
  
  
  (provide id)
  ;; id : 'a -> 'a
  ;;       x ->  y
  (define (id x) x)
  
  
  (provide or* atomic? atomic/stx? stx->lst gmap orgmap andgmap syntax-is-syntax? equal-syntax?)
  
  ;; or* : ('a list) -> 'a
  (define (or* args)
    (ormap id args))
  
  ;; atomic? : any -> boolean
  (define (atomic? x)
    (not (or (pair? x)
             (list? x)
             (vector? x))))
  
  ;; atomic/stx? : syntax -> boolean
  (define (atomic/stx? stx)
    (atomic? (syntax-e stx)))
  
  ;; stx->lst : syntax -> (syntax list)
  (define (stx->lst stx)
    (match (syntax-e stx)
           [(? atomic?)           empty]
           [(vector xs ...)       xs]
           [(list xs   ...)       xs]
           [(list-rest lst ... last) (append lst (list last))])) ; for things like '(a b c . d)
  
  ;; gmap : (syntax -> 'a) syntax -> ('a list)
  (define (gmap fn stx)
    (map fn (stx->lst stx)))

  ;; orgmap : (syntax -> 'a) syntax -> 'a
  (define (orgmap fn stx)
    (ormap fn (stx->lst stx)))

  ;; andgmap : (syntax -> 'a) syntax -> 'a
  (define (andgmap fn stx)
    (andmap fn (stx->lst stx)))

  ;; syntax-is-syntax? : syntax -> syntax -> boolean
  (define ((syntax-is-syntax? stx) sty)
    (equal-syntax? stx sty))

  ;; equal-syntax? : syntax syntax -> boolean
  (define (equal-syntax? stx1 stx2)
    (equal? (syntax-object->datum stx1) (syntax-object->datum stx2)))

  (provide list-equal? syntax<-symbol)
  ;; syntax<-symbol : symbol -> syntax
  (define (syntax<-symbol symbol)
    #`#,symbol)

  ;; I need it to compare 2 lists of syntax object.
  ;; list-equal? : ('a 'a -> boolean) -> ('a list) ('a list) -> boolean
  (define ((list-equal? equal?) l1 l2)
    (with-handlers ([(lambda args true) (lambda args false)])
      (andmap equal? l1 l2)))


  ;; This function converts the first element and the last element
  ;; into parenthesis, with form according to its parameter.
  (provide shape-paren)
  
  ;; shape-paren : (union false 'Round 'Square 'Curly) string -> string
  (define (shape-paren type text)
    ;; aux : char char -> string
    (define (aux open close)
      (format "~a~a~a" open (substring text 1 (sub1 (string-length text))) close))
    (match type
      [#f       text]
      ['Round  (aux #\( #\))]
      ['Square (aux #\[ #\])]
      ['Curly  (aux #\{ #\})]))
  
  
  (provide quoting-char?)
  ;; Returns true if the character appears to be a quoting char.
  (define (quoting-char? ch)
    (member ch (list #\` #\' #\, #\#)))
  
  
  ;; This function is to read the content of a file.
  ;; Use it for test ("engine.ss" of MzTake).
  (provide file->string)
  
  ;; file -> string : string -> string
  (define (file->string filename)
    (define input false)
    (dynamic-wind
     (lambda () (set! input (open-input-file filename)))
     (lambda () (list->string (let loop ([char (read-char input)])
                                (if (eof-object? char)
                                    empty
                                    (cons char (loop (read-char input)))))))
        (lambda () (close-input-port input))))


  ;; These functions are to convert a string to Scheme tree,
  ;; with syntax objects.
  (provide input->syntax-list string->syntax-list string->syntax)
  
  ;; input->syntax-list : input-port -> (syntax list)
  (define (input->syntax-list input-port)
    ;(letrec ([read-scheme-tree (lambda () (with-handlers ([list (lambda args (read-scheme-tree))]) (read-syntax 'voice:action:get-syntax input-port)))])
    (let ([read-scheme-tree (lambda () (read-syntax 'voice:action:get-syntax input-port))])
      (port-count-lines! input-port)
      (with-handlers
          ([(lambda args true)
            (lambda (exn)
              (raise (make-voice-exn "The parenthesis of the definitions text are not correctly balanced.")))])
        (let loop ([stx (read-scheme-tree)])
          (if (eof-object? stx)
              ()
              (cons stx (loop (read-scheme-tree))))))))
  
  ;; string->syntax-list : string -> (syntax list)
  (define (string->syntax-list text)
    (input->syntax-list (open-input-string text)))
  
  ;; string->syntax : string -> syntax
  (define (string->syntax text)
    (match (string->syntax-list text)
      [(list head tail ...) head]
      [_ (raise (make-voice-exn "string->syntax: empty text"))]))
  
  
  ;; Offset begins at 1 (syntax-first) for syntax object instead of 0.
  (provide syntax-first syntax-pos->index index->syntax-pos syntax-index syntax-end-position syntax-end-index pos->index index->pos syntax-position->mred-position mred-position->syntax-position)

  ;; This is from where the positions are counted in syntax-object.
  (define syntax-first (syntax-position (string->syntax "a")))
  
  (define (syntax-pos->index pos)
    (- pos syntax-first))
  
  (define (index->syntax-pos index)
    (+ index syntax-first))

  ;; Accessor for syntax element in index instead of position.
  (define (syntax-index stx)
    (syntax-pos->index (syntax-position stx)))

  ;; Accessors for the end position/index of an syntax object.
  (define (syntax-end-position stx)
    (+ (syntax-position stx)
       (syntax-span stx)))
  
  (define (syntax-end-index stx)
    (syntax-pos->index (syntax-end-position stx)))
  
  (define pos->index syntax-pos->index)
  (define index->pos index->syntax-pos)

  (define syntax-position->mred-position syntax-pos->index)
  (define mred-position->syntax-position index->syntax-pos)

  

  ;; Functions manipulating strings.
  (provide insert-text delete-text replace-text get-subtext/pos+len get-subtext/stx)
  
  ;; insert-text : string index string -> string
  (define (insert-text txt index tyt)
    (format "~a~a~a" (substring txt 0 index)
                      tyt
                     (substring txt index (string-length txt))))
  
  ;; delete-text : string index int -> string
  (define (delete-text txt index len)
    (cond
     [( = len 0) txt]
     [(< len 0) (delete-text txt (- index len) (- len))]
     [else 
      (format "~a~a" (substring txt 0 index)
                     (substring txt (+ index len) (string-length txt)))]))

  ;; replace-text : string index string int -> string
  (define (replace-text txt index tyt len)
    (if (< len 0)
        (replace-text txt (+ index len) tyt (- len))
        (format "~a~a~a" (substring txt 0 index)
                tyt
                (substring txt (+ index len) (string-length txt)))))
  
  ;; get-subtext/stx : string syntax -> string
  (define (get-subtext/stx text stx)
    (get-subtext/pos+len text (syntax-position stx) (syntax-span stx)))
  
  ;; get-subtext/pos+len : string pos integer -> string
  (define (get-subtext/pos+len text pos len)
    (if (<= 0 len)
        (substring text
                   (syntax-pos->index pos)
                   (syntax-pos->index (+ pos len)))
        (get-subtext/pos+len text (+ pos len) (- len))))


  ;; Functions on exception.
  (provide make-voice-exn
           voice-exn?
           voice-exn-message)
  
  (define (make-voice-exn text)
    (list 'voice-exn text))
  
  (define (voice-exn? exn)
    (match exn
           [(list 'voice-exn (? string? text)) true]
           [_ false]))

  (define (voice-exn-message exn)
    (match exn
           [(list 'voice-exn text) text]))

  (define (make-voice-exn/world text world)
    (list 'voice-exn/world text world))

  (provide make-voice-exn/world
	   voice-exn/world?
	   voice-exn/world-message
	   voice-exn/world-world)

  (define (voice-exn/world? exn)
    (match exn
      [(list 'voice-exn/world text world) true]
      [_ false]))

  (define (voice-exn/world-message exn)
    (match exn
      [(list 'voice-exn/world text world) text]))

  (define (voice-exn/world-world exn)
    (match exn
      [(list 'voice-exn/world text world) world]))
  
  (provide timef)
  (define (timef label thunk)
    (let-values ([(results cpu real gc)
                  (time-apply thunk empty)])
      (printf "timef ~a: cpu ~a   real ~a   gc ~a~n" label cpu real gc)
      (apply values results))))
