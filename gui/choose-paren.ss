(module choose-paren mzscheme
  (require (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework"))
  
  ;; This is very much a copy-and-paste of private behavior within
  ;; framework/private/scheme.ss.  I'm adjusting it for DivaScheme usage.
  ;; I don't like copy-and-paste, but I'm not seeing a good way at reusing
  ;; the existing functionality without doing worse entangling things.
  
  (provide choose-paren
           get-contextual-open-cmd)
  
  ;; get-contextual-open-cmd: scheme-text% symbol -> symbol
  ;; Given a scheme-text% and a default-cmd, returns DrScheme's
  ;; choice for paren opening as either 'Open or 'Open-Square.
  ;; Otherwise, returns the default command.
  (define (get-contextual-open-cmd editor default-cmd)
    (cond
      [(preferences:get 'framework:fixup-open-parens)
       (case (choose-paren editor)
         [(#\()
          'Open]
         [(#\[)
          'Open-Square]
         [(#\{)
          ;; Fixme: interpreter.ss has no provision for
          ;; curly open at the moment!
          'Open-Square])]
      [else default-cmd]))
  
  
  (define (call-in-eventspace-thread thunk)
    (cond
      [(eq? (current-thread) (eventspace-handler-thread (current-eventspace)))
       (thunk)]
      [else
       (let ([ch (make-channel)])
         (queue-callback (lambda ()
                           (let ([result (thunk)])
                             (channel-put ch result)))
                         #t)
         (sync ch))]))
  
  
  (define (with-unstructured-decoration editor f)
    (let ([old-val (send editor in-unstructured-editing?)])
      (dynamic-wind (lambda ()
                      (send editor set-in-unstructured-editing? #t))
                    f
                    (lambda ()
                      (send editor set-in-unstructured-editing? old-val)))))
  
  
  
  ;; choose-paren: scheme-text% -> char
  ;; Returns the right square paren to use at the context where the cursor is.
  (define (choose-paren text)
    (call-in-eventspace-thread
     (lambda ()
       (let* ([pos (send text get-start-position)]
              [real-char #\[]
              [change-to (λ (i c)
                           ;(printf "change-to, case ~a\n" i)
                           (set! real-char c))]
              [start-pos (send text get-start-position)]
              [end-pos (send text get-end-position)]
              [letrec-like-forms (preferences:get 'framework:square-bracket:letrec)])
         (send text begin-edit-sequence #f #f)
         (send text set-position start-pos 'same #f #f 'local)
         (with-unstructured-decoration text
                                       (lambda ()
                                         (send text insert " " pos 'same #f)
                                         (send text insert "[" pos 'same #f)))
         (when (eq? (send text classify-position pos) 'parenthesis)
           (let* ([before-whitespace-pos (send text skip-whitespace pos 'backward #t)]
                  [keyword/distance (find-keyword-and-distance before-whitespace-pos text)])
             (cond
               [(and keyword/distance
                     (member keyword/distance
                             (preferences:get 'framework:square-bracket:cond/offset)))
                ;; just leave the square backet in, in this case
                (void)]
               [(and keyword/distance
                     (member (car keyword/distance)
                             (preferences:get 'framework:square-bracket:local)))
                (unless (= (cadr keyword/distance) 0)
                  (change-to 7 #\())]
               [else
                (let* ([backward-match (send text backward-match before-whitespace-pos 0)]
                       [b-m-char (and (number? backward-match) (send text get-character backward-match))])
                  (cond
                    [backward-match
                     ;; there is an expression before this, at this layer
                     (let* ([before-whitespace-pos2 (send text skip-whitespace backward-match 'backward #t)]
                            [backward-match2 (send text backward-match before-whitespace-pos2 0)])
                       
                       (cond
                         [(member b-m-char '(#\( #\[ #\{))
                          ;; found a "sibling" parenthesized sequence. use the parens it uses.
                          (change-to 1 b-m-char)]
                         [else
                          ;; otherwise, we switch to (
                          (change-to 2 #\()]))]
                    [(not (zero? before-whitespace-pos))
                     ;; this is the first thing in the sequence
                     ;; pop out one layer and look for a keyword.
                     (let ([b-w-p-char (send text get-character (- before-whitespace-pos 1))])
                       (cond
                         [(equal? b-w-p-char #\()
                          (let* ([second-before-whitespace-pos (send text skip-whitespace
                                                                     (- before-whitespace-pos 1)
                                                                     'backward
                                                                     #t)]
                                 [second-backwards-match (send text backward-match
                                                               second-before-whitespace-pos
                                                               0)])
                            (cond
                              [(not second-backwards-match)
                               (change-to 3 #\()]
                              [(and (beginning-of-sequence? text second-backwards-match)
                                    (ormap (λ (x) (text-between-equal? x
                                                                       text
                                                                       second-backwards-match
                                                                       second-before-whitespace-pos))
                                           letrec-like-forms))
                               ;; we found a let<mumble> keyword, so we get a square bracket
                               (void)]
                              [else
                               ;; go back one more sexp in the same row, looking for `let loop' pattern
                               (let* ([second-before-whitespace-pos2 (send text skip-whitespace
                                                                           second-backwards-match
                                                                           'backward
                                                                           #t)]
                                      [second-backwards-match2 (send text backward-match
                                                                     second-before-whitespace-pos2
                                                                     0)])
                                 (cond
                                   [(and second-backwards-match2
                                         (eq? (send text classify-position second-backwards-match)
                                              ;;; otherwise, this isn't a `let loop', it is a regular let!
                                              'symbol)
                                         (member "let" letrec-like-forms)
                                         (text-between-equal? "let"
                                                              text
                                                              second-backwards-match2
                                                              second-before-whitespace-pos2))
                                    ;; found the `(let loop (' so we keep the [
                                    (void)]
                                   [else
                                    ;; otherwise, round.
                                    (change-to 4 #\()]))]))]
                         [else
                          (change-to 5 #\()]))]
                    [else
                     (change-to 6 #\()]))])))
         (with-unstructured-decoration text
                                       (lambda ()
                                         (send text delete pos (+ pos 1) #f)
                                         (send text delete pos (+ pos 1) #f)))
         (send text end-edit-sequence)
         (send text set-position start-pos end-pos #f #f 'local)
         real-char))))
  
  
  
  ;; find-keyword-and-distance : -> (union #f (cons string number))
  (define (find-keyword-and-distance before-whitespace-pos text)
    ;; searches backwards for the keyword in the sequence at this level.
    ;; if found, it counts how many sexps back it was
    (let loop ([pos before-whitespace-pos]
               [n 0])
      (let ([backward-match (send text backward-match pos 0)])
        (cond
          [backward-match
           (let ([before-whitespace-pos (send text skip-whitespace backward-match 'backward #t)])
             (loop before-whitespace-pos
                   (+ n 1)))]
          [else
           (let* ([afterwards (send text get-forward-sexp pos)]
                  [keyword
                   (and afterwards
                        (send text get-text pos afterwards))])
             (and keyword
                  (list keyword (- n 1))))]))))
  
  ;; beginning-of-sequence? : text number -> boolean
  ;; determines if this position is at the beginning of a sequence
  ;; that begins with a parenthesis.
  (define (beginning-of-sequence? text start)
    (let ([before-space (send text skip-whitespace start 'backward #t)])
      (cond
        [(zero? before-space) #t]
        [else
         (equal? (send text get-character (- before-space 1)) 
                 #\()])))
  
  (define (text-between-equal? str text start end)
    (and (= (string-length str) (- end start))
         (let loop ([i (string-length str)])
           (cond
             [(= i 0) #t]
             [else
              (and (char=? (string-ref str (- i 1))
                           (send text get-character (+ i start -1)))
                   (loop (- i 1)))])))))