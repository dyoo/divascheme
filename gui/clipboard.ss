(module clipboard mzscheme
  (require (lib "etc.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "contract.ss")
           "../rope.ss")
  
  ;; We do some kludgery here to store snip content in a clipboard.
  ;; If we try to set-clipboard-content with a snip, we remember
  ;; it with a few stateful variables.
  
  (define last-remembered-clip #f)
  (define last-remembered-clip-id #f)
  
  
  ;; get-clipboard-content : void -> (union rope false)
  (define (get-clipboard-content)
    (local ((define str/false
              (send the-clipboard get-clipboard-string 0)))
      (cond
        [(not str/false)
         #f]
        [(and last-remembered-clip-id
              (string=? str/false last-remembered-clip-id))
         last-remembered-clip]
        [else
         (string->rope str/false)])))
  
  
  ;; set-clipboard-content : (union rope false) -> void
  (define (set-clipboard-content a-rope)
    (when a-rope
      (set! last-remembered-clip a-rope)
      (cond [(rope-has-special? a-rope)
             (set! last-remembered-clip-id
                   (format "~a~n~nclipboard-id-for-special-content-~a"
                           (rope->string/erasing-specials a-rope)
                           (random)))
             (send the-clipboard set-clipboard-string
                   last-remembered-clip-id
                   0)]
            [else
             (set! last-remembered-clip #f)
             (set! last-remembered-clip-id #f)
             (send the-clipboard set-clipboard-string
                   (rope->string a-rope)
                   0)])))
  
  (provide/contract [get-clipboard-content
                     (-> (or/c rope? false/c))]
                    [set-clipboard-content
                     ((or/c rope? false/c) . -> . any)]))