(module server mzscheme
  (require (lib "web-server.ss" "web-server")
           (lib "servlet.ss" "web-server")
           (lib "list.ss")
           (lib "contract.ss")
           "self-ip-address.ss")
  
  (provide/contract [start-server (number? . -> . string?)])
  
  
  ;; The server just holds a list of messages it's seen, as well as the last id it used.
  (define-struct server-state (messages last-id))
  
  
  
  ;; start-server: number string -> void
  ;; Creates a web server under the current custodian.
  ;; Returns the url used to talk to this server.
  #;(define (start-server port)
      (let ([shutdown (serve #:dispatch (make (main-dispatcher (make-server-state '() -1)))
                             #:port port)])
        (format "http://~a:~a/" (self-ip-address) port)))
  
  (define (start-server port)
    (void))
  
  
  
  ;; The interaction is simply:
  ;; Client sends a request with the following information:
  ;;
  ;; action: either pull or push
  ;;
  ;; If action is a push:
  ;;   msg: a string
  ;;
  ;; If action is a pull:
  ;;   last-seen is a number
  ;; The response will be a byte stream containing (id msg) s-expressions, up to and not including
  ;; the given last-seen.
  ;;
  ;; Fixme: add timeout because if the request just sits there, we might be in trouble.
  (define ((main-dispatcher a-state) request)
    (cond
      [(pull? request)
       (handle-pull a-state request)]
      [(push? request)
       (handle-push a-state request)]
      [else
       (error-response "Expected 'action")
       #;(handle-default a-server-state request)]))
  
  
  ;; lookup-single-binding: symbol request -> (or/c #f string)
  ;; Looks up a binding: if a single one exists, returns its value.
  ;; If more than one value exists, or no values exist at all, returns #f.
  (define (lookup-single-binding id request)
    (let ([bindings (request-bindings request)])
      (cond [(exists-binding? id bindings)
             (let ([vals (extract-bindings id bindings)])
               (cond [(empty? (rest vals))
                      (first vals)]
                     [else #f]))]
            [else
             #f])))
  
  ;; pull?: request -> boolean
  ;; Returns true if the request asks for messages
  (define (pull? request)
    (let ([val (lookup-single-binding 'action request)])
      (and val (string=? val "pull"))))
  
  
  ;; pull?: request -> boolean
  (define (push? request)
    (let ([val (lookup-single-binding 'action request)])
      (and val (string=? val "push"))))
  
  
  
  ;; handle-pull: server-state request -> response
  (define (handle-pull a-state request)
    (let ([cutoff-id (lookup-single-binding 'last-seen request)])
      (cond
        [cutoff-id
         (let ([msgs (collect-messages a-state (string->number cutoff-id))])
           (make-messages-response msgs))]
        [else
         (error-response "expected 'last-seen parameter")])))
  
  
  ;; make-messages-response: (listof (list number string)) -> response
  ;; Creates a response that formats all the requested messages.
  (define (make-messages-response msgs)
    `(html (head (title "Pull results"))
           (body
            (p ,(format "~s" msgs)))))
  
  
  
  ;; collect-messages: server-state number -> (listof (list number string))
  (define (collect-messages a-state cutoff-id)
    (let loop ([msgs (server-state-messages a-state)])
      (cond
        [(empty? msgs)
         '()]
        [(<= (first (first msgs)) cutoff-id)
         '()]
        [else
         (cons (first msgs)
               (loop (rest msgs)))])))
  
  
  ;; handle-push: server-state request -> response
  (define (handle-push a-state request)
    (let ([msg (lookup-single-binding 'msg request)])
      (cond
        [msg
         (let ([new-id (add-new-message! a-state msg)])
           `(html (head (title "Push result"))
                  (body
                   (p ,(format "Added ~s." new-id)))))]
        [else
         (error-response "expected 'msg")])))
  
  
  
  ;; handle-default: server-state request -> response
  ;; By default, just dump out all the messages for debugging purposes.
  (define (handle-default state request)
    (make-messages-response (server-state-messages state)))
  
  
  
  ;; error-response: string -> resposne
  ;; Makes a very silly response.
  (define (error-response msg)
    `(html (head (title "Error"))
           (body (p ,msg))))
  
  
  ;; add-new-message: server-state string -> number
  ;; Accumulates a new message to the server, incrementing the id, and returning that id.
  (define (add-new-message! a-state a-message)
    (let ([new-id (add1 (server-state-last-id a-state))])
      (set-server-state-messages! a-state
                                  (cons (list new-id a-message)
                                        (server-state-messages a-state)))
      (set-server-state-last-id! a-state new-id)
      new-id)))

