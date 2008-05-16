(module client mzscheme
  ;; A simple producer/consumer mailbox between the server and the client.
  
  (require (lib "contract.ss")
           (lib "async-channel.ss")
           (lib "url.ss" "net")
           (lib "uri-codec.ss" "net")
           (lib "plt-match.ss")
           (lib "list.ss")
           (lib "xml.ss" "xml"))
  
  (define-struct client (url last-seen-id mailbox outbox
                             polling-delay polling? worker-thread in-ch))
  
  
  
  (provide/contract
   ;; make-client: string -> client
   [new-client (string? . -> . any)]
   
   ;; client-url: client? -> string
   [client-url (client? . -> . string?)]
   
   ;; client-polling?: client? -> boolean
   [client-polling? (client? . -> . boolean?)]
   
   ;; client-mailbox: client -> async-channel
   [client-mailbox (client? . -> . async-channel?)]
   
   ;; client-start-polling: client -> void
   [client-start-polling (client? . -> . any)]
   
   ;; client-pause-polling: client -> void
   [client-pause-polling (client? . -> . any)]
   
   ;; client-send-message: client string -> void
   [client-send-message (client? any/c . -> . any)])
  
  
  ;; Wait 100 milliseconds between pulls.
  (define default-polling-delay 500)
  
  ;; start-client: string -> client
  ;; Begins a client that periodically polls
  ;; the text for new events.  It also accepts new events and sends
  ;; them out to the server.
  (define (new-client url)
    (let* ([client (make-client url ;; url
                                -1 ;; last-seen-id
                                (make-async-channel) ;; mailbox
                                (make-async-channel) ;; outbox
                                default-polling-delay ;; polling-delay
                                #t ; polling is initially turned on
                                'uninitialized ; thread will be momentarily initialized
                                (make-async-channel) ; channel for receiving commands.
                                )]
           [worker-t (thread (lambda () (worker-loop client)))])
      (set-client-worker-thread! client worker-t)
      client))
  
  
  
  ;; worker-loop: client -> void
  ;; Never terminates: the client worker thread maintains the state of the system.
  (define (worker-loop a-client)
    (let loop ()
      (let ([handle-polling-evt
             (wrap-evt (alarm-evt
                        (+ (current-inexact-milliseconds)
                           (client-polling-delay a-client)))
                       (lambda (evt)
                         (send-all-outbound a-client)
                         (pull-all-inbound a-client)))]
            [handle-in-ch-evt
             (wrap-evt (client-in-ch a-client)
                       (lambda (msg)
                         (match msg
                           ['pause-polling
                            (set-client-polling?! a-client #f)]
                           ['start-polling
                            (set-client-polling?! a-client #t)])))])
        (cond
          [(client-polling? a-client)
           (void (sync (choice-evt handle-polling-evt handle-in-ch-evt)))]
          [else
           (void (sync (choice-evt handle-in-ch-evt)))])
        (loop))))
  
  
  
  ;; client-pause-polling: client -> void
  (define (client-pause-polling a-client)
    (thread-resume (client-worker-thread a-client) (current-thread))
    (async-channel-put (client-in-ch a-client) 'pause-polling))
  
  
  ;; client-start-polling: client -> void
  (define (client-start-polling a-client)
    (thread-resume (client-worker-thread a-client) (current-thread))
    (async-channel-put (client-in-ch a-client) 'start-polling))
  
  
  
  ;; client-send-message: client string -> void
  ;; Sends a client message to the server.
  (define (client-send-message a-client a-message)
    (thread-resume (client-worker-thread a-client) (current-thread))
    (async-channel-put (client-outbox a-client) a-message))
  
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; send-all-outbound: client -> void
  ;; Sends out all outbound messages to the server.
  (define (send-all-outbound a-client)
    (let loop ()
      (cond
        [(and (client-polling? a-client)
              (async-channel-try-get (client-outbox a-client)))
         =>
         (lambda (msg)
           (with-handlers ([exn:fail?
                            (lambda (exn)
                              (printf "~a~n" exn)
                              #;(set-client-polling?! a-client #f)
                              (async-channel-put (client-outbox a-client) msg))])
             (let* ([encoded-params (alist->form-urlencoded
                                     `((action . "push")
                                       (msg . ,msg)))]
                    [url (string->url
                          (string-append (client-url a-client) "?" encoded-params))])
               ;; fixme: we really should be using put here!
               ;; we should also double check the return response...
               (close-input-port (get-pure-port url))))
           (loop))]
        [else
         (void)])))
  
  
  ;; with-resource-reclamation: (-> X) -> X
  ;; Evaluate a thunk with a custodian, cleaning up all resources consumed by the
  ;; application of the thunk.
  (define (with-resource-reclamation thunk)
    (let ([a-cust (make-custodian)])
      (dynamic-wind (lambda ()
                      (void))
                    (lambda ()
                      (parameterize ([current-custodian a-cust])
                        (thunk)))
                    (lambda ()
                      (custodian-shutdown-all a-cust)))))
  
  
  ;; pull-all-inbound: client -> void
  ;; Retrieves new messages from the server and puts them in the mailbox.
  (define (pull-all-inbound a-client)
    (with-resource-reclamation
     (lambda ()
       (let loop ([sexps (reverse (get-sexp-results a-client))])
         (cond
           [(empty? sexps)
            (void)]
           [else
            (match (first sexps)
              [(list id payload)
               (set-client-last-seen-id! a-client
                                         (max (client-last-seen-id a-client)
                                              id))
               (async-channel-put (client-mailbox a-client) payload)
               (loop (rest sexps))])])))))
  
  
  ;; get-sexp-results: input-port -> (listof (list number string))
  (define (get-sexp-results a-client)
    (with-handlers ([exn:fail? (lambda (exn)
                                 (printf "~a~n" exn)
                                 #;(set-client-polling?! a-client #f)
                                 '())])
      (let ([ip (get-pure-port (make-next-results-url a-client))])
        (let ([xml (read-xml/element ip)])
          (match (xml->xexpr xml)
            [(list 'html _
                   (list 'head _ _)
                   (list 'body _ (list 'p _ payload ...)))
             (let ([result (read (open-input-string (apply string-append payload)))])
               result)])))))
  
  
  ;; make-url: client -> url
  ;; Builds the url we need to get the next set of messages.
  (define (make-next-results-url a-client)
    (let* ([encoded-params (alist->form-urlencoded
                            `((action . "pull")
                              (last-seen . ,(number->string
                                             (client-last-seen-id a-client)))))]
           [url (string->url
                 (string-append (client-url a-client) "?" encoded-params))])
      url)))