(module client mzscheme
  ;; A simple producer/consumer mailbox between the server and the client.
  
  (require (lib "contract.ss")
           (lib "async-channel.ss")
           (lib "url.ss" "net")
           (lib "uri-codec.ss" "net")
           (lib "plt-match.ss")
           (lib "list.ss")
           (lib "xml.ss" "xml"))
  
  (define-struct client (url last-seen-id mailbox polling-delay polling? worker-thread in-ch))
  
  
  
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
  (define default-polling-delay 100)
  
  ;; start-client: string -> client
  ;; Begins a client that periodically polls
  ;; the text for new events.  It also accepts new events and sends
  ;; them out to the server.
  (define (new-client url)
    (let* ([client (make-client url
                                -1
                                (make-async-channel)
                                default-polling-delay
                                #t ; polling is initially turned on
                                'uninitialized ; thread will be momentarily initiallized
                                (make-channel))]
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
                         (client-pull a-client)))]
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
    (channel-put (client-in-ch a-client) 'pause-polling))
  
  
  ;; client-start-polling: client -> void
  (define (client-start-polling a-client)
    (thread-resume (client-worker-thread a-client) (current-thread))
    (channel-put (client-in-ch a-client) 'start-polling))
  
  
  
  ;; client-send-message: client string -> void
  ;; Sends a client message to the server.
  (define (client-send-message a-client a-message)
    (thread-resume (client-worker-thread a-client) (current-thread))
    (let* ([encoded-params (alist->form-urlencoded
                            `((action . "push")
                              (msg . ,a-message)))]
           [url (string->url
                 (string-append (client-url a-client) "?" encoded-params))])
      ;; fixme: we really should be using put here!
      ;; we should also double check the return response...
      (close-input-port (get-pure-port url))))
  
  
  
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
  
  
  ;; client-pull: client -> void
  ;; Retrieves new messages from the server and puts them in the mailbox.
  (define (client-pull a-client)
    (with-resource-reclamation
     (lambda ()
       (let* ([encoded-params (alist->form-urlencoded
                               `((action . "pull")
                                 (last-seen . ,(number->string (client-last-seen-id a-client)))))]
              [url (string->url
                    (string-append (client-url a-client) "?" encoded-params))])
         (let ([ip (get-pure-port url)])
           (let loop ([sexps (reverse (get-sexp-results ip))])
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
                   (loop (rest sexps))])])))))))
  
  
  ;; get-sexp-results: input-port -> (listof (list number string))
  (define (get-sexp-results ip)
    (match (xml->xexpr (read-xml/element ip))
      [(list 'html _
             (list 'head _ _)
             (list 'body _ (list 'p _ payload)))
       (read (open-input-string payload))])))