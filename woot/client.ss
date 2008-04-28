(module client mzscheme
  ;; A simple producer/consumer mailbox between the server and the client.
  
  (require (lib "contract.ss")
           (lib "mred.ss" "mred")
           (lib "async-channel.ss")
           (lib "url.ss" "net")
           (lib "uri-codec.ss" "net")
           (lib "plt-match.ss")
           (lib "list.ss")
           (lib "xml.ss" "xml"))
  
  (define-struct client (url last-seen-id mailbox polling-delay))
  
  
  
  (provide/contract
   ;; make-client: string -> client
   [new-client (string? . -> . any)]
   ;; client-mailbox: client -> async-channel
   [client-mailbox (client? . -> . async-channel?)]
   ;; client-send-message: client string -> void
   [client-send-message (client? any/c . -> . any)])
  
  
  
  (define default-polling-delay 0.5)
  
  ;; start-client: string -> client
  ;; Begins a client that periodically polls
  ;; the text for new events.  It also accepts new events and sends
  ;; them out to the server.
  (define (new-client url)
    (let ([client (make-client url -1 (make-async-channel) default-polling-delay)])
      (thread (lambda () (polling-loop client)))
      client))
  
  
  ;; polling-loop: client -> void
  ;; Never terminates: calls polling-loop continuously.
  (define (polling-loop a-client)
    (client-pull a-client)
    (sleep (client-polling-delay a-client))
    (polling-loop a-client))
  
  
  ;; client-send-message: client string -> void
  ;; Sends a client message to the server.
  (define (client-send-message a-client a-message)
    (let* ([encoded-params (alist->form-urlencoded
                            `((action . "push")
                              (msg . ,a-message)))]
           [url (string->url
                 (string-append (client-url a-client) "?" encoded-params))])
      ;; fixme: we really should be using put here!
      ;; we should also double check the return response...
      (close-input-port (get-pure-port url))))
  
  
  ;; client-pull: client -> void
  ;; Retrieves new messages from the server and puts them in the mailbox.
  (define (client-pull a-client)
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
                (printf "Adding ~a~n" payload)
                (async-channel-put (client-mailbox a-client) payload)
                (loop (rest sexps))])])))))
  
  
  ;; get-sexp-results: input-port -> (listof (list number string))
  (define (get-sexp-results ip)
    (match (xml->xexpr (read-xml/element ip))
      [(list 'html _
             (list 'head _ _)
             (list 'body _ (list 'p _ payload)))
       (read (open-input-string payload))])))