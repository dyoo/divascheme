(module client mzscheme
  ;; A simple producer/consumer mailbox between the server and the client.
  
  (require (lib "contract.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "async-channel.ss"))
  
  (provide/contract
   ;; make-client: string -> client
   [new-client (string? (is-a?/c text%) . -> . any)]
   ;; client-mailbox: client -> async-channel
   [client-mailbox (client? . -> . async-channel?)]
   ;; client-send-message: client any -> void
   [client-send-message (client? any/c . -> . any)])
  
  
  
  (define-struct client (url last-seen-id mailbox polling-delay))
  
  
  ;; start-client: string -> client
  ;; Begins a client that periodically polls
  ;; the text for new events.  It also accepts new events and sends
  ;; them out to the server.
  (define (new-client url)
    (let ([client (make-client url #f (make-async-channel) 5)])
      (thread (lambda () (polling-loop client)))
      client))
  
  
  ;; polling-loop: client -> void
  ;; Never terminates: calls polling-loop continuously.
  (define (polling-loop a-client)
    (client-pull)
    (sleep (client-polling-delay a-client)))
  
  
  ;; client-send-message: client message -> void
  ;; Sends a client message to the server.
  (define (client-send-message a-client a-message)
    (void))
  
  
  ;; Retrieves new messages from the server and puts them in the mailbox.
  (define (client-pull a-client)
    (void))
  
  
  
  )