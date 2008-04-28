(module client mzscheme
  (require (lib "contract.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred"))
  
  (provide/contract
   [start-client (string? (is-a?/c text%) . -> . any)])
  
  
  
  (define-struct client ())
  
  ;; start-client: string text% -> client
  ;; begins a client thread that periodically polls
  ;; the text for new events.  It also accepts new events and sends
  ;; them out to the server.
  (define (start-client url text%)
    (make-client)))