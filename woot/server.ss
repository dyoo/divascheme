#lang scheme/base

(require web-server/web-server
         web-server/configuration/responders
         web-server/private/request-structs
         web-server/dispatchers/dispatch-lift
         scheme/list
         scheme/contract
         "self-ip-address.ss")

(provide/contract [start-server (number? . -> . string?)])


;; The server just holds a list of messages it's seen, as well as the last id it used.
(define-struct server-state (messages last-id) #:mutable)



;; start-server: number string -> void
;; Creates a web server under the current custodian.
;; Returns the url used to talk to this server.
(define (start-server port)
  (let ([shutdown (serve #:dispatch (make (main-dispatcher (make-server-state '() -1)))
                         #:port port)])
    (format "http://~a:~a/" (self-ip-address) port)))



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
(define ((main-dispatcher a-server-state) request)
  (cond
    [(pull? request)
     (handle-pull)]
    [(push? request)
     (handle-push)]
    [else
     ;; fixme: do dispatching based on the request.
     `(html (head (title "hello world"))
            (body (h1 "hello world")))]))


;; add-new-message: server-state string -> bytes.
;; Accumulates a new message to the server, incrementing the id.
(define (add-new-message! a-server-state a-message)
  (let ([new-id (add1 (server-state-last-id a-server-state))])
    (set-server-state-messages! (cons (list new-id a-message)
                                      (server-state-messages a-server-state)))
    (set-server-state-last-id! new-id)
    (string->bytes/utf-8 (number->string new-id))))

