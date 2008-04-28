#lang scheme/base

(require web-server/web-server
         web-server/configuration/responders
         web-server/private/request-structs
         web-server/dispatchers/dispatch-lift
         scheme/list
         scheme/contract
         "self-ip-address.ss")

(provide/contract [start-server (number? string? . -> . string?)])


;; start-server: number string -> void
;; Creates a web server under the current custodian.
;; Returns the url used to talk to this server.
(define (start-server port session-name)
  (let ([shutdown (serve #:dispatch (make main-dispatcher) #:port port)])
    (format "http://~a:~a/~a"
            (self-ip-address))))


;; The interaction is simply:
;; Client sends a request with the following information:
;;
;; url represents the session name
;; new-operations represents the new operations to queue up
;; last-seen is either #f or a woot id.
;;
;; The response is all of the operations with a woot-id greater than last-seen.

(define (main-dispatcher request)
  (void)
  #;`(html (head (title "External IP Address"))
           (body (h1 "External IP Address")
                 (p ,(get-client-ip request)))))




