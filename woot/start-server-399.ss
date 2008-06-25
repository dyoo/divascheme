#lang scheme/base
(require scheme/contract
         "server.ss"
         "self-ip-address.ss"
         (lib "web-server.ss" "web-server")
         (prefix-in dispatch-lift: (lib "dispatch-lift.ss" "web-server" "dispatchers")))


(provide/contract [start-server (number? . -> . string?)])

;; start-server: number -> string
;; Creates a web server under the current custodian.
;; Returns the url used to talk to this server.
(define (start-server port)
  (let ([shutdown (serve #:dispatch (dispatch-lift:make (new-dispatcher))
                         #:port port)])
    (format "http://~a:~a/" "127.0.0.1" port)))