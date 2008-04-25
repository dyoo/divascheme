#lang scheme/base

(require web-server/web-server
         web-server/configuration/responders
         web-server/private/request-structs
         web-server/dispatchers/dispatch-lift
         scheme/list)

(define (start-server)
  (serve #:dispatch (make my-responder)
         #:port 6432))

(define (get-client-ip request)
  (let loop ([headers (request-headers/raw request)])
    (cond [(empty? headers)
           (error 'get-client-ip)]
          [(bytes=? #"X-Forwarded-For" (header-field (first headers)))
           (bytes->string/utf-8 (header-value (first headers)))]
          [else
           (loop (rest headers))])))


(define (my-responder request)
  `(html (head (title "External IP Address"))
         (body (h1 "External IP Address")
               (p ,(get-client-ip request)))))


(start-server)
(do-not-return)
