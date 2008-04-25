(module self-ip-address mzscheme
  ;; dyoo@cs.wpi.edu
  ;;
  ;; This is a _very_ kludgy way to do this, but I have a web server set up on
  ;; http://ip.hashcollision.org whose sole purpose is to return the ip address
  ;; of the client.  We use this as a platform-independent way to get the ip address.
  ;;
  ;; I would rather we do something smarter, but I'm under time pressure at the moment.
  
  (require (lib "url.ss" "net")
           (lib "xml.ss" "xml")
           (lib "plt-match.ss")
           (lib "contract.ss"))
  
  (provide/contract [self-ip-address
                     (-> string?)])
  
  
  ;; url where the simple application lives.
  (define ip-url (string->url "http://ip.hashcollision.org/"))
  
  
  ;; get-expr-body: url -> xexpr
  (define (get-xexpr-body url)
    (xml->xexpr (read-xml/element (get-pure-port url))))
  
  
  ;; self-ip-address: -> string
  ;; Returns the ip address of this computer.
  (define (self-ip-address)
    (let ([xexpr (get-xexpr-body ip-url)])
      (match xexpr
        [(list 'html
               (list)
               (list 'head _ ...)
               (list 'body _
                     (list 'h1 _ ...)
                     (list 'p _ ip)))
         ip]))))