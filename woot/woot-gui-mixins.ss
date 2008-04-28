(module woot-gui-mixins mzscheme
  
  (require (lib "class.ss")
           (lib "plt-match.ss")
           (lib "mred.ss" "mred")
           "../structures.ss"
           "../dsyntax/dsyntax.ss"
           "self-ip-address.ss"
           (prefix server: "server.ss")
           (prefix client: "client.ss")
           (only (lib "13.ss" "srfi") string-join))
  
  (provide woot-text-mixin
           woot-frame-mixin)
  
  ;; A woot-id is a (list a-number host-string)
  ;; where a-number is a number, and host-string is a string containing an ip address.
  
  
  
  ;; Infrastructure for tying in woot stuff into DivaScheme
  ;; woot-text-mixin: diva-text% -> diva-text%
  (define (woot-text-mixin super%)
    (class super%
      (inherit queue-for-interpretation! get-top-level-window)
      
      (define woot-custodian (make-custodian))
      
      (define/public (get-woot-custodian)
        woot-custodian)
      
      
      
      (define host-ip (self-ip-address))
      
      (define (initialize)
        (super-new))
      
      
      (define/augment (decorate-new-dstx a-dstx)
        (let ([a-dstx (inner a-dstx decorate-new-dstx a-dstx)])
          (attach-woot-ids a-dstx)))
      
      
      ;; Attach new woot identifiers to any dstx that doesn't yet have one.
      (define (attach-woot-ids a-dstx)
        (dstx-deepmap (lambda (a-dstx)
                        (cond
                          [(dstx-property-ref a-dstx 'woot-id (lambda () #f))
                           a-dstx]
                          [else
                           (dstx-property-set a-dstx 'woot-id
                                              (fresh-woot-id host-ip))]))
                      a-dstx))
      
      
      
      
      (define (handle-structured-insert-before a-fcursor a-dstx)
        #;(printf "inserted ~s~n" (dstx->string a-dstx))
        (void))
      
      (define (handle-structured-insert-after a-fcursor a-dstx)
        #;(printf "inserted ~s~n" (dstx->string a-dstx))
        (void))
      
      (define (handle-structured-delete a-fcursor a-dstx)
        #;(printf "deleted ~s~n" (dstx->string a-dstx))
        (void))
      
      
      
      
      
      
      ;; Hooks into on-structured-insert-before.
      (define/augment (on-structured-insert-before a-fcursor a-dstx)
        (when (not (dstx-from-unstructured-editing? a-dstx))
          (handle-structured-insert-before a-fcursor a-dstx))
        (inner (void) on-structured-insert-before a-fcursor))
      
      ;; Hooks into on-structured-insert-after.
      (define/augment (on-structured-insert-after a-fcursor a-dstx)
        (when (not (dstx-from-unstructured-editing? a-dstx))
          (handle-structured-insert-after a-fcursor a-dstx))
        (inner (void) on-structured-insert-after a-fcursor))
      
      ;; Hooks into on-structured-delete.
      (define/augment (on-structured-delete a-fcursor)
        (when (not (dstx-from-unstructured-editing? (cursor-dstx a-fcursor)))
          (handle-structured-delete a-fcursor (cursor-dstx a-fcursor)))
        (inner (void) on-structured-delete a-fcursor))
      
      
      
      
      
      ;; host-session: -> void
      ;; Brings up a dialog window to host a session.  Shows our ip, and
      ;; some message on how to get others to join.
      (define/public (host-session)
        (let ([session-name
               (get-text-from-user "Host shared session"
                                   "Choose a session name:"
                                   (get-top-level-window)
                                   (or (send this get-filename)
                                       "default-session"))])
          (parameterize ([current-custodian (get-woot-custodian)])
            (let ([url (start-local-server session-name)])
              (start-network-client url)
              (message-box
               (format "Session started.\nOther hosts may connect by entering the session url: ~s"
                       url))))))
      
      
      ;; join-session: -> void
      ;; Brings up a dialog box asking which system to join to.
      (define/public (join-session)
        (let ([session-url (get-text-from-user "Join shared session"
                                               "Session url:"
                                               (get-top-level-window)
                                               (default-hosting-url))])
          (start-network-client session-url)))
      
      
      
      ;; start-local-server: string -> string
      ;; Starts up the local server.
      (define (start-local-server session-name)
        (parameterize ([current-custodian (get-woot-custodian)])
          (server:start-server default-port-number session-name)))
      
      
      ;; start-network-client: string -> void
      ;; Starts up the client part of the server.
      (define (start-network-client url)
        (parameterize ([current-custodian (get-woot-custodian)])
          (client:start-client url this)))
      
      
      
      
      ;; Just as an experiment, see that we can queue the following for interpretation.
      #;(thread (lambda ()
                  (let loop ()
                    (sleep 5)
                    (queue-for-interpretation! (make-No-op))
                    (loop))))
      
      (initialize)))
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
  ;; woot-frame-mixin: add the menu options for hosting and joining sessions.
  (define (woot-frame-mixin super%)
    (class super%
      (inherit get-diva-menu get-definitions-text)
      
      (define host-menu-item #f)
      (define join-menu-item #f)
      
      (define (initialize)
        (super-new)
        (set! host-menu-item
              (new menu-item%
                   [label "Host shared session..."]
                   [parent (get-diva-menu)]
                   [callback (lambda (menu-item control-event)
                               (send (get-definitions-text) host-session))]))
        (set! join-menu-item
              (new menu-item%
                   [label "Join shared session..."]
                   [parent (get-diva-menu)]
                   [callback (lambda (menu-item control-event)
                               (send (get-definitions-text) join-session))])))
      
      
      (initialize)))
  
  
  (define default-port-number 44444)
  
  ;; default-hosting-url: -> string
  ;; Returns a string representing a default hosting connection.
  (define (default-hosting-url)
    (hosting-url (self-ip-address) default-port-number "default-session"))
  
  
  ;; hosting-url number string: -> string
  ;; Returns a string representing the connection url.
  (define (hosting-url port session-name)
    (format "http://~a:~a/~a" (self-ip-address) port session-name))
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
  
  
  ;; fresh-woot-id: string -> woot-id
  ;; Returns a fresh woot id.
  (define (fresh-woot-id host-ip)
    (list (next-logical-id) host-ip))
  
  
  ;; dstx-from-unstructured-editing?: dstx -> boolean
  ;; Returns true if we're certain that the dstx came from intermediate insert-mode.
  (define (dstx-from-unstructured-editing? a-dstx)
    (dstx-property-ref a-dstx 'from-unstructured-editing (lambda ()
                                                           #f)))
  
  
  ;; dstx->string: dstx -> string
  ;; For debugging.
  (define (dstx->string a-dstx)
    (match a-dstx
      [(struct atom (props content))
       content]
      [(struct special-atom (props content width))
       (cond [(is-a? content string-snip%)
              (send content get-text 0 (send content get-count))]
             [else
              (format "~s" content)])]
      [(struct space (props content))
       content]
      [(struct fusion (props prefix children suffix))
       (format "~s ~a ~s"
               prefix
               (string-join (map (lambda (a-dstx)
                                   (format "~s" (dstx->string a-dstx)))
                                 children)
                            " ")
               suffix)]))
  
  
  
  ;; next-logical-id: -> number
  ;; Returns the next logical clock id.
  (define next-logical-id
    (let ([n 0])
      (lambda ()
        (set! n (add1 n))
        n))))