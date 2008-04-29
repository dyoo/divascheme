(module woot-gui-mixins mzscheme
  
  (require (lib "class.ss")
           (lib "plt-match.ss")
           (lib "mred.ss" "mred")
           "../dsyntax/dsyntax.ss"
           "msg-structs.ss"
           "woot-struct.ss"
           (prefix self-ip: "self-ip-address.ss")
           (prefix server: "server.ss")
           (prefix client: "client.ss")
           (only (lib "13.ss" "srfi") string-join))
  
  (provide woot-text-mixin
           woot-frame-mixin)
  
  ;; The woot-text-mixin is a function turning a diva-text% into one that
  ;; supports woot-specific operations.
  (define (woot-text-mixin super%)
    (network-mixin
     (structure-tracking-mixin
      super%)))
  
  
  
  
  
  ;; structure-tracking-mixin: diva-text% -> diva-text%
  ;; Mixin for dstx-text that detects structured edits.
  (define (structure-tracking-mixin super%)
    (class super%
      
      ;; We annotate every new structure with a host ip.
      (define self-ip #f)
      
      (define (initialize)
        (set! self-ip (self-ip:self-ip-address))
        (super-new))
      
      
      ;; get-self-ip: -> string
      ;; Returns the self-ip address
      (define/public (get-self-ip)
        self-ip)
      
      
      ;; Hook to add woot ids to new structures.
      (define/augment (decorate-new-dstx a-dstx)
        (let ([a-dstx (inner a-dstx decorate-new-dstx a-dstx)])
          (deep-attach-woot-ids a-dstx (get-self-ip))))
      
      
      ;; on-woot-structured-insert: dstx woot-id woot-id -> void
      ;; We define additional hooks for the network mixin to do its stuff.
      (define/pubment (on-woot-structured-insert a-dstx before-woot-id after-woot-id)
        (inner (void) on-woot-structured-insert a-dstx before-woot-id after-woot-id))
      
      
      ;; on-woot-structured-delete: woot-id -> void
      (define/pubment (on-woot-structured-delete woot-id)
        (inner (void) on-woot-structured-delete woot-id))
      
      
      ;; Hooks into on-structured-insert-before.
      ;; Given a structured insert appropriate for woot, we call out to
      ;; on-woot-structured-insert.
      (define/augment (on-structured-insert-before a-fcursor a-dstx)
        (when (not (dstx-from-unstructured-editing? a-dstx))
          (cond
            [(focus-younger/no-snap a-fcursor)
             (on-woot-structured-insert
              (deep-strip-local-ids a-dstx)
              (dstx-woot-id (cursor-dstx (focus-younger/no-snap a-fcursor)))
              (dstx-woot-id (cursor-dstx a-fcursor)))]
            [else
             ;; as an invariant, no insert-before should occur before the sentinel
             ;; element, so we should never see this situation...
             (error 'handle-structured-insert-before)]))
        (inner (void) on-structured-insert-before a-fcursor))
      
      
      ;; Hooks into on-structured-insert-after.
      ;; Given a structured insert appropriate for woot, we call out to
      ;; on-woot-structured-insert.
      (define/augment (on-structured-insert-after a-fcursor a-dstx)
        (when (not (dstx-from-unstructured-editing? a-dstx))
          (cond
            [(focus-older/no-snap a-fcursor)
             (on-woot-structured-insert
              (deep-strip-local-ids a-dstx)
              (dstx-woot-id (cursor-dstx a-fcursor))
              (dstx-woot-id (cursor-dstx (focus-older/no-snap a-fcursor))))]
            [else
             ;; Last element in a fusion or at the toplevel will have a nil right id.
             (on-woot-structured-insert a-dstx (dstx-woot-id (cursor-dstx a-fcursor)) #f)]))
        (inner (void) on-structured-insert-after a-fcursor))
      
      
      ;; Hooks into on-structured-delete.
      ;; Given a structured delete appropriate for woot, we call out to
      ;; on-woot-structured-insert.
      (define/augment (on-structured-delete a-fcursor)
        (when (not (dstx-from-unstructured-editing? (cursor-dstx a-fcursor)))
          (cond
            [(and (focus-younger/no-snap a-fcursor)
                  (focus-older/no-snap a-fcursor))
             (on-woot-structured-delete (dstx-woot-id (cursor-dstx a-fcursor)))]
            [(focus-younger/no-snap a-fcursor)
             (on-woot-structured-delete (dstx-woot-id (cursor-dstx a-fcursor)))]
            ;; The two cases below should never occur.
            [(focus-older/no-snap a-fcursor)
             (error 'handle-structured-delete)]
            [else
             (error 'handle-structured-delete)]))
        (inner (void) on-structured-delete a-fcursor))
      
      (initialize)))
  
  
  
  
  ;; woot-text-mixin: diva-text% -> diva-text%
  ;; Infrastructure for tying in woot stuff with the network and DivaScheme
  (define (network-mixin super%)
    (class super%
      (inherit queue-for-interpretation!
               get-self-ip
               get-top-level-window)
      
      (define woot-custodian (make-custodian))
      (define woot-client #f)
      
      
      (define (initialize)
        (super-new))
      
      ;; host-session: -> void
      ;; Brings up a dialog window to host a session.  Shows our ip, and
      ;; some message on how to get others to join.
      (define/public (host-session)
        (let ()
          (let ([url (start-local-server)])
            (start-network-client url)
            (message-box
             "Host session started"
             (format "Session started.\nOther hosts may join by using the session url: ~s"
                     url)))))
      
      ;; join-session: -> void
      ;; Brings up a dialog box asking which system to join to.
      (define/public (join-session)
        (let ([session-url (get-text-from-user "Join shared session"
                                               "Session url:"
                                               (get-top-level-window)
                                               (default-hosting-url))])
          (start-network-client session-url)
          (message-box
           "Connected to server"
           (format "Connected to session ~s." session-url))))
      
      
      ;; start-local-server: string -> string
      ;; Starts up the local server.
      (define (start-local-server)
        (parameterize ([current-custodian woot-custodian])
          (server:start-server default-port-number)))
      
      
      ;; start-network-client: string -> void
      ;; Starts up the client part of the server.
      (define (start-network-client url)
        (parameterize ([current-custodian woot-custodian])
          (set! woot-client (client:new-client url))))
      
      
      ;; on-woot-structured-insert: dstx woot-id woot-id -> void
      ;; Broadcast a structured insert.
      (define/augment (on-woot-structured-insert a-dstx before-woot-id after-woot-id)
        (when woot-client
          (client:client-send-message
           woot-client
           (msg->string
            (make-msg:insert (get-self-ip) a-dstx before-woot-id after-woot-id))))
        (inner (void) on-woot-structured-insert a-dstx before-woot-id after-woot-id))
      
      
      ;; on-woot-structured-insert: dstx woot-id woot-id -> void
      ;; Broadcast a structured delete.
      (define/augment (on-woot-structured-delete woot-id)
        (when woot-client
          (client:client-send-message
           woot-client
           (msg->string
            (make-msg:delete (get-self-ip) woot-id))))
        (inner (void) on-woot-structured-delete woot-id))
      
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
    (hosting-url default-port-number "default-session"))
  
  
  ;; hosting-url number string: -> string
  ;; Returns a string representing the connection url.
  (define (hosting-url port session-name)
    (format "http://~a:~a/~a" (self-ip:self-ip-address) port session-name))
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
  
  
  ;; fresh-woot-id: string -> woot-id
  ;; Returns a fresh woot id.
  (define (fresh-woot-id host-ip)
    (make-woot-id (next-logical-id) host-ip))
  
  
  ;; dstx-woot-id: dstx -> woot-id
  (define (dstx-woot-id a-dstx)
    (dstx-property-ref a-dstx 'woot-id (lambda () #f)))
  
  
  ;; deep-attach-woot-id: dstx string -> dstx
  ;; Attach new woot identifiers to any dstx that doesn't yet have one.
  (define (deep-attach-woot-ids a-dstx host-ip)
    (dstx-deepmap (lambda (a-dstx)
                    (cond
                      [(dstx-woot-id a-dstx)
                       a-dstx]
                      [else
                       (dstx-property-set a-dstx 'woot-id (fresh-woot-id host-ip))]))
                  a-dstx))
  
  
  ;; deep-strip-local-ids: dstx -> dstx
  ;; Given a dstx, rip out the local ids.
  (define (deep-strip-local-ids a-dstx)
    (dstx-deepmap (lambda (a-dstx)
                    (dstx-property-remove a-dstx 'local-id))
                  a-dstx))
  
  
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