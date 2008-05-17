(module woot-gui-mixins mzscheme
  
  (require (lib "class.ss")
           (lib "plt-match.ss")
           (lib "mred.ss" "mred")
           (lib "async-channel.ss")
           (lib "list.ss")
           "../dsyntax/dsyntax.ss"
           "../structures.ss"
           "woot-struct.ss"
           "mock-woot.ss"
           "utilities.ss"
           (prefix self-ip: "self-ip-address.ss")
           (prefix server: "start-server.ss")
           (prefix client: "client.ss"))
  
  (provide woot-text-mixin
           woot-frame-mixin)
  
  ;; The woot-text-mixin is a function turning a diva-text% into one that
  ;; supports woot-specific operations.
  (define (woot-text-mixin super%)
    (network-mixin
     (structure-tracking-mixin
      super%)))
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; The following methods are meant to be internal to this module.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define-local-member-name
    with-remote-operation-active
    get-host-id
    on-woot-structured-insert
    on-woot-structured-delete
    host-session
    join-session)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define big-number 2147483647)
  
  ;; structure-tracking-mixin: diva-text% -> diva-text%
  ;; Mixin for dstx-text that detects structured edits.
  (define (structure-tracking-mixin super%)
    (class super%
      
      (inherit get-dstx-cursor)
      
      ;; We annotate every new structure with a self-ip and host ip.
      (define host-id #f)
      
      (define (initialize)
        (set! host-id (string-append
                       (with-handlers ([exn:fail? (lambda (exn) "unknown")])
                         (self-ip:self-ip-address))
                       "::"
                       (number->string (random big-number))))
        (super-new))
      
      
      ;; We need to detect when things are being evaluated because they're
      ;; driven by the outside world.  The in-remote-operation? is
      ;; meant to protect the potential feedback that might happen otherwise.
      (define in-remote-operation? #f)
      
      
      ;; with-remote-operation-active: (-> X) -> X
      ;; Evalutes the thunk in a dynamic environment where the in-remote-operation?
      ;; is true.
      (define/public (with-remote-operation-active thunk)
        (let ([old-val in-remote-operation?])
          (dynamic-wind
           (lambda () (set! in-remote-operation? #t))
           thunk
           (lambda () (set! in-remote-operation? old-val)))))
      
      
      ;; get-host-id: -> string
      ;; Returns the host identifer, which is some combination of ip and random number.
      (define/public (get-host-id)
        host-id)
      
      
      ;; woot-id->local-dstx: woot-id -> dstx
      ;; Given a woot-id, find the structure in the dstx tree and return it.
      (define/public (woot-id->local-dstx a-woot-id)
        (let ([a-cursor (get-dstx-cursor)])
          (send a-cursor focus-find/dstx!
                (lambda (a-dstx)
                  (woot-id-equal? (dstx-woot-id a-dstx)
                                  a-woot-id)))
          (send a-cursor cursor-dstx)))
      
      
      (define/augment (reparse-all-dstxs!)
        (set-toplevel-sentinel-woot-id!)
        (inner (void) reparse-all-dstxs!))
      
      
      ;; Ensures that the very first dstx, the sentinel space, has a canonical
      ;; woot id that all clients share.
      (define (set-toplevel-sentinel-woot-id!)
        (let ([cursor (get-dstx-cursor)])
          (send cursor focus-toplevel!)
          (send cursor property-set! 'woot-id first-sentinel-woot-id)))
      
      
      ;; Hook to add woot ids to new structures.
      (define/augment (decorate-new-dstx a-dstx)
        (let ([a-dstx (inner a-dstx decorate-new-dstx a-dstx)])
          (deep-attach-woot-ids a-dstx (get-host-id))))
      
      
      ;; on-woot-structured-insert: dstx woot-id woot-id -> void
      ;; We define additional hooks for the network mixin to do its stuff.
      (define/pubment (on-woot-structured-insert a-dstx after-woot-id before-woot-id)
        (when (not in-remote-operation?)
          (inner (void) on-woot-structured-insert a-dstx after-woot-id before-woot-id)))
      
      
      ;; on-woot-structured-delete: woot-id -> void
      (define/pubment (on-woot-structured-delete woot-id)
        (when (not in-remote-operation?)
          (inner (void) on-woot-structured-delete woot-id)))
      
      
      ;; Hooks into on-structured-insert-before.
      ;; Given a structured insert appropriate for woot, we call out to
      ;; on-woot-structured-insert.
      (define/augment (on-structured-insert-before a-fcursor a-dstx)
        (when (not (dstx-from-unstructured-editing? a-dstx))
          (cond
            [(focus-younger/no-snap a-fcursor)
             (on-woot-structured-insert (deep-strip-local-ids a-dstx)
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
             (on-woot-structured-insert (deep-strip-local-ids a-dstx)
                                        (dstx-woot-id (cursor-dstx a-fcursor))
                                        (dstx-woot-id (cursor-dstx (focus-older/no-snap a-fcursor))))]
            [else
             ;; Last element in a fusion or at the toplevel will have a nil right id.
             (on-woot-structured-insert (deep-strip-local-ids a-dstx)
                                        (dstx-woot-id (cursor-dstx a-fcursor))
                                        #f)]))
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
      (inherit interpret!
               get-host-id
               get-top-level-window
               get-dstx-cursor
               get-toplevel-dstxs
               woot-id->local-dstx
               diva-message
               with-remote-operation-active
               queue-callback/in-command-mode
               begin-edit-sequence
               end-edit-sequence)
      
      (define server-custodian (make-custodian))
      (define server-url #f)
      
      (define woot-custodian (make-custodian))
      (define network-mailbox-client #f)
      (define woot-state #f)
      (define notifier-widget #f)
      
      
      (define (initialize)
        (super-new))
      
      
      ;; host-session: -> void
      ;; Brings up a dialog window to host a session.  Shows our ip, and
      ;; some message on how to get others to join.
      (define/public (host-session)
        (let ([url (start-local-server)]
              [local-url (format "http://localhost:~a" default-port-number)])
          (start-network-client local-url)
          (message-box
           "Host session started"
           (format "Session started.\nOther hosts may join by using the session url: ~s.~nIf your host is under a NAT, it may be difficult for others to join."
                   url))))
      
      
      ;; close-hosting-session: -> void
      ;; Closes down the hosting session and notifies the user.
      (define/public (close-hosting-session)
        (maybe-shutdown-local-server)
        (maybe-shutdown-network-client)
        (message-box
         "Server stopped."
         "Hosting session closed."))
      
      
      ;; join-session: -> void
      ;; Brings up a dialog box asking which system to join to.
      (define/public (join-session)
        (let ([session-url (get-text-from-user "Join shared session"
                                               "Session url:"
                                               (get-top-level-window)
                                               (default-hosting-url))])
          (when session-url
            (start-network-client session-url)
            (message-box
             "Connected to server"
             (format "Connected to session.")))))
      
      
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      
      ;; start-local-server: string -> string
      ;; Starts up the local server.
      (define (start-local-server)
        (maybe-shutdown-local-server)
        (parameterize ([current-custodian server-custodian])
          (set! server-url
                (server:start-server default-port-number))
          server-url))
      
      
      ;; maybe-shutdown-local-server: -> void
      ;; Shuts down the local server if it was on.
      (define (maybe-shutdown-local-server)
        (when server-url
          (custodian-shutdown-all server-custodian)
          (set! server-url #f)
          (set! server-custodian (make-custodian))))
      
      
      ;; start-network-client: string -> void
      ;; Starts up the client part of the server.
      (define (start-network-client url)
        (maybe-shutdown-network-client)
        (parameterize ([current-custodian woot-custodian])
          (set! network-mailbox-client (client:new-client url))
          (set! woot-state (new-mock-woot initial-toplevel-cursor))
          (set! notifier-widget
                (new network-notifier-widget%
                     [parent (send (get-top-level-window) get-diva-panel)]
                     [woot-state woot-state]
                     [mailbox-client network-mailbox-client]))
          (start-process-client-message-loop!)
          (integrate-initial-state)))
      
      
      ;; maybe-shutdown-network-client: -> void
      ;; Shuts down the local client if it was on.
      (define (maybe-shutdown-network-client)
        (when notifier-widget
          (send notifier-widget detach!))
        
        (when woot-custodian
          (custodian-shutdown-all woot-custodian))
        (set! woot-custodian (make-custodian))
        (set! network-mailbox-client #f)
        (set! woot-state #f)
        (set! notifier-widget #f))
      
      
      
      ;; start-process-client-message-loop!: -> void
      ;; Starts up a thread that reads messages from the network and passes them off to
      ;; woot.
      (define (start-process-client-message-loop!)
        ;; message-handling thread
        (thread
         (lambda ()
           (let loop ()
             (sync
              (wrap-evt (client:client-mailbox network-mailbox-client)
                        (lambda (a-str)
                          (let ([msg (string->msg a-str)])
                            (dynamic-wind
                             (lambda ()
                               (begin-edit-sequence))
                             (lambda ()
                               (integrate-remote-message msg)
                               (for-each integrate-remote-message (collect-other-messages)))
                             (lambda ()
                               (end-edit-sequence)))))))
             (loop))))
        
        ;; error notification thread
        (thread
         (lambda ()
           (let loop ()
             (sync (wrap-evt (client:client-error-ch network-mailbox-client)
                             (lambda (exn)
                               (printf "~s~n" exn)
                               (diva-message
                                "Network error; pausing sync!")
                               (client:client-pause-polling network-mailbox-client)
                               (send notifier-widget refresh-message-panel!))))
             (loop)))))
      
      
      ;; collect-other-messages: -> (listof msg)
      ;; Returns the other messages that might be in the mailbox.
      (define (collect-other-messages)
        (let ([maybe-msg (async-channel-try-get
                          (client:client-mailbox network-mailbox-client))])
          (cond
            [maybe-msg
             (let ([msg (string->msg maybe-msg)])
               (cons msg (collect-other-messages)))]
            [else
             '()])))
      
      
      ;; integrate-initial-state: -> void
      ;; Go through the toplevel dstxs and send them over as if the user
      ;; had entered them in sequence.
      (define (integrate-initial-state)
        (let ([toplevel-dstxs (get-toplevel-dstxs)])
          (let loop ([dstxs toplevel-dstxs])
            (match dstxs
              [(list sentinel-space)
               (void)]
              [(list after-dstx a-dstx)
               (integrate-local-message-and-send-remotely
                (make-msg:insert (get-host-id)
                                 a-dstx
                                 (dstx-woot-id after-dstx)
                                 #f))]
              [(list after-dstx a-dstx others ...)
               (integrate-local-message-and-send-remotely
                (make-msg:insert (get-host-id)
                                 a-dstx
                                 (dstx-woot-id after-dstx)
                                 #f))
               (loop (rest dstxs))]))))
      
      
      ;; local-integrate-message-and-send-remotely: msg -> void
      ;; Given a message that was generated locally, integrate it into woot state
      ;; and broadcast it to remote peers.
      (define (integrate-local-message-and-send-remotely a-msg)
        (queue-callback/in-command-mode
         (lambda ()
           ;; Locally integrate the message.
           (consume-msg! woot-state a-msg)
           ;; And send it remotely.
           (client:client-send-message network-mailbox-client (msg->string a-msg)))))
      
      
      ;; integrate-remote-message: msg -> void
      ;; Send a message off to woot the woot state.
      (define (integrate-remote-message a-msg)
        (queue-callback/in-command-mode
         (lambda ()
           (cond [(msg-origin-remote? a-msg)
                  (let ([ops (consume-msg! woot-state a-msg)])
                    (dynamic-wind (lambda ()
                                    (begin-edit-sequence))
                                  (lambda ()
                                    (for-each apply-remote-operation ops))
                                  (lambda ()
                                    (end-edit-sequence))))]
                 [else
                  ;; Avoid feedback: don't integrate messages that we ourselves
                  ;; generated.
                  (void)]))))
      
      
      ;; apply-remote-operation: op -> void
      ;; Apply the effects of the remote operation onto the local buffer state.
      (define (apply-remote-operation an-op)
        (match an-op
          [(struct op:no-op (msg))
           (void)]
          
          [(struct op:insert-after (msg dstx after-id))
           (let ([visible-woot-id
                  (visible-before-or-at woot-state after-id)])
             (with-remote-operation-active
              (lambda ()
                (interpret!
                 (make-Insert-Dstx-After dstx
                                         (dstx-local-id
                                          (woot-id->local-dstx visible-woot-id)))))))]
          
          [(struct op:delete (msg id))
           (with-remote-operation-active
            (lambda ()
              (interpret!
               (make-Delete-Dstx
                (dstx-local-id (woot-id->local-dstx id))))))]))
      
      
      ;; msg-origin-remote?: msg -> boolean
      ;; Returns true if the operation was generated off-site remotely.
      (define (msg-origin-remote? a-msg)
        (not (string=? (get-host-id)
                       (msg-host-id a-msg))))
      
      
      
      
      ;; on-woot-structured-insert: dstx woot-id woot-id -> void
      ;; Broadcast a structured insert.
      (define/augment (on-woot-structured-insert a-dstx after-woot-id before-woot-id)
        (when network-mailbox-client
          (let ([a-msg (make-msg:insert (get-host-id) a-dstx after-woot-id before-woot-id)])
            (integrate-local-message-and-send-remotely a-msg)))
        (inner (void) on-woot-structured-insert a-dstx after-woot-id before-woot-id))
      
      
      
      ;; on-woot-structured-insert: dstx woot-id woot-id -> void
      ;; Broadcast a structured delete.
      (define/augment (on-woot-structured-delete woot-id)
        (when network-mailbox-client
          (let ([a-msg (make-msg:delete (get-host-id) woot-id)])
            (integrate-local-message-and-send-remotely a-msg)))
        (inner (void) on-woot-structured-delete woot-id))
      
      (initialize)))
  
  
  
  
  
  ;; A little widget that shows when we're connected.
  (define network-notifier-widget%
    (class object%
      (init-field parent
                  woot-state
                  mailbox-client)
      
      (define h-panel #f)
      (define msg-panel #f)
      
      
      (define (initialize)
        (super-new)
        (set! h-panel (new horizontal-panel% [parent parent]))
        (refresh-message-panel!))
      
      
      ;; refresh-message-panel!: subarea% -> void
      ;; Refreshes the message panel to contain information about the state of the connection
      ;; to the woot server.  Also provides buttons to stop the mailbox polling
      ;; and resume
      (define/public (refresh-message-panel!)
        (when msg-panel
          (send h-panel delete-child msg-panel))
        
        (set! msg-panel (new horizontal-panel% [parent h-panel]))
        (new immutable-text-field%
             [parent msg-panel]
             [label "Connected to "]
             [init-value (client:client-url mailbox-client)])
        (cond
          [(client:client-polling? mailbox-client)
           (new button%
                [label "Pause syncing"]
                [parent msg-panel]
                [callback (lambda (btn evt)
                            (client:client-pause-polling mailbox-client)
                            (refresh-message-panel!))])]
          [else
           (new button%
                [label "Resume syncing"]
                [parent msg-panel]
                [callback (lambda (btn evt)
                            (client:client-start-polling mailbox-client)
                            (refresh-message-panel!))])])
        (void))
      
      
      ;; detach!: -> void
      ;; Remove the notifier.
      (define/public (detach!)
        (send parent delete-child h-panel)
        (set! h-panel #f)
        (set! msg-panel #f))
      
      
      (initialize)))
  
  
  (define immutable-text-field%
    (class text-field%
      (inherit get-editor)
      (super-new)
      (send (get-editor) lock #t)))
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
  ;; woot-frame-mixin: add the menu options for hosting and joining sessions.
  (define (woot-frame-mixin super%)
    (class super%
      (inherit get-diva-menu get-definitions-text)
      
      (define host-menu-item #f)
      (define join-menu-item #f)
      
      (define host-label-string "Host shared session...")
      (define stop-hosting-label-string "Stop hosting session...")
      
      
      
      (define (initialize)
        (super-new)
        (set! host-menu-item
              (new menu-item%
                   [label host-label-string]
                   [parent (get-diva-menu)]
                   [callback (lambda (menu-item control-event)
                               (cond
                                 [(string=? (send menu-item get-label)
                                            host-label-string)
                                  (send (get-definitions-text) host-session)
                                  (send menu-item set-label stop-hosting-label-string)]
                                 [else
                                  (send (get-definitions-text) close-hosting-session)
                                  (send menu-item set-label host-label-string)]))]))
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
    (hosting-url default-port-number))
  
  
  ;; hosting-url number string: -> string
  ;; Returns a string representing the connection url.
  (define (hosting-url port)
    (format "http://~a:~a/" (self-ip:self-ip-address) port))
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; dstx-from-unstructured-editing?: dstx -> boolean
  ;; Returns true if we're certain that the dstx came from intermediate insert-mode.
  (define (dstx-from-unstructured-editing? a-dstx)
    (dstx-property-ref a-dstx 'from-unstructured-editing (lambda ()
                                                           #f)))
  
  )