(module woot-gui-mixins mzscheme
  
  (require (lib "class.ss")
           (lib "plt-match.ss")
           (lib "mred.ss" "mred")
           (lib "async-channel.ss")
           "../dsyntax/dsyntax.ss"
           "../structures.ss"
           "woot-struct.ss"
           "mock-woot.ss"
           "utilities.ss"
           "../gui/changable-message.ss"
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
                       (self-ip:self-ip-address)
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
                  (equal? (dstx-woot-id a-dstx)
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
               woot-id->local-dstx
               with-remote-operation-active
               queue-callback/in-command-mode
               begin-edit-sequence
               end-edit-sequence)
      
      
      (define woot-custodian (make-custodian))
      (define network-mailbox-client #f)
      (define mailbox-client-thread #f)
      (define woot-state #f)
      (define notifier-widget #f)
      
      
      (define (initialize)
        (super-new))
      
      
      ;; host-session: -> void
      ;; Brings up a dialog window to host a session.  Shows our ip, and
      ;; some message on how to get others to join.
      (define/public (host-session)
        (let ([url (start-local-server)])
          (start-network-client url)
          (integrate-initial-state)
          (message-box
           "Host session started"
           (format "Session started.\nOther hosts may join by using the session url: ~s"
                   url))
          (set! notifier-widget
                (new network-notifier-widget%
                     [parent (send (get-top-level-window) get-diva-panel)]
                     [woot-state woot-state]
                     [mailbox-client network-mailbox-client]))))
      
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
           (format "Connected to session."))))
      
      
      ;; start-local-server: string -> string
      ;; Starts up the local server.
      (define (start-local-server)
        (parameterize ([current-custodian woot-custodian])
          (server:start-server default-port-number)))
      
      
      ;; start-network-client: string -> void
      ;; Starts up the client part of the server.
      (define (start-network-client url)
        (parameterize ([current-custodian woot-custodian])
          (set! network-mailbox-client (client:new-client url))
          (set! woot-state
                (new-mock-woot (send (get-dstx-cursor) get-functional-cursor)))
          (start-process-client-message-loop!)))
      
      
      
      ;; start-process-client-message-loop!: -> void
      ;; Starts up a thread that reads messages from the network and passes them off to
      ;; woot.
      ;; When we get messages from the client, queue-callback a handler that
      ;; kickstarts woot.
      (define (start-process-client-message-loop!)
        (set! mailbox-client-thread
              (thread
               (lambda ()
                 (let loop ()
                   (let ([msg (string->msg
                               (async-channel-get
                                (client:client-mailbox network-mailbox-client)))])
                     (integrate-message-into-woot msg)
                     (loop)))))))
      
      
      (define (integrate-initial-state)
        ;; fixme: go through the text and generate insert-between messages
        (let ([a-cursor (get-dstx-cursor)])
          (send a-cursor focus-toplevel!)
          ;; fixme!
          ;; not ready yet: do this tomorrow!
          #;(let loop ()
              (integrate-message-into-woot
               (make-msg:insert (get-host-id) (cursor-dstx) after-woot-id before-woot-id))
              ))
        (void))
      
      
      ;; integrate a message-to-woot: msg -> void
      ;; Send a message off to woot.  This is running under the context of a queue-callback
      ;; in command mode.
      (define (integrate-message-into-woot a-msg)
        (queue-callback/in-command-mode
         (lambda ()
           (let ([ops (consume-msg! woot-state a-msg)])
             (dynamic-wind (lambda ()
                             (begin-edit-sequence))
                           (lambda ()
                             (for-each maybe-apply-remote-operation ops))
                           (lambda ()
                             (end-edit-sequence)))))))
      
      
      ;; maybe-apply-op: op -> void
      ;; Maybe interpret the operation.
      (define (maybe-apply-remote-operation an-op)
        (match an-op
          [(struct op:insert-after (msg dstx after-id))
           (cond
             [(msg-origin-remote? msg)
              (let ([visible-woot-id
                     (visible-before-or-at woot-state after-id)])
                (with-remote-operation-active
                 (lambda ()
                   (interpret!
                    (make-Insert-Dstx-After dstx
                                            (dstx-local-id
                                             (woot-id->local-dstx visible-woot-id)))))))]
             [else
              (void)])]
          [(struct op:delete (msg id))
           (cond
             [(msg-origin-remote? msg)
              (with-remote-operation-active
               (lambda ()
                 (interpret!
                  (make-Delete-Dstx
                   (dstx-local-id (woot-id->local-dstx id))))))]
             [else
              (void)])]))
      
      
      ;; msg-origin-remote?: msg -> boolean
      ;; Returns true if the operation was generated off-site remotely.
      (define (msg-origin-remote? a-msg)
        (not (string=? (get-host-id)
                       (msg-host-id a-msg))))
      
      
      
      ;; on-woot-structured-insert: dstx woot-id woot-id -> void
      ;; Broadcast a structured insert.
      (define/augment (on-woot-structured-insert a-dstx after-woot-id before-woot-id)
        (when network-mailbox-client
          (client:client-send-message
           network-mailbox-client
           (msg->string
            (make-msg:insert (get-host-id) a-dstx after-woot-id before-woot-id))))
        (inner (void) on-woot-structured-insert a-dstx after-woot-id before-woot-id))
      
      
      
      ;; on-woot-structured-insert: dstx woot-id woot-id -> void
      ;; Broadcast a structured delete.
      (define/augment (on-woot-structured-delete woot-id)
        (when network-mailbox-client
          (client:client-send-message
           network-mailbox-client
           (msg->string
            (make-msg:delete (get-host-id) woot-id))))
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
        (set! msg-panel (make-message-panel h-panel)))
      
      (define (make-message-panel parent)
        (define message-panel (new horizontal-panel% [parent parent]))
        (new immutable-text-field%
             [parent message-panel]
             [label "Connected to "]
             [init-value (client:client-url mailbox-client)])
        message-panel)
      
      
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