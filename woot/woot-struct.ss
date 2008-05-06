(module woot-struct mzscheme
  (require "../dsyntax/dsyntax.ss"
           (lib "contract.ss")
           (lib "plt-match.ss")
           (lib "serialize.ss"))
  
  ;; woot-id represents a globally unique id.
  (define-serializable-struct woot-id (logic-id host-id) #f)
  
  
  
  
  
  ;; Module for passing along woot operations (insert, delete) as messages.
  
  (define-serializable-struct msg (host-id) #f)
  (define-serializable-struct (msg:insert msg) (dstx after-id before-id) #f)
  (define-serializable-struct (msg:delete msg) (id) #f)
  
  ;; msg->string: msg -> string
  ;; Convert a msg into a form suitable for transport.
  (define (msg->string a-msg)
    (match a-msg
      [(struct msg:insert (host-id dstx after-id before-id))
       (format "~s" (serialize a-msg))]
      [(struct msg:delete (host-id id))
       (format "~s" (serialize a-msg))]))
  
  
  
  ;; operations are produced after we process messages.
  (define-struct op (msg) #f)
  (define-struct (op:insert-after op) (dstx id) #f)
  (define-struct (op:delete op) (id) #f)
  
  
  ;; Rehydrate a string back into a message.
  (define (string->msg a-msg)
    (let ([ip (open-input-string a-msg)])
      (deserialize (read ip))))
  
  
  
  
  (provide/contract
   
   [struct woot-id ([logic-id natural-number/c]
                    [host-id string?])]
   
   [struct msg ([host-id string?])]
   [struct (msg:insert msg) ([host-id string?]
                             [dstx dstx?]
                             [after-id woot-id?]
                             [before-id
                              (or/c false/c woot-id?)])]
   [struct (msg:delete msg) ([host-id string?]
                             [id woot-id?])]
   
   [msg->string (msg? . -> . string?)]
   [string->msg (string? . -> . msg?)]
   
   
   [struct op ([msg msg?])]
   [struct (op:insert-after op) ([msg msg?]
                                 [dstx dstx?]
                                 [id woot-id?])]
   [struct (op:delete op) ([msg msg?]
                           [id woot-id?])])
  
  
  )