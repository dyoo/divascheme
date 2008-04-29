(module msg-structs mzscheme
  (require (lib "plt-match.ss")
           (lib "contract.ss")
           (lib "serialize.ss"))
  
  ;; Module for passing along woot operations (insert, delete) as messages.
  
  (define-serializable-struct msg (host-id) #f)
  (define-serializable-struct (msg:insert msg) (dstx before-id after-id) #f)
  (define-serializable-struct (msg:delete msg) (id) #f)
  
  ;; msg->string: msg -> string
  ;; Convert a msg into a form suitable for transport.
  (define (msg->string a-msg)
    (match a-msg
      [(struct msg:insert (host-id dstx before-id after-id))
       (format "~s" (serialize a-msg))]
      [(struct msg:delete (host-id id))
       (format "~s" (serialize a-msg))]))
  
  
  ;; Rehydrate a string back into a message.
  (define (string->msg a-msg)
    (let ([ip (open-input-string a-msg)])
      (deserialize (read ip)))))