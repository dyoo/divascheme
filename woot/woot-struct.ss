#lang scheme/base

(require "../dsyntax/dsyntax.ss"
         (lib "contract.ss")
         (lib "plt-match.ss"))

;; woot-id represents a globally unique id.
(define-struct woot-id (logic-id host-id) #:prefab)





;; Module for passing along woot operations (insert, delete) as messages.

(define-struct msg (host-id) #:prefab)
(define-struct (msg:insert msg) (dstx after-id before-id) #:prefab)
(define-struct (msg:delete msg) (id) #:prefab)
(define-struct (msg:move msg) (from-id after-id before-id new-id) #:prefab)

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

;; Tomb types are used to denote movement and deletion tombs
(define-struct tomb () #f)
(define-struct (tomb:d tomb) () #f)
(define-struct (tomb:m tomb) (id) #f)



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
 [struct (msg:move msg) ([host-id string?]
                         [from-id woot-id?]
                         [after-id woot-id?]
                         [before-id woot-id?]
                         [new-id woot-id?])]
 
 [msg->string (msg? . -> . string?)]
 [string->msg (string? . -> . msg?)]
 
 
 [struct op ([msg msg?])]
 [struct (op:insert-after op) ([msg msg?]
                               [dstx dstx?]
                               [id woot-id?])]
 [struct (op:delete op) ([msg msg?]
                         [id woot-id?])]
 [struct tomb ()]
 [struct (tomb:d tomb) ()]
 [struct (tomb:m tomb) ([id woot-id?])])
