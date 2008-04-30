(module woot-struct mzscheme
  (require (lib "contract.ss")
           (lib "serialize.ss"))
  
  ;; woot-id represents a globally unique id.
  (define-serializable-struct woot-id (logic-id host-id) #f)
  (provide/contract [struct woot-id ([logic-id natural-number/c]
                                     [host-id string?])]))