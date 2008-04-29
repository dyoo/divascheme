(module woot-struct mzscheme
  (require (lib "contract.ss"))
  
  ;; woot-id represents a globally unique id.
  (define woot-id/c (list/c number? string?))
  
  (define (make-woot-id logic-id host-string)
    (list logic-id host-string))
  
  (provide/contract [woot-id/c flat-contract?]
                    [make-woot-id (number? string? . -> . woot-id/c)]))