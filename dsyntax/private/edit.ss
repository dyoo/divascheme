(module edit mzscheme
  (require (lib "contract.ss")
           "struct.ss")
  
  
  (define (cursor-insert-before a-cursor a-dstx)
    ;; fixme!
    (void))
  
  (define (cursor-insert-after a-cursor a-dstx)
    ;; fixme!
    (void))
  
  (define (cursor-delete a-cursor)
    ;; fixme!
    (void))
  
  (define (cursor-set-property a-cursor a-symbol a-value)
    ;; fixme!
    (void))
  
  
  (provide/contract
   [cursor-insert-before (cursor? dstx? . -> . cursor?)]
   [cursor-insert-after (cursor? dstx? . -> . cursor?)]
   [cursor-delete (cursor? . -> . cursor?)]
   [cursor-set-property (cursor? symbol? any/c . -> . cursor?)]))