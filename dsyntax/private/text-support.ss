(module text-support mzscheme
  (require (lib "etc.ss")
           (lib "contract.ss")
           (lib "mred.ss" "mred"))
  
  (provide/contract [call-with-text-input-port
                     ((is-a?/c text%)
                      (input-port? . -> . any)
                      . -> .
                      any)])
  
  (define (call-with-text-input-port text f)
    (local ((define (boxing-filter snip)
              (box snip))
            
            (define ip
              (open-input-text-editor text 0 'end boxing-filter
                                      text #t)))
      (with-handlers ((exn:fail? (lambda (exn)
                                   (close-input-port ip)
                                   (raise exn))))
        (local ((define result (f ip)))
          (close-input-port ip)
          result)))))