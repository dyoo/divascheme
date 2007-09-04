(module dot-processing mzscheme
  (require "semi-read-syntax/semi-read-syntax.ss"
           (only "utilities.ss" print-mem* make-voice-exn print-time*)
           (lib "contract.ss"))
  
  (provide/contract [parse-syntax (input-port? . -> . (listof syntax?))])
  
  ;; parse-syntax/dot: input-port -> (listof syntax)
  (define (parse-syntax ip)
    (print-time*
     'parse-syntax
     (print-mem*
      'parse-syntax/dot
      (with-handlers
          ([(lambda args #t)
            (lambda (exn)
              (raise (make-voice-exn "The parenthesis of the definitions text are not correctly balanced.")))])
        (port-count-lines! ip)
        (semi-read-syntax-list 'voice:action:get-syntax ip))))))

