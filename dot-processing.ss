(module dot-processing mzscheme
  
  (require "semi-read-syntax/semi-read-syntax.ss"
           (only "utilities.ss" print-mem* make-voice-exn))
  
  (provide parse-syntax/dot)
  
  ;; parse-syntax/dot: string -> (listof syntax)
  (define (parse-syntax/dot text)
    (print-mem*
     'parse-syntax/dot
     (with-handlers
         ([(lambda args #t)
           (lambda (exn)
             (raise (make-voice-exn "The parenthesis of the definitions text are not correctly balanced.")))])
       (let ([ip (open-input-string text)])
         (port-count-lines! ip)
         (semi-read-syntax-list 'voice:action:get-syntax ip))))))

