(module dot-processing mzscheme
  (require "semi-read-syntax/semi-read-syntax.ss"
           (only "utilities.ss" print-mem* make-voice-exn print-time*)
           (lib "contract.ss")
           (lib "etc.ss")
           "rope.ss")
  
  (provide/contract [parse-syntax (input-port? . -> . (listof syntax?))]
                    [rope-parse-syntax (rope? . -> . (listof syntax?))])
  
  ;; parse-syntax: input-port -> (listof syntax)
  (define (parse-syntax ip)
    (print-mem*
     'parse-syntax/dot
     (with-handlers
         ([(lambda args #t)
           (lambda (exn)
             (raise
              (make-voice-exn
               "The parenthesis of the definitions text are not correctly balanced: ~a"
               (exn-message exn))))])
       (port-count-lines! ip)
       (semi-read-syntax-list 'voice:action:get-syntax ip))))
  
  
  ;; rope-parse-syntax: rope -> (listof syntax)
  ;; Parses a rope.
  ;; FIXME: this function can be potentially expensive. We may need
  ;; to do something smarter to make this work fast on larger files.
  (define (rope-parse-syntax a-rope)
    (print-time*
     'rope-parse-syntax
     (parse-syntax (open-input-rope a-rope)))))

