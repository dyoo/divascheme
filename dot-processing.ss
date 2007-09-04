(module dot-processing mzscheme
  (require "semi-read-syntax/semi-read-syntax.ss"
           (only "utilities.ss" print-mem* make-voice-exn print-time*)
           (lib "contract.ss")
           (lib "etc.ss")
           "rope.ss")
  
  (provide/contract [rope-parse-syntax (rope? . -> . (listof syntax?))])
  
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
  
  
  (define (parse-syntax/no-specials ip)
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
       (semi-read-syntax-list/no-specials 'voice:action:get-syntax ip))))
  
  
  ;; rope-parse-syntax: rope -> (listof syntax)
  ;; Parses a rope.
  ;; FIXME: this function can be potentially expensive. We may need
  ;; to do something smarter to make this work fast on larger files.
  (define (rope-parse-syntax a-rope)
    (cond
      [(rope-has-special? a-rope)
       (parse-syntax (open-input-rope a-rope))]
      [else
       (parse-syntax/no-specials (open-input-rope a-rope))])))

