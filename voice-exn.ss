(module voice-exn mzscheme
  (require (lib "plt-match.ss"))
  
  ;; Some of the functions here are redundant and are an
  ;; artifact of these exceptions not originally being
  ;; substructures of exn:fail.
  
  
  ;; Functions on exception.
  (provide (rename -make-voice-exn make-voice-exn)
           voice-exn?
           voice-exn-message)
  
  (define-struct (voice-exn exn:fail) ())
  
  (define (-make-voice-exn text)
    (make-voice-exn text (current-continuation-marks))) 
  
  (define (voice-exn-message exn)
    (exn-message exn))
  
  
  
  (define-struct (voice-exn/world exn:fail) (world))
  
  (define (-make-voice-exn/world text world)
    (make-voice-exn/world text (current-continuation-marks) world))
  
  (provide (rename -make-voice-exn/world make-voice-exn/world)
           voice-exn/world?
           voice-exn/world-message
           voice-exn/world-world)
  
  (define (voice-exn/world-message exn)
    (exn-message exn)))