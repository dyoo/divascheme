(module simple-profile mzscheme
  ;; Provides a form "prof" that tells us memory usage and computation time
  
  (require (lib "list.ss")
           (lib "etc.ss"))
  (provide prof)
  
  (define (collect-garbage/time)
    (local ((define t1 (current-inexact-milliseconds)))
      (collect-garbage)
      (- (current-inexact-milliseconds) t1)))
  
  
  (define print-mem-labels '())
  (define print-mem-times '())
  (provide print-mem)
  
  (define (print-mem label thunk)
    (set! print-mem-labels (cons label print-mem-labels))
    (let* ([a (current-memory-use)]
           [_1 (collect-garbage/time)]
           [b (current-memory-use)])
      (set! print-mem-times (map (lambda (t) (+ t _1)) print-mem-times))
      (set! print-mem-times (cons (current-inexact-milliseconds) print-mem-times))
      (let* ([result (call-with-values thunk (lambda args args))]
             [t1 (first print-mem-times)]
             [t2 (current-inexact-milliseconds)]
             [c (current-memory-use)]
             [_2 (collect-garbage/time)]
             [d (current-memory-use)])
        (printf "PM ~a ms | ~a: GC pre ~a | GC post ~a~n"
                (- t2 t1)
                print-mem-labels
                (round (/ (- a b) 1000))
                (round (/ (- c d) 1000)))
        (set! print-mem-labels (rest print-mem-labels))
        (set! print-mem-times (rest print-mem-times))
        (set! print-mem-times (map (lambda (t) (+ t _2)) print-mem-times))
        (apply values result))))
  
  
  (define-syntax (prof stx)
    (syntax-case stx ()
      [(_ label e ...)
       (syntax/loc stx
         (print-mem label (lambda () e ...)))])))