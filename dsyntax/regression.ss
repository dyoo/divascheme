(module regression mzscheme
  ;; A regression test across all of the source code I can find for
  ;; the plt-scheme parser.
  
  (require (lib "file.ss")
           (lib "etc.ss")
           (lib "mred.ss" "mred")
           "parse-plt-scheme.ss")
  
  (define (map-append f l)
    (apply append (map f l)))
  
  
  (define (exclude? path)
    (call-with-input-file* path
      (lambda (ip)
        (with-handlers ((exn:fail? (lambda (exn) #t)))
          (let loop ([x (read ip)])
            (cond [(eof-object? x) #f]
                  [else
                   (loop (read ip))]))))))
  
  
  (define (scheme-module? path)
    (member (filename-extension path) '(#"ss" #"scm")))
  
  
  (define (files)
    (local [(define root-dirs
              (list (normalize-path
                     (build-path (path-only (find-system-path 'exec-file))
                                 (find-system-path 'collects-dir)))))
            (define all-files
              (map-append (lambda (root-dir) (find-files scheme-module? root-dir))
                          root-dirs))]
      all-files))
  
  
  (define (test-faithful-parse path)
    (with-handlers
        ((exn:fail? (lambda (exn)
                      (cond
                        [(exclude? path)
                         (void)]
                        [else
                         (printf "parsing ~a~n" path)
                         (printf "\tERROR: ~a~n" (exn-message exn))
                         #;(raise exn)]))))
      (local
          ((define ip (open-input-graphical-file path))
           (define parsed-dstx (parse-port ip)))
        ;; TODO: see if writing it back out gets us similar input.
        
        (close-input-port ip))))
  
  
  (time (for-each test-faithful-parse (files))))