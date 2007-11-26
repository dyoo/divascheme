(module install-launchers mzscheme
  (require (lib "launcher.ss" "launcher")
           (lib "etc.ss")
           (lib "contract.ss")
           (lib "runtime-path.ss"))

  
  (provide/contract [install-launchers
                     (case->
                      (path-string? . -> . any)
                      (-> any))])
  

  (define-runtime-path generate-stags-path "generate-stags.ss")

  
  ;; default-install-directory: -> path 
  ;; Returns the default path where we will install our launchers, plt/bin 
  ;; (copy-and-pasted from setup/configtab.ss) 
  (define (default-install-directory)
    (let ([exec (path->complete-path
                 (find-executable-path (find-system-path 'exec-file))
                 (find-system-path 'orig-dir))])
      (let-values ([(base name dir?) (split-path exec)])
        base)))
  
  ;; install-launchers: -> void
  ;; install-launchers: path-string -> void
  ;; Installs our launchers into either the given directory, or the default one. 
  (define install-launchers
    (case-lambda
      [(dest-dir)
       (local
           ((define target-bin (build-path dest-dir "generate-stags")))
         (make-mzscheme-launcher (list "-qmvt-" (path->string generate-stags-path))
                                 target-bin))]
      [()
       (install-launchers (default-install-directory))])))