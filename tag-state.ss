(module tag-state mzscheme
  ;; stateful module holding singleton instance of a tag library.
  (require "tag-reader.ss"
           (lib "contract.ss"))
  
  ;; The current tag library.  By default, we're just reading the one
  ;; associated to divascheme for testing.  Not a parameter: we want all threads
  ;; to share this?  (or is doing this with a parameter a better approach?)
  (define current-tag-library 
    (let ([path (build-path (current-directory) "STAGS-LIBRARY")])
      (cond
        [(file-exists? path) (open-tag-library path)]
        [else empty-tag-library])))
  
  (provide/contract (set-current-tag-library! (tag-library? . -> . any)))
  (define (set-current-tag-library! library)
    (set! current-tag-library library))

  
  (provide/contract (get-current-tag-library (-> tag-library?)))
  (define (get-current-tag-library)
    current-tag-library))