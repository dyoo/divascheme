(module info (lib "infotab.ss" "setup")
  
  (define name "DivaScheme")
  (define blurb '("semi-structured programming editing for DrScheme"))
  (define homepage "http://www.cs.brown.edu/research/plt/software/divascheme/")
  (define version "2.3")
  (define doc.txt "doc.txt")
  
  (define release-notes '((p "Recent changes in 2.3:")
                          (ul
                           (li "Graphical snips are supported.")
                           (li "Comments can be edited without exiting DivaScheme.")
                           (li "Up and down can be used in Insert Mode.")
                           (li "Tab autocompletion has been improved."))
                          (p "For more details, see http://www.cs.brown.edu/research/plt/software/divascheme/")))
  
  (define primary-file "install.ss")
  (define categories '(devtools))
  ;; This is the file which is loaded on the module start.
  (define tools '(("tool.ss")))
  ;; The icon of the project.
  (define tool-names (list "DivaScheme"))
  ;; the url of the plugin.
  (define tool-urls (list #f))
  
  ;; Some of the tests we have are broken; don't try to
  ;; compile them.
  (define compile-omit-paths (list "tests"))

  
  ;; minimal version of DrScheme necessary will be 360.
  (define required-core-version "360")
  
  ;; The following are commented out because they don't work well
  ;; for people who don't have permission to write to the default bin
  ;; directory.  So instead, we have a separate 'install-launchers.ss'
  ;; module that should do this work. 
  #;(define mzscheme-launcher-names '("generate-stags"))
  #;(define mzscheme-launcher-libraries '("generate-stags.ss"))
  )
