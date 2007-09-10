(module info (lib "infotab.ss" "setup")
  
  (define name "DivaScheme")
  (define blurb '("semi-structured programming editing for DrScheme"))
  (define homepage "http://www.cs.brown.edu/research/plt/software/divascheme/")
  (define version "2.3")
  (define doc.txt "doc.txt")
  
  (define release-notes '((p "Recent changes in 2.3:")
                          (ul
                           (li "Comments can now be edited like strings.")
                           (li "Graphical snips now supported.")
                           (li "Switching tabs while in Insert mode properly works now.")
                           (li "Up and down are bound to move around in Insert mode."))))
  
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
  (define compile-omit-files (list "tests/actions-test.ss"
                                   "tests/interpreter-test.ss"
                                   "tests/structures-test.ss"
                                   "tests/templates-test.ss"
                                   "tests/traversal-test.ss"
                                   "tests/utilities-test.ss"))
  
  ;; minimal version of DrScheme necessary will be 360.
  (define required-core-version "360")
  
  ;; The following are commented out because they don't work well
  ;; for people who don't have permission to write to the default bin
  ;; directory.  So instead, we have a separate 'install-launchers.ss'
  ;; module that should do this work. 
  #;(define mzscheme-launcher-names '("generate-stags"))
  #;(define mzscheme-launcher-libraries '("generate-stags.ss"))
  )
