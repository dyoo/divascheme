(module info (lib "infotab.ss" "setup")
  (define name "DivaScheme")
  (define blurb '("semi-structured programming editing for DrScheme"))
  (define homepage "http://www.cs.brown.edu/research/plt/software/divascheme/")
  (define version "2.2")
  (define doc.txt "doc.txt")
  
  (define primary-file "install.ss")
  (define categories '(devtools))
  ;; This is the file which is loaded on the module start.
  (define tools '(("tool.ss")))
  ;; The icon of the project.
  (define tool-names (list "DivaScheme"))
  ;; the url of the plugin.
  (define tool-urls (list #f))
  
  (define mzscheme-launcher-names '("generate-stags"))
  (define mzscheme-launcher-libraries '("generate-stags.ss"))
  )
