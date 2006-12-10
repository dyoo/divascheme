(module info (lib "infotab.ss" "setup")
  (define name "DivaScheme")
  (define blurb '("semi-structured programming editing for DrScheme"))
  (define categories '(devtools))
  (define version "2.1")
  
  (define doc.txt "doc.txt")
  
  ;; This is the file which is loaded on the module start.
  (define tools '(("tool.ss")))
  
  ;; The icon of the project.
  (define tool-names (list "DivaScheme"))
  
  ;; the url of the plugin.
  (define tool-urls (list #f)))
