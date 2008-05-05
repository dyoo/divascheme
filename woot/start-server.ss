(module start-server mzscheme
  (require (planet "version-case.ss" ("dyoo" "version-case.plt" 1 4))
           (lib "contract.ss"))
  
  ;; A little bit of compatibility code.
  (version-case
   [(version<= (version) "372")
    (require "start-server-372.ss")]
   [else
    (require "start-server-399.ss")])
  
  
  (provide/contract [start-server (number? . -> . string?)]))