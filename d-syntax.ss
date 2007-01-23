(module d-syntax mzscheme
  
  ;; A more suitable notion of syntax for DivaScheme that captures
  ;; what we need for semi-structured editing.
  
  (provide (all-defined))
  
  (define-struct d-syntax (text line column position span) #f)
  (define-struct (d-list d-syntax) (elts) #f)
  (define-struct (d-atom d-syntax) () #f)
  (define-struct (d-quoted d-syntax) (elt) #f))