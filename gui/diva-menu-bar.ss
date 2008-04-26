(module diva-menu-bar mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss"))
  
  (provide diva-menu-bar-mixin)
  
  ;; This adds another menu bar specifically for DivaScheme stuff
  
  
  ;; diva-menu-bar-mixin: frame:basic<%> -> frame:basic<%>
  (define (diva-menu-bar-mixin super%)
    (class super%
      (inherit get-menu-bar)
      
      ;; menu will be our personal menu, containing options
      ;; for disabling or enabling DivaScheme.
      (define diva-menu #f)
      
      
      
      (define (initialize)
        (super-new)
        (set! diva-menu (new menu%
                             [label "DivaScheme"]
                             [parent (get-menu-bar)])))
      
      (initialize))))