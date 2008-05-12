(module changable-message mzscheme
  (require (lib "class.ss")
           (lib "mred.ss" "mred"))
  
  (provide changable-message%)
  
  
  ;; changable-message%: defines a label that knows how to resize itself.  Since message% doesn't
  ;; change its geometry when we change the label, we make an auxillary class to do this
  ;; geometry management for us.
  (define changable-message%
    (class panel%
      (init [label #f])
      (inherit change-children)
      
      (define (initialize)
        (super-new)
        (when label
          (redraw-label label)))
      
      
      ;; redraw-label: string -> void
      ;; Remove all children, and add a single message child.
      (define (redraw-label label-text)
        (change-children (lambda (children) '()))
        (new message%
             [parent this]
             [label label-text])
        (void))
      
      
      (define/override (set-label a-label)
        (super set-label a-label)
        (when a-label
          (redraw-label a-label)))
      
      
      (initialize))))