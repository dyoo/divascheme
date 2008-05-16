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
      
      (define current-msg #f)
      
      (define (initialize)
        (super-new)
        (when label
          (redraw-label label)))
      
      
      ;; redraw-label: string -> void
      ;; Remove all children, and add a single message child.
      (define (redraw-label label-text)
        (cond
          [(and current-msg
                (string=? (send current-msg get-label)
                          label-text))
           ;; avoid redrawing if the label text is identical.
           (void)]
          [else
           (change-children (lambda (children) '()))
           (set! current-msg (new message%
                                  [parent this]
                                  [label label-text]))]))
      
      
      (define/override (set-label a-label)
        (super set-label a-label)
        (redraw-label a-label))
      
      
      (initialize))))