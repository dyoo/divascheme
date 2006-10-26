(module marker mzscheme
  (require (lib "class.ss")
           (lib "list.ss")
           (lib "mred.ss" "mred")) 
  
  ;; Implementation of emacs marks as an editor text% mixin.
  ;; Markers provide a position into the text%.
  ;; Inserts and deletes on text will automatically adjust the mark
  ;; so that it points at the same characters.
  
  (define-struct marker (pos) #f)
  (provide (struct marker (pos)))
  
  
  
  (provide marker-mixin)
  ;; marker-mixin
  ;; Mixes in marker behavior into a text% editor class.
  (define marker-mixin
    (let ([text-interface% (class->interface text%)])
      (mixin (text-interface%) (text-interface%)
        (super-new)
        
        (define boxed-markers empty)
        
        (define/public (add-marker! pos)
          (let ([new-marker (make-marker pos)])
            (set! boxed-markers (cons (make-weak-box new-marker) boxed-markers))
            new-marker))
        
        (define/augment (on-insert start len)
          (inner void on-insert start len)
          (set! boxed-markers (map/filter-weak
                       (lambda (m)
                         (adjust-for-insert! start len m)
                         m)
                       boxed-markers)))
        
        (define/augment (on-delete start len)
          (inner void on-delete start len)
          (set! boxed-markers (map/filter-weak
                       (lambda (m)
                         (adjust-for-delete! start len m)
                         m)
                       boxed-markers))))))
  
  
  (define (adjust-for-delete! start length mark)
    (cond
      [(< start (marker-pos mark) (+ start length)) ;; overlapping case
       (set-marker-pos! mark start)]
      [(< start (marker-pos mark))
       (set-marker-pos! mark (- (marker-pos mark) length))]))
  
  
  (define (adjust-for-insert! start length mark)
    (when (< start (marker-pos mark))
      (set-marker-pos! mark (+ (marker-pos mark) length))))
  
  
  ;; map/filter-weak: (listof (weak-box-of X)) -> (listof (weak-box-of X))
  ;; maps a function across a list of weak boxes.   Any boxes that turn
  ;; into dead ones will be filtered out.
  (define (map/filter-weak f l)
    (reverse
     (foldl
      (lambda (x acc)
        (let ([val (weak-box-value x)])
          (if val
              (let ([result (f val)])
                (if (eq? result val)
                    (cons x acc)
                    (cons (make-weak-box result)
                          acc)))
              acc)))
      empty
      l)))
    
  
  ;; quick-and-dirty tests
  (define (tests)
    (define (check label x y)
      (unless (equal? x y)
        (error label "~s not equal to expected value ~s" x y))
      (printf "~a ok~n" label))
    
    (define (make-text)
      (new (marker-mixin text%)))
    
    (define (test1)
      (define text (make-text))
      (send text insert "ello" 0)
      (let [(mark1 (send text add-marker! 1))]
        (send text insert "h" 0)
        (check 'test1 (marker-pos mark1) 2)))
    
    (define (test2)
      (define text (make-text))
      (send text insert "hello" 0)
      (let ([mark1 (send text add-marker! 1)])
        (send text insert "world" 5)
        (check 'test2 (marker-pos mark1) 1)))
    
    (define (test3)
      (define t (make-text))
      (send t insert "hello")
      (let ([mark1 (send t add-marker! 5)])
        (send t delete 0 2)
        (check 'test3 (marker-pos mark1) 3)))
    
    (test1)
    (test2)
    (test3)))