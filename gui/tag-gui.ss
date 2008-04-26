(module tag-gui mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss")
           (lib "list.ss")
           (lib "list.ss")
           "../stags-lib.ss"
           "../tag-reader.ss"
           "../tag-state.ss")
  
  
  (provide tag-gui-unit:frame-mixin
           generate-navigation-tags-dialog)
  
  
  (define (load-tag-library filename)
    (set-current-tag-library! (open-tag-library filename)))
  
  
  ;; generate-navigation-tags-dialog: -> void
  ;; Brings up a dialog window to select a project directory.  If a directory
  ;; is selected, creates an STAGS in that directory whose contents index
  ;; all the Scheme files within that directory.
  ;;
  ;; This might take a while, so we allow this to run in a thread while we wait.
  (define (generate-navigation-tags-dialog)
    (define title "Generate Navigation Tags")
    (define instructions-msg
      "Select project directory")
    
    (let ([dir (get-directory instructions-msg #f #f '(enter-packages))])
      (when (and dir (directory-exists? dir))
        (let [(sema (make-semaphore 0))]
          (thread
           (lambda ()
             (message-box
              title
              "Generating navigation tags; this may take a while.")
             (semaphore-wait sema)
             (message-box
              title
              "Navigation tags have been generated and loaded.")))
          (thread
           (lambda ()
             (dynamic-wind
              (lambda () (void))
              (lambda ()
                ;; TODO: better error trapping?
                (generate-stags-file/project dir "STAGS")
                (load-tag-library (build-path dir "STAGS")))
              (lambda ()
                (semaphore-post sema)))))))))
  
  
  ;; Adds the support necessary to load new tag files.
  ;; This adds a new menu-item: "Load Navigation Tags".
  (define (tag-gui-unit:frame-mixin super%)
    (class super%
      (inherit get-diva-menu)
      (define load-menu-item #f)
      
      (define (initialize)
        (super-new)
        (set! load-menu-item
              (new menu-item%
                   [label "Load Navigation Tags..."]
                   [parent (get-diva-menu)]
                   [callback (lambda (menu-item control-event)
                               (ask-and-load-tags this))])))
      
      
      (define (ask-and-load-tags parent)
        (let ([filename
               (get-file "Choose a STAGS file.\nSTAGS files contain navigation hints,\nand are created by plt/bin/stags."
                         parent
                         #f
                         #f
                         #f
                         empty
                         '(("STAGS" "STAGS")))])
          (when filename
            (load-tag-library filename))))
      
      
      (initialize))))




