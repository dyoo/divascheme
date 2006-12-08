(module language mzscheme
  (require (lib "list.ss")
           (lib "class.ss")
           (lib "framework.ss" "framework"))
  
  (provide initialize-get-language
           get-language)
  
  ;; get-language: -> string
  ;; Returns the name of the current language.  Expected to be one of the following:
  ;; 
  ;;     (string-constant beginner-student)
  ;;     (string-constant beginning-student/abbrev)
  ;;     (string-constant intermediate-student)
  ;;     (string-constant intermediate-student/lambda)
  ;;     (string-constant advanced-student)
  ;;
  ;; or something else (like module language).  Used for conditional templates based
  ;; on what language level is currently used.
  (define get-language #f)
  
  
  ;; initialize-get-language: (-> drscheme:language-configuration:language-settings?)
  ;;                          drscheme:language-configuration:language-settings?
  ;;                          -> (is-a?/c drscheme:language:language<%>)
  (define (initialize-get-language
           drscheme:language-configuration:get-settings-preferences-symbol
           drscheme:language-configuration:language-settings-language)
    (set! get-language (make-get-language
                        drscheme:language-configuration:get-settings-preferences-symbol
                        drscheme:language-configuration:language-settings-language)))
  
  (define ((make-get-language
            drscheme:language-configuration:get-settings-preferences-symbol
            drscheme:language-configuration:language-settings-language))
    (let* ([language-settings
            (preferences:get
             (drscheme:language-configuration:get-settings-preferences-symbol))]
           [language
            (drscheme:language-configuration:language-settings-language language-settings)]
           [name1 (send language get-language-name)]
           [name2 (first (last-pair (send language get-language-position)))])
      name2)))