(module language mzscheme
  (require (lib "list.ss")
           (lib "class.ss")
           (lib "etc.ss")
           (lib "framework.ss" "framework")
           (lib "contract.ss")
           "utilities.ss")
  
  (provide initialize-get-language)
  
  
  
  
  
  (define get-settings-preferences-symbol #f)
  (define language-settings-language #f)
  (define capability-registered? #f)
  ;; initialize-get-language: (-> drscheme:language-configuration:language-settings?)
  ;;                          drscheme:language-configuration:language-settings?
  ;;                          -> (is-a?/c drscheme:language:language<%>)
  ;; FIXME: do this proper with unit integration once we've abandoned compatibility
  ;; with 301. 
  (define (initialize-get-language
           drscheme:language-configuration:get-settings-preferences-symbol
           drscheme:language-configuration:language-settings-language
           drscheme:language:capability-registered?)
    (set! get-settings-preferences-symbol
          drscheme:language-configuration:get-settings-preferences-symbol)
    (set! language-settings-language
          drscheme:language-configuration:language-settings-language)
    (set! capability-registered?
          drscheme:language:capability-registered?))
  
  
  ;; get-language: -> language%
  ;; Returns the language. 
  (define (get-language)
    (let* ([language-settings
            (preferences:get (get-settings-preferences-symbol))]
           [language
            (language-settings-language language-settings)])
      language))
  
  
  ;; get-language-name: -> string 
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
  (define (get-language-name)
    (local ((define language (get-language))
            (define name1 (send language get-language-name))
            (define name2 (first (last-pair (send language get-language-position)))))
      name2))
  
  ;; get-language-autocompletes: (listof string)
  ;; Returns a list of strings that can autocomplete 
  (define (get-language-autocompletes)
    (define (return-defaults)
      (map symbol->string (get-mzscheme-mapped-symbols)))
    (cond
      [(capability-registered? 'drscheme:autocomplete-words)
       (with-handlers ([exn:fail? (lambda (exn) (return-defaults))])
         (send (get-language) capability-value 'drscheme:autocomplete-words))]
      [else
       (return-defaults)]))
  
  
  (provide/contract [get-language-name (-> string?)]
                    [get-language-autocompletes (-> (listof string?))]))