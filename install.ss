(module install mzscheme
  ;; Small little message for use with PLaneT installation.  The idea is to
  ;; provide people the following PLaneT include path:
  ;;
  ;; (require (planet "install.ss" ("dyoo" "divascheme.plt" 1)))
  ;;
  ;; Actually, this does a bit more: if we need to do any
  ;; postinstall stuff, we'll do it in here.  This must be executed
  ;; in the context of DrScheme.
  
  (require (lib "util.ss" "planet")
           (lib "list.ss")
           (lib "plt-match.ss")
           
           
           ;; NOTE: diva-preferences.ss must be invoked, even if we
           ;; don't use any of its defaults here.  It installs
           ;; preference defaults that we will be looking at later.
           (prefix preferences: "diva-preferences.ss"))
  
  
  ;; print-installation-finished-msg: -> void
  ;; Prints out the installed message banner.
  (define (print-installation-finished-msg)
    (printf "DivaScheme should now be installed.~n~nTo finish the installation, please restart DrScheme.~nOnce restarted, F4 will toggle DivaScheme on and off."))
  
  
  ;; get-planet-package-installed-versions: -> (listof (list major
  ;;                                                         (listof minor)))
  ;; looking at the installed cache, we pull out all the major/minor installs.
  ;; If we can't find anything, returns the empty list.
  (define (get-planet-package-installed-versions owner pkg)
    (cond
      [(assoc owner (current-cache-contents))
       => (lambda (d-owner)
            (cond
              [(assoc pkg (rest d-owner)) => rest]
              [else '()]))]
      [else '()]))
  
  
  ;; package-version-installed?: string string nat nat -> boolean
  ;; Returns true if the package with a particular major and minor
  ;; are installed.
  (define (package-version-installed? owner package major minor)
    (cond
      [(assoc major (get-planet-package-installed-versions owner package))
       =>
       (lambda (major&minors)
         (cond
           [(member minor (second major&minors)) #t]
           [else #f]))]
      [else #f]))
  
  
  ;; no-newer-package-version?: string string nat nat -> boolean
  ;; Returns true if there are no other installs of the package that have
  ;; a higher major/minor combination.
  (define (no-newer-package-version? owner package major minor)
    (let ([major&minors (get-planet-package-installed-versions owner package)])
      (let loop ([major&minors major&minors])
        (cond
          [(empty? major&minors) #t]
          [else
           (let ([installed-major (first (first major&minors))]
                 [installed-minors (second (first major&minors))])
             (cond
               [(< installed-major major)
                (loop (rest major&minors))]
               [(= installed-major major)
                (<= (apply max installed-minors) minor)]
               [else #f]))]))))
  
  
  ;; has-no-contextual-square-open? -> boolean
  ;; Returns true if open-square hasn't been bound.
  (define (has-no-contextual-square-open?)
    (and (member '("[" "diva:open")
                 (preferences:get-command-mode-bindings))
         (member '("[" "diva:open-square")
                 (preferences:get-insert-mode-bindings))))
  
  
  ;; eligible-for-contextual-square-keybinding-update?: -> boolean
  ;; Returns true if we should do the update.  We update only if
  ;; an older version of DivaScheme exists.
  (define (eligible-for-contextual-square-keybinding-update?)
    ;; At the time of our 2.2 release, the major and minor
    ;; number on planet should be (1 1).
    (and (no-newer-package-version? "divascheme" "divascheme.plt" 1 1)
         (package-version-installed? "divascheme" "divascheme.plt" 1 0)
         (has-no-contextual-square-open?)))
  
  
  (define (upgrade-preferences-with-magic-square-keybinding)
    (define (list-replace lst x y)
      (let loop ([lst lst])
        (cond
          [(empty? lst) lst]
          [(equal? (first lst) x)
           (cons y (loop (rest lst)))]
          [else (cons (first lst)
                      (loop (rest lst)))])))
    (preferences:set-command-mode-bindings
     (list-replace (preferences:get-command-mode-bindings)
                   '("[" "diva:open")
                   '("[" "diva:open-square/contextual")))
    (preferences:set-insert-mode-bindings
     (list-replace (preferences:get-insert-mode-bindings)
                   '("[" "diva:open-square")
                   '("[" "diva:open-square/contextual"))))
  
  
  (when (eligible-for-contextual-square-keybinding-update?)
    (upgrade-preferences-with-magic-square-keybinding))
  
  (print-installation-finished-msg))