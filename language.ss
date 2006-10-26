(module language mzscheme
  (require (lib "etc.ss")
           (lib "list.ss")
           (lib "plt-match.ss")
           (lib "class.ss")
           (lib "unitsig.ss")
           (lib "tool.ss" "drscheme")
           (lib "string-constant.ss" "string-constants")
           (lib "framework.ss" "framework"))


  (provide get-language language@ language^)

  (define get-language false)

  (define-signature language^ ())

  (define language@
    (unit/sig language^
        (import drscheme:tool^)

      (set! get-language
            (lambda ()
              (let* ([language-settings (preferences:get (drscheme:language-configuration:get-settings-preferences-symbol))]
                     [language (drscheme:language-configuration:language-settings-language language-settings)]
                     [name1 (send language get-language-name)]
                     [name2 (first (last-pair (send language get-language-position)))])
                name2))))))