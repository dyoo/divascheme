(module install mzscheme
  ;; Small little message for use with PLaneT installation.  The idea is to
  ;; provide people the following PLaneT include path:
  ;;
  ;; (require (planet "install.ss" ("dyoo" "divascheme.plt" 1)))
  ;;
  ;;
  (printf "DivaScheme should now be installed.~n~nTo finish the installation, please restart DrScheme.~nOnce restarted, F4 will toggle DivaScheme on and off."))