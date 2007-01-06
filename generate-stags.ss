#|
Generates an TAGS.scm file suitable for divascheme tags support.

   cd plt/collects; find . -name \*.ss | xargs generate-stags

This will create a file called 'STAGS', which can then be used by
divascheme.  While in divascheme's command mode, hit '.'  It will
prompt for an identifier's name, and then jump to the file and the
line where that name is defined.


Sun Apr 17 2005
    Ported to MzScheme 299, based on a patch by Danny Yoo

Sun Nov 2 2003
    Written by Guillaume Marceau (gmarceau@cs.brown.edu)  

|#
(module generate-stags mzscheme
  ;; TODO: use cmdline
  ;; TODO: support project directories
  (require "stags-lib.ss")
  (with-handlers
        ([exn:break? (lambda (exn) (void))])
      (generate-stags-file (vector->list (current-command-line-arguments)) "STAGS")))