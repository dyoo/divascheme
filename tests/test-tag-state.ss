(module test-tag-state mzscheme
  (require "test-harness.ss"
           "../tag-state.ss"
           "../tag-reader.ss")

  ;; Stop when something bad happens
  (print-tests 'stop)
  
  ;; Test for loading from arbitrary input port
  (set-current-tag-library! (open-tag-library/input-port 
   (open-input-string #<<EOF
(
("cleanup-whitespace" "actions.ss" 520 24518)
("diva-printf" "diva-panel.ss" 27 612)
("to-pipe-mode" "insert-keymap.ss" 171 6058)
)
EOF
                      )
   (current-directory)))
  
  (test (tag-library-lookup (get-current-tag-library) "diva-printf")
        (list (make-tag
               "diva-printf"
               (path->complete-path (build-path (current-directory) "diva-panel.ss"))
               27
               612))))