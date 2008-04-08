(module struct mzscheme
  (require "private/struct.ss"
           "private/cursor.ss")
  
  (provide (all-from "private/struct.ss")
           (all-from "private/cursor.ss")))