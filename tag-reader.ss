(module tag-reader mzscheme
  (require (lib "file.ss")
           (lib "list.ss")
           
           (lib "plt-match.ss")
           (lib "pregexp.ss")
           (lib "contract.ss")
           (lib "struct.ss")
           (lib "serialize.ss"))
  
  
  ;; A tag is a (make-tag n f l p)
  ;; where n is a string, f is a path, and l and p are positive numbers.
  ;; Tags give us location information for names that we're interested in.
  (define-struct tag (name path line position) #f)
  (provide (struct tag (name path line position)))
  
  ;; A tag library is a collection of tags.
  ;; fixme: enhance me for faster searching
  (define-struct tag-library (all-tags))
  (provide/contract (tag-library? (-> any/c boolean?)))
  
  
  (provide empty-tag-library)
  (define empty-tag-library (make-tag-library empty))
  
  (provide/contract (open-tag-library (path-string? . -> . tag-library?)))
  ;; open-tag-library: filename -> tag-library
  (define (open-tag-library filename)
    (call-with-input-file filename
      (lambda (ip)
        (open-tag-library/input-port ip (path-only filename)))))
  
  
  (provide/contract (open-tag-library/input-port (input-port? path-string? . -> . tag-library?)))
  (define (open-tag-library/input-port ip base-directory)
    (let* ([all-tags
            (foldl 
             (lambda (index-exp acc)
               (match index-exp
                 [(list id serialized-path line position)
                  (cons (make-tag id (deserialize serialized-path) line position)
                        acc)]
                 [else
                  (error "Malformed index sexp at: ~a" index-exp)]))
             empty
             (read ip))]
           [all-tags-with-absolute-paths
            (map (lambda (t)
                   (copy-struct 
                    tag t
                    (tag-path (path->complete-path
                               (build-path base-directory (tag-path t))))))
                 all-tags)])
      ;; todo: do something interesting here for fast indexing
      (make-tag-library all-tags-with-absolute-paths)))
  
  
  (provide/contract (tag-library-lookup (tag-library? string? . -> . (listof tag?))))

  ;; lookup-tags-linearly: (listof tag) string -> (listof tag)
  (define (tag-library-lookup library query)

    ;; case-matters?: -> boolean
    ;; Checks to see if we should treat the query as a case-sensitive one.
    (define (case-matters?)
      (pregexp-match "[A-Z]" query))
    
    ;; lookup-tags-linearly: (listof tag) string -> (listof tag)
    (define (linear-scan tags)
      (define query-pattern (if (case-matters?)
                                (format "^~a" (pregexp-quote query))
                                (format "^(?i:~a)" (pregexp-quote query))))
      
      (define (name-matches? name)
        (pregexp-match query-pattern name))
      
      (filter (lambda (a-tag)
                (match a-tag
                  [(struct tag (name file line position))
                   (name-matches? name)]))
              tags))
    
    (match library
      [(struct tag-library (tags))
       ;; for the moment, do naive linear search till we know that there isn't
       ;; something better in one of the plt collections
       (linear-scan tags)])))


