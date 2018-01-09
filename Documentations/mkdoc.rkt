#lang racket

(define sln-root (make-parameter #false))

(define handbook-root
  (lambda []
    (simplify-path (build-path (sln-root) "Documentations" "Handbook"))))

(define handbook-entry
  (lambda [proj-root]
    (define-values (parent projname _) (split-path proj-root))
    (simplify-path (build-path proj-root (path-add-extension projname #".scrbl")))))

(define make-doc
  (lambda [handbook.scrbl]
    (parameterize ([current-namespace (make-base-namespace)])
      (parameterize ([exit-handler (thunk* (error 'make "[fatal] ~a needs a proper `exit-handler`!" handbook.scrbl))])
        (eval '(require (prefix-in html: scribble/html-render) setup/xref scribble/render))
        (eval `(render (list ,(dynamic-require handbook.scrbl 'doc)) (list ,(file-name-from-path handbook.scrbl))
                       #:render-mixin (λ [%] (html:render-multi-mixin (html:render-mixin %)))
                       #:dest-dir ,(handbook-root) #:xrefs (list (load-collections-xref))
                       #:quiet? #false #:warn-undefined? #false))))))

(define find-solution-root-dir
  (lambda [dir]
    (cond [(ormap (curry regexp-match? #px"[.]sln$") (directory-list dir)) dir]
          [else (let-values ([(parent dirname _) (split-path dir)])
                  (and parent (find-solution-root-dir parent)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (define mkdoc.rkt (simplify-path (build-path (find-system-path 'orig-dir) (find-system-path 'run-file))))

  (parameterize ([sln-root (find-solution-root-dir (path-only mkdoc.rkt))])
    (when (sln-root)
      (define all-projects (filter directory-exists? (directory-list (sln-root) #:build? #true)))
      (define handbook.scrbls (filter file-exists? (map handbook-entry all-projects)))
      (for ([handbook.scrbl (in-list handbook.scrbls)])
        (parameterize ([current-custodian (make-custodian)])
          (define-values (/dev/hbin /dev/hbout) (make-pipe))
          (parameterize ([current-output-port /dev/hbout])
            (make-doc handbook.scrbl)
            (close-output-port /dev/hbout))
          
          (define index.html (caddr (read /dev/hbin)))
          (system (format "open ~a" index.html))
          (printf "[Output to ~a]~n" index.html)
          (custodian-shutdown-all (current-custodian)))))))
  