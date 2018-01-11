#lang racket

(require "handbook.rkt")

(define make-docs
  (lambda [handbook.scrbl]
    (parameterize ([current-namespace (make-base-namespace)])
      (parameterize ([exit-handler (thunk* (error 'make "[fatal] ~a needs a proper `exit-handler`!" handbook.scrbl))])
        ;;; WARNING: the eval namespace does not share handbook module with the main namespace.
        (eval '(require (prefix-in html: scribble/html-render) setup/xref scribble/render))
        (eval `(render (list ,(dynamic-require handbook.scrbl 'doc)) (list ,(file-name-from-path handbook.scrbl))
                       #:render-mixin (λ [%] (html:render-multi-mixin (html:render-mixin %)))
                       #:dest-dir ,(handbook-root) #:xrefs (list (load-collections-xref))
                       #:quiet? #false #:warn-undefined? #false))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (define mkdoc.rkt (simplify-path (build-path (find-system-path 'orig-dir) (find-system-path 'run-file))))

  (parameterize ([sln-root (find-solution-root-dir (path-only mkdoc.rkt))])
    (when (sln-root)
      (define all-projects (filter directory-exists? (directory-list (sln-root) #:build? #true)))
      (for ([project-root (in-list all-projects)])
        (define handbook.scrbl (handbook-entry project-root))
        (when (file-exists? handbook.scrbl)
          (parameterize ([current-directory project-root]
                         [current-custodian (make-custodian)])
            (define /dev/hbout (open-output-string))
            (parameterize ([current-output-port /dev/hbout])
              (make-docs handbook.scrbl))

            (define maybe-output-to (get-output-string /dev/hbout))
            (with-handlers ([exn:fail? (λ [e] (displayln maybe-output-to))])
              (define index.html (caddr (read (open-input-string maybe-output-to))))
              (system (format "open ~a" index.html))
              (printf "[Output to ~a]~n" index.html))
            (custodian-shutdown-all (current-custodian))))))))