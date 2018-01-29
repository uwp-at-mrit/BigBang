#lang racket

(provide (all-defined-out))

(define sln-root (make-parameter #false)) 

(define document-root
  (lambda []
    (simplify-path (build-path (sln-root) "Documentations"))))

(define handbook-root
  (lambda []
    (build-path (document-root) "Handbook")))

(define handbook-entry
  (lambda [[proj-root (current-directory)]]
    (define-values (parent projname _) (split-path proj-root))
    (simplify-path (build-path proj-root (path-add-extension projname #".scrbl")))))

(define handbook-dest
  (lambda [[proj-root (current-directory)]]
    (define-values (parent projname _) (split-path proj-root))
    (simplify-path (build-path proj-root "Generated Files" "Handbook"))))

(define handbook-style
  (lambda [[proj-root (current-directory)]]
    (define-values (parent projname _) (split-path proj-root))
    (simplify-path (build-path proj-root (path-add-extension projname #".css")))))

(define find-solution-root-dir
  (lambda [[dir (current-directory)]]
    (cond [(ormap (curry regexp-match? #px"[.]sln$") (directory-list dir)) dir]
          [else (let-values ([(parent dirname _) (split-path dir)])
                  (and parent (find-solution-root-dir parent)))])))
