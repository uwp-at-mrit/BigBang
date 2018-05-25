#lang racket

(define force-remake (make-parameter #false))
(define sln-root (make-parameter #false))

(define find-solution-root-dir
  (lambda [[dir (current-directory)]]
    (cond [(ormap (curry regexp-match? #px"[.]sln$") (directory-list dir)) dir]
          [else (let-values ([(parent dirname _) (split-path dir)])
                  (and parent (find-solution-root-dir parent)))])))

(define schema-exists?
  (lambda [schema.rktl]
    (regexp-match? #px"[.]sql[.]rktl$" schema.rktl)))

(define do-make-dao
  (lambda [cat schema]
    (printf "> cat ~a~n" (file-name-from-path schema))
    (call-with-output-file* schema #:exists 'truncate/replace
      (λ [/dev/schout]
        (cat (current-output-port))
        (cat /dev/schout)))))

(define make-daos
  (lambda [schema.sql.rktl]
    (define schema (cadr (regexp-match #px".*[/](.+)[.]sql[.]rktl$" schema.sql.rktl)))
    (define schema.hpp (build-path (path-only schema.sql.rktl) (path-add-extension schema #".hpp")))
    (define schema.cpp (build-path (path-only schema.sql.rktl) (path-add-extension schema #".cpp")))

    (define rkt.mtime (file-or-directory-modify-seconds schema.sql.rktl))
    (define hpp.mtime (if (or (force-remake) (not (file-exists? schema.hpp))) (- rkt.mtime 1) (file-or-directory-modify-seconds schema.hpp)))
    (define cpp.mtime (if (or (force-remake) (not (file-exists? schema.cpp))) (- rkt.mtime 1) (file-or-directory-modify-seconds schema.cpp)))

    (when (or (> rkt.mtime hpp.mtime) (> rkt.mtime cpp.mtime))
      (dynamic-require schema.sql.rktl #false)
      (define schema-zone (module->namespace schema.sql.rktl))
      (define cat-schema.hpp (namespace-variable-value (string->symbol (format "cat-~a.hpp" schema)) #false #false schema-zone))
      (define cat-schema.cpp (namespace-variable-value (string->symbol (format "cat-~a.cpp" schema)) #false #false schema-zone))

      (when (> rkt.mtime hpp.mtime)
        (do-make-dao cat-schema.hpp schema.hpp))
      
      (newline)
      (newline)
      (newline)

      (when (> rkt.mtime hpp.mtime)
        (do-make-dao cat-schema.cpp schema.cpp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (command-line
   #:program "mkdao"
   #:once-each
   [("-f" "--force") "force remake database access objects" (force-remake #true)]
   #:args []
   (let ([mkdao.rkt (simplify-path (build-path (find-system-path 'orig-dir) (find-system-path 'run-file)))])
     (parameterize ([sln-root (find-solution-root-dir (path-only mkdao.rkt))])
       (when (sln-root)
         (define all-projects (filter directory-exists? (directory-list (sln-root) #:build? #true)))
         (for ([project-root (in-list all-projects)])
           (define schema-root (build-path project-root "schema"))
           (when (directory-exists? schema-root)
             (for-each make-daos (filter schema-exists? (directory-list schema-root #:build? #true))))))))))