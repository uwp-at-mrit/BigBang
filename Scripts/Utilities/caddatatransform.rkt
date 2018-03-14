#lang racket/gui

(define transform
  (lambda [/dev/stdin]
    (filter-map (λ [line]
                  (define pattern (regexp-match #px"X = ([0-9.]+), Y = ([-0-9.]+)" line))
                  (and pattern
                       (make-rectangular (string->number (cadr pattern))
                                         (string->number (caddr pattern)))))
                (port->lines /dev/stdin))))

(define res-root (get-directory "Choose the root directory of the raw vertices"))

(when (path? res-root)
  (for ([src.txt (in-directory res-root)] #:when (regexp-match? #px"[.]txt$" src.txt))
    (define vertices
      (call-with-input-file* src.txt
        (λ [/dev/stdin]
          (cond [(not (regexp-match? #px"^\\s*X =" /dev/stdin)) (printf "[Skipped]~a~n" src.txt)]
                [else (with-handlers ([exn? (λ [e] (fprintf (current-error-port) "[Failed]~a: ~a~n" src.txt (exn-message e)))])
                        (transform /dev/stdin))]))))
    (when (pair? vertices)
      (with-handlers ([exn? (λ [e] (fprintf (current-error-port) "[Failed]~a: ~a~n" src.txt (exn-message e)))])
        (call-with-output-file* src.txt #:exists 'truncate/replace
          (λ [/dev/stdout]
            (for ([v (in-list vertices)])
              (displayln vertices /dev/stdout)
              (displayln vertices)))))
      (printf "[Transformed]~a~n" src.txt))))
