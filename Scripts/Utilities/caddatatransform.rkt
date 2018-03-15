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
  (for ([src.rktl (in-directory res-root)] #:when (regexp-match? #px"[.]rktl$" src.rktl))
    (define vertices
      (call-with-input-file* src.rktl
        (λ [/dev/stdin]
          (cond [(not (regexp-match? #px"^[[] AutoCAD" /dev/stdin)) (printf "[Skipped]~a~n" src.rktl)]
                [else (with-handlers ([exn? (λ [e] (fprintf (current-error-port) "[Failed]~a: ~a~n" src.rktl (exn-message e)))])
                        (transform /dev/stdin))]))))
    (when (pair? vertices)
      (with-handlers ([exn? (λ [e] (fprintf (current-error-port) "[Failed]~a: ~a~n" src.rktl (exn-message e)))])
        (call-with-output-file* src.rktl #:exists 'truncate/replace
          (λ [/dev/stdout]
            (displayln #\( /dev/stdout)
            (for ([v (in-list vertices)])
              (displayln v /dev/stdout)
              (displayln v))
            (displayln #\) /dev/stdout))))
      (printf "[Transformed]~a~n" src.rktl))))
