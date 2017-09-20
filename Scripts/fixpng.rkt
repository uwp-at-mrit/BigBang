#lang racket/gui

(define fixpng
  (lambda [src.png]
    (define png (read-bitmap src.png))
    (send png save-file src.png 'png)
    (printf "[Fixed]~a~n" src.png)))

(define res-root (get-directory "Choose the root directory of the image resources"))

(when (path? res-root)
  (for ([src.png (in-directory res-root)] #:when (regexp-match? #px"[.]png$" src.png))
    (define maybe-ok (with-handlers ([exn? values]) (read-bitmap src.png 'png/alpha)))
    (cond [(not (exn? maybe-ok)) (printf "[Skipped]~a~n" src.png)]
          [else (with-handlers ([exn? (λ [e] (fprintf (current-error-port) "[Failed]~a: ~a~n" src.png (exn-message e)))])
                  (fixpng src.png))])))
