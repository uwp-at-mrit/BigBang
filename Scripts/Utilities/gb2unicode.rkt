#lang racket/gui

(define gbk->utf8
  (lambda [src.java]
    (define /dev/gbkin (reencode-input-port (open-input-file src.java) "GB18030" #false #true))
    (define java (port->string /dev/gbkin))
    (close-input-port /dev/gbkin)
    (call-with-output-file* src.java #:exists 'truncate/replace
      (λ [/dev/utf8out] (displayln java /dev/utf8out)))
    (printf "[Converted]~a~n" src.java)))

(define java-root (get-directory "Choose the root directory of the java sources"))

(when (path? java-root)
  (for ([src.java (in-directory java-root)] #:when (regexp-match? #px"[.]java$" src.java))
    (define maybe-ok (with-handlers ([exn? values]) (bytes->string/utf-8 (file->bytes src.java))))
    (cond [(not (exn? maybe-ok)) (printf "[Skipped]~a~n" src.java)]
          [else (with-handlers ([exn? (λ [e] (fprintf (current-error-port) "[Failed]~a: ~a~n" (exn-message e)))])
                  (gbk->utf8 src.java))])))
