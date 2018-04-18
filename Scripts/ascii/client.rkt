#lang racket

(require racket/tcp)
(require racket/date)

(date-display-format 'iso-8601)

(define make-message
  (lambda [msg]
    (define now (seconds->date (* (current-inexact-milliseconds) 0.001)))
    (string-append "[" (date->string now #true)
                   "." (~a #:min-width 3 #:align 'right #:pad-string "0" (exact-round (/ (date*-nanosecond now) 1000000)))
                   "]" (~a msg))))

(with-handlers ([exn:break? void])
  (let connect-send-wait-loop ()
    (with-handlers ([exn:fail? (λ [e] (fprintf (current-error-port) "~a~n" (exn-message e)))])
      (parameterize ([current-custodian (make-custodian)])
        (dynamic-wind
         (thunk (void))
         (thunk (let-values ([(/dev/tcpin /dev/mbout) (tcp-connect/enable-break "127.0.0.1" 2100)])
                  (let ([now (seconds->date (* (current-inexact-milliseconds) 0.001))])
                    (define greetings (make-message (format "Hello, I am Racket! 我替代了例子中的客户端(~a)。" 'github:microsoft/windows-universal-samples.git)))
                    (write-char #\$) ; #x24
                    (write-char #x41)
                    (write-bytes (integer->integer-bytes #x0062 2 #false #true) /dev/mbout) ; DB
                    (write-bytes (integer->integer-bytes #x0000 2 #false #true) /dev/mbout) ; start address
                    (write-bytes (integer->integer-bytes #x11D9 2 #false #true) /dev/mbout) ; end address
                    (write-byte #xFF /dev/mbout)
                    (write-byte #x05 /dev/mbout)
                    (write-string greetings /dev/mbout)
                    (flush-output /dev/mbout)
                    (displayln greetings))))
         (thunk (custodian-shutdown-all (current-custodian))))))
    
    (sleep 1)
    (connect-send-wait-loop)))
