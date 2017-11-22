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
         (thunk (let-values ([(/dev/tcpin /dev/mbout) (tcp-connect/enable-break "172.16.8.222" 502)])
                  (let ([now (seconds->date (* (current-inexact-milliseconds) 0.001))])
                    (define greetings (make-message (format "Hello, I am Racket! 我替代了例子中的客户端(~a)。" 'github:microsoft/windows-universal-samples.git)))
                    (write-bytes (integer->integer-bytes (random #xFFFF) 2 #false #true) /dev/mbout)
                    (write-bytes (integer->integer-bytes #x00 2 #false #true) /dev/mbout)
                    (write-bytes (integer->integer-bytes (+ (string-utf-8-length greetings) 2) 2 #false #true) /dev/mbout)
                    (write-byte #xFF /dev/mbout)
                    (write-byte #x05 /dev/mbout)
                    (write-string greetings /dev/mbout)
                    (flush-output /dev/mbout)
                    (displayln greetings))))
         (thunk (custodian-shutdown-all (current-custodian))))))
    
    (sleep 1)
    (connect-send-wait-loop)))
