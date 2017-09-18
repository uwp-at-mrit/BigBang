#lang racket

(require racket/tcp)
(require racket/date)

(date-display-format 'iso-8601)

(with-handlers ([exn:break? void])
  (let connect-send-wait-loop ()
    (with-handlers ([exn:fail? (λ [e] (fprintf (current-error-port) "~a~n" (exn-message e)))])
      (parameterize ([current-custodian (make-custodian)])
        (dynamic-wind
         (thunk (void))
         (thunk (let-values ([(/dev/tcpin /dev/tcpout) (tcp-connect/enable-break "172.16.8.195" 18030)])
                  (let ([now (seconds->date (* (current-inexact-milliseconds) 0.001))])
                    (define greetings
                      (format "[~a.~a]Hello, I am Racket! 我替代了例子中的客户端(~a)。"
                        (date->string now #true)
                        (~a #:min-width 3 #:align 'right #:pad-string "0"
                            (exact-round (/ (date*-nanosecond now) 1000000)))
                        'github:microsoft/windows-universal-samples.git))
                    (write-bytes (integer->integer-bytes (string-utf-8-length greetings) 4 #false #true) /dev/tcpout)
                    (write-string greetings /dev/tcpout)
                    (flush-output /dev/tcpout)
                    (displayln greetings))))
         (thunk (custodian-shutdown-all (current-custodian))))))
    
    (sleep 1)
    (connect-send-wait-loop)))
