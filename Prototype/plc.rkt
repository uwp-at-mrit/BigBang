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
         (thunk (let-values ([(/dev/tcpin /dev/tcpout) (tcp-connect/enable-break "172.16.8.1" 18030)])
                  (file-stream-buffer-mode /dev/tcpout 'line)
                  
                  (let ([now (seconds->date (* (current-inexact-milliseconds) 0.001))])
                    (define ms (~a (exact-round (/ (date*-nanosecond now) 1000000)) #:min-width 3 #:align 'right #:pad-string "0"))
                    (fprintf /dev/tcpout "[~a.~a]Hello, I am Racket!~n" (date->string now #true) ms)
                    
                    (if (sync/timeout/enable-break 1.618 /dev/tcpin)
                        (displayln (read-line /dev/tcpin))
                        (error 'plc "[~a.~a]Handshaking Timeout!" (date->string now #true) ms)))))
         (thunk (custodian-shutdown-all (current-custodian))))))
    
    (sleep 1)
    (connect-send-wait-loop)))
