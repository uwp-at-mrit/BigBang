#lang racket

(require racket/tcp)

(define (server)
  (with-handlers ([exn:fail? (λ [e] (fprintf (current-error-port) "~a~n" (exn-message e)))])
    (parameterize ([current-custodian (make-custodian)])
      (dynamic-wind
       (thunk (void))
       (thunk (let ([listener (tcp-listen 18030 256 #true)])
                (define-values (/dev/tcpin /dev/tcpout) (tcp-accept/enable-break listener))
                (file-stream-buffer-mode /dev/tcpout 'line)
                
                (define-values (local lport remote rport) (tcp-addresses /dev/tcpout #true))
                (displayln (read-line /dev/tcpin))
                (fprintf /dev/tcpout "Greetings, ~a:~a, I am Racket Too.~n" remote rport)))
       (thunk (custodian-shutdown-all (current-custodian))))))
  (server))

(with-handlers ([exn? void])
  (server))
