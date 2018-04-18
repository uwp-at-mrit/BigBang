#lang racket

(require racket/tcp)


(define read-mrmsg
  (lambda [/dev/tcpin]
    (define head (read-byte /dev/tcpin))
    (define code (read-byte /dev/tcpin))
    (define DB (integer-bytes->integer (read-bytes 2 /dev/tcpin) #false #true))
    (define addr0 (integer-bytes->integer (read-bytes 2 /dev/tcpin) #false #true))
    (define addrn (integer-bytes->integer (read-bytes 2 /dev/tcpin) #false #true))
    (define size (integer-bytes->integer (read-bytes 2 /dev/tcpin) #false #true))
    (define data (read-bytes size /dev/tcpin))
    (define checksum (integer-bytes->integer (read-bytes 2 /dev/tcpin) #false #true))
    (define EOM (integer-bytes->integer (read-bytes 2 /dev/tcpin) #false #true))
    (list head code DB addr0 addrn data checksum EOM)))

(define (server)
  (with-handlers ([exn:fail? (λ [e] (fprintf (current-error-port) "~a~n" (exn-message e)))])
    (parameterize ([current-custodian (make-custodian)])
      (dynamic-wind
       (thunk (void))
       (thunk (let ([listener (tcp-listen 2100 256 #true)])
                (define-values (hostname port _r _p) (tcp-addresses listener #true))
                (printf "> ~a:~a~n" hostname port)
                (define-values (/dev/tcpin /dev/tcpout) (tcp-accept/enable-break listener))
                (file-stream-buffer-mode /dev/tcpout 'line)
                
                (define-values (local lport remote rport) (tcp-addresses /dev/tcpout #true))
                (define message (read-mrmsg /dev/tcpin))
                (writeln message)))
       (thunk (custodian-shutdown-all (current-custodian))))))
  (sleep 1)
  (server))

(with-handlers ([exn? void])
  (server))
