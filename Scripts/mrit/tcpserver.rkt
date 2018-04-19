#lang racket

(require "message.rkt")

(require racket/tcp)

(define memory (make-bytes 0))

(define (server)
  (with-handlers ([exn:fail? (λ [e] (fprintf (current-error-port) "~a~n" (exn-message e)))])
    (parameterize ([current-custodian (make-custodian)])
      (dynamic-wind
       (thunk (void))
       (thunk (let ([listener (tcp-listen 2100 256 #true)])
                (define-values (hostname port _r _p) (tcp-addresses listener #true))
                (printf "> ~a:~a~n" hostname port)
                (define-values (/dev/tcpin /dev/tcpout) (tcp-accept/enable-break listener))
                (file-stream-buffer-mode /dev/tcpout 'none)
                
                (define-values (local lport remote rport) (tcp-addresses /dev/tcpout #true))
                (printf "[accepted ~a:~a]~n" hostname port)

                (define-values (signature _) (read-mrmsg /dev/tcpin))
                (printf "[received ~a]~n" signature)
                (define-values (addr0 addrn) (values (mrmsg-addr0 signature) (mrmsg-addrn signature)))

                (when (> addrn (bytes-length memory)) (set! memory (make-bytes (+ addrn 1))))
                (for ([i (in-range addr0 (+ addrn 1))]) (bytes-set! memory i (random 0 256)))

                (printf ">> [sent ~a bytes to ~a:~a]~n"
                        (write-mrmsg /dev/tcpout (mrmsg-code signature) (mrmsg-block signature) addr0 addrn memory)
                        remote rport)))
       (thunk (custodian-shutdown-all (current-custodian))))))
  (sleep 1)
  (server))

(with-handlers ([exn? void])
  (server))
