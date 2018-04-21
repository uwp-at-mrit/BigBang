#lang racket

(require "message.rkt")

(require racket/tcp)

(define memory (make-bytes 0))
(define master-ipv4 (vector-ref (current-command-line-arguments) 0))

(with-handlers ([exn:break? void])
  (let connect-send-wait-loop ()
    (with-handlers ([exn:fail? (λ [e] (fprintf (current-error-port) "~a~n" (exn-message e)))])
      (parameterize ([current-custodian (make-custodian)])
        (dynamic-wind
         (thunk (void))
         (thunk (let-values ([(/dev/tcpin /dev/tcpout) (tcp-connect/enable-break master-ipv4 2000)])
                  (define-values (local lport remote rport) (tcp-addresses /dev/tcpout #true))
                  (printf "[connected to ~a:~a]~n" remote rport)

                  (let wait-read-response-loop ()
                    (define-values (signature _) (read-mrmsg /dev/tcpin 40))
                    (define-values (addr0 addrn) (values (mrmsg-addr0 signature) (mrmsg-addrn signature)))
                    
                    (when (> addrn (bytes-length memory)) (set! memory (make-bytes (+ addrn 1))))
                    (for ([i (in-range addr0 (+ addrn 1))]) (bytes-set! memory i (random 0 256)))
                    
                    (printf ">> [sent ~a bytes to ~a:~a]~n"
                            (write-mrmsg /dev/tcpout (mrmsg-code signature) (mrmsg-block signature) addr0 addrn memory)
                            remote rport)
                    (wait-read-response-loop))))
         (thunk (custodian-shutdown-all (current-custodian))))))
    
    (sleep 1)
    (connect-send-wait-loop)))
