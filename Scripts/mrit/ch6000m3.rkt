#lang racket

(require "message.rkt")

(require racket/tcp)

(define memory (make-bytes #x1264))
(define master-ipv4 (vector-ref (current-command-line-arguments) 0))

(define refresh-memory
  (lambda []
    ;;; DB2
    (for ([i (in-range 1 176)]) ;; don't change the tidemark
      (real->floating-point-bytes (+ 2.0 (random)) 4 #true memory (+ 3422 (* i 4))))
    
    ;;; DB203
    (for ([i (in-range 280)])
      (real->floating-point-bytes (+ 203.0 (random)) 4 #true memory (+ 1120 (* i 4))))

    ;;; DB205
    (for ([i (in-range 385)])
      (define state (arithmetic-shift #x1 (random 8)))
      (bytes-set! memory (+ 4322 i) state))))

(with-handlers ([exn:break? void])
  (let connect-send-wait-loop ()
    (with-handlers ([exn:fail? (λ [e] (fprintf (current-error-port) "~a~n" (exn-message e)))])
      (parameterize ([current-custodian (make-custodian)])
        (dynamic-wind
         (thunk (void))
         (thunk (let-values ([(/dev/tcpin /dev/tcpout) (tcp-connect/enable-break master-ipv4 2008)])
                  (define-values (local lport remote rport) (tcp-addresses /dev/tcpout #true))
                  (printf "[connected to ~a:~a]~n" remote rport)

                  (let wait-read-response-loop ()
                    (define-values (signature tidemark) (read-mrmsg /dev/tcpin 40))
                    (define-values (addr0 addrn) (values (mrmsg-addr0 signature) (mrmsg-addrn signature)))

                    (case (mrmsg-code signature)
                      [(#x41) (refresh-memory)
                              (printf ">> [sent ~a bytes to ~a:~a]~n"
                                      (write-mrmsg /dev/tcpout (mrmsg-code signature) (mrmsg-block signature) addr0 addrn memory)
                                      remote rport)]
                      [else (void)])
                    (wait-read-response-loop))))
         (thunk (custodian-shutdown-all (current-custodian))))))
    
    (sleep 1)
    (connect-send-wait-loop)))
