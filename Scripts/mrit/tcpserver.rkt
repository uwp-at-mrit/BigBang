#lang racket

(require racket/tcp)

(struct mrmsg (code block addr0 addrn data) #:transparent)

(define memory (make-bytes 0))
(define header (make-bytes (+ 2 2 2 2 2)))
(define tail (make-bytes 4))

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
    (mrmsg code DB addr0 addrn data)))

(define write-mrmsg
  (lambda [/dev/tcpout code db addr0 addrn]
    (define dsize (- addrn addr0 -1))
    
    (bytes-set! header 0 (char->integer #\$))
    (bytes-set! header 1 code)
    (integer->integer-bytes db 2 #false #true header 2)
    (integer->integer-bytes addr0 2 #false #true header 4)
    (integer->integer-bytes addrn 2 #false #true header 6)
    (integer->integer-bytes dsize 2 #false #true header 8)
    
    (integer->integer-bytes #xFFFF 2 #false #true tail 0)
    (bytes-set! tail 2 (char->integer #\return))
    (bytes-set! tail 3 (char->integer #\newline))

    (write-bytes header /dev/tcpout)
    (write-bytes memory /dev/tcpout addr0 (+ addrn 1))
    (write-bytes tail /dev/tcpout)

    (flush-output /dev/tcpout)

    (+ (bytes-length header) dsize (bytes-length tail))))

(define (server)
  (with-handlers ([exn:fail? (λ [e] (fprintf (current-error-port) "~a~n" (exn-message e)))])
    (parameterize ([current-custodian (make-custodian)])
      (dynamic-wind
       (thunk (void))
       (thunk (let ([listener (tcp-listen 2100 256 #true)])
                (define-values (hostname port _r _p) (tcp-addresses listener #true))
                (printf "> ~a:~a~n" hostname port)
                (define-values (/dev/tcpin /dev/tcpout) (tcp-accept/enable-break listener))
                
                (define-values (local lport remote rport) (tcp-addresses /dev/tcpout #true))
                (define message (read-mrmsg /dev/tcpin))
                (define-values (addr0 addrn) (values (mrmsg-addr0 message) (mrmsg-addrn message)))

                (when (> addrn (bytes-length memory)) (set! memory (make-bytes (+ addrn 1))))
                (for ([i (in-range addr0 (+ addrn 1))]) (bytes-set! memory i (random 0 256)))
                
                (printf ">> [sent ~a bytes to ~a:~a]~n"
                        (write-mrmsg /dev/tcpout (mrmsg-code message) (mrmsg-block message) addr0 addrn)
                        remote rport)))
       (thunk (custodian-shutdown-all (current-custodian))))))
  (sleep 1)
  (server))

(with-handlers ([exn? void])
  (server))
