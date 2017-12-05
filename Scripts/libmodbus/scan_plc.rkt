#lang racket

(require racket/tcp)

(define write-request
  (lambda [/dev/mbout device transaction read-code object]
    (define pdu (bytes #x2B #x0E read-code object))
    (write-bytes (integer->integer-bytes transaction 2 #false #true) /dev/mbout)
    (write-bytes (integer->integer-bytes #x00 2 #false #true) /dev/mbout)
    (write-bytes (integer->integer-bytes (+ (bytes-length pdu) 1) 2 #false #true) /dev/mbout)
    (write-byte #xFF /dev/mbout)
    (write-bytes pdu /dev/mbout)
    (flush-output /dev/mbout)
    (printf "#[~a] sent request(~a, ~a, ~a)" device transaction read-code object)))

(define read-response
  (lambda [/dev/mbin device]
    (define adu (make-bytes 260))
    (read-bytes! adu /dev/mbin 0 7)
    (define transaction (integer-bytes->integer adu #false #true 0 2))
    (define protocol (integer-bytes->integer adu #false #true 2 4))
    (define size (integer-bytes->integer adu #false #true 4 6))
    (define unit (bytes-ref adu 6))
    (define pdu-length (read-bytes! adu /dev/mbin 7 (+ 6 size)))
    (define funcode (bytes-ref adu 7))
    (define MEI (bytes-ref adu 8))
    (printf "#[~a] received response(~a, ~a, ~a, ~a, ~a, ~a) " device transaction protocol (add1 pdu-length) unit funcode MEI)
    (define opcode (bytes-ref adu 9))
    (define conformity-level (bytes-ref adu 10))
    (define more? (bytes-ref adu 11))
    (define next-object (bytes-ref adu 12))
    (define count (bytes-ref adu 13))
    (printf "with device identification header(~a, ~a, ~a, ~a, ~a)~n" opcode conformity-level more? next-object count)
    (let read-object ([i 0] [idx 14])
      (when (< i count)
        (define object (bytes-ref adu idx))
        (define valsize (bytes-ref adu (add1 idx)))
        (printf "[Object ~a@~a: ~a]~n" (~r object #:base (list 'up 16) #:min-width 2 #:pad-string "0") idx
                (bytes->string/utf-8 adu #\? (+ idx 2) (+ idx 2 valsize)))
        (read-object (add1 i) (+ idx valsize 2))))))

(with-handlers ([exn:fail? (λ [e] (fprintf (current-error-port) "~a~n" (exn-message e)))])
  (define plcs
    (for/list ([host (in-range 1 254 1)])
      (parameterize ([current-custodian (make-custodian)])
        (thread (thunk (let ([device (format "192.168.0.~a" host)])
                         (dynamic-wind
                          (thunk (void))
                          (thunk (with-handlers ([exn:fail:network:errno? (λ [e] (printf "#[~a]~n~a~n" device (exn-message e)))])
                                   (let-values ([(/dev/mbin /dev/mbout) (tcp-connect/enable-break device 502)])
                                     (write-request /dev/mbout device #x00 #x01 #x00)
                                     (read-response /dev/mbin device)
                                     (write-request /dev/mbout device #x01 #x02 #x03)
                                     (read-response /dev/mbin device))))
                          (thunk (custodian-shutdown-all (current-custodian))))))))))

  (let wait ([evts (map thread-dead-evt plcs)])
    (unless (null? evts)
      (define evt (apply sync/enable-break evts))
      (wait (remove evt evts)))))
