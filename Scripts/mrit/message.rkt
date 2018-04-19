#lang racket

(provide (all-defined-out))

(require file/sha1)

(struct mrmsg (code block addr0 addrn data) #:transparent)

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
  (lambda [/dev/tcpout code db addr0 addrn [memory #false]]
    (define header (make-bytes (+ 2 2 2 2 2)))
    (define tail (make-bytes 4))
    (define dsize (if (bytes? memory) (- addrn addr0 -1) 0))

    (bytes-set! header 0 (char->integer #\$))
    (bytes-set! header 1 code)
    (integer->integer-bytes db 2 #false #true header 2)
    (integer->integer-bytes addr0 2 #false #true header 4)
    (integer->integer-bytes addrn 2 #false #true header 6)
    (integer->integer-bytes dsize 2 #false #true header 8)
    
    (integer->integer-bytes #xFFFF 2 #false #true tail 0)
    (bytes-set! tail 2 (char->integer #\return))
    (bytes-set! tail 3 (char->integer #\newline))

    (define hsize (write-bytes header /dev/tcpout 0))
    (define msize (if (bytes? memory) (write-bytes memory /dev/tcpout addr0 (+ addrn 1)) 0))
    (define tsize (write-bytes tail /dev/tcpout 0))

    (printf "[message signature: ~a ~a]~n" (bytes->hex-string header) (bytes->hex-string tail))
    
    (flush-output /dev/tcpout)
    
    (+ hsize msize tsize)))
