#lang racket

(provide (all-defined-out))

(require file/sha1)

(define header-size (+ 1 1 2 2 2 2))
(define tail-size 4)

(struct mrmsg (head code block addr0 addrn size checksum eom) #:transparent)

(define (~hex n size)
  (~r n #:base 16 #:pad-string "0" #:min-width size))

(define (foldsum bs start stop)
  (for/sum ([b (in-bytes bs start (and stop (+ stop 1)))]) b))

(define read-mrmsg
  (lambda [/dev/tcpin [alignment-size 0]]
    (define head (read-byte /dev/tcpin))
    (define code (read-byte /dev/tcpin))
    (define block (integer-bytes->integer (read-bytes 2 /dev/tcpin) #false #true))
    (define addr0 (integer-bytes->integer (read-bytes 2 /dev/tcpin) #false #true))
    (define addrn (integer-bytes->integer (read-bytes 2 /dev/tcpin) #false #true))
    (define data-size (integer-bytes->integer (read-bytes 2 /dev/tcpin) #false #true))
    (define data (read-bytes data-size /dev/tcpin))
    (define checksum (integer-bytes->integer (read-bytes 2 /dev/tcpin) #false #true))
    (define EOM (integer-bytes->integer (read-bytes 2 /dev/tcpin) #false #true))

    (when (> alignment-size 0)
      (define padding-size (- alignment-size header-size data-size tail-size))
      (read-bytes padding-size /dev/tcpin))

    (printf "[received message signature: ~a{~a}~a]~n"
            (string-append (~hex head 2) (~hex code 2)
                           (~hex block 4) (~hex addr0 4) (~hex addrn 4) (~hex data-size 4))
            (~r (floating-point-bytes->real data #true 0 4) #:precision '(= 3))
            (string-append (~hex checksum 4) (~hex EOM 4)))
    
    (values (mrmsg head code block addr0 addrn data-size checksum EOM) data)))

(define write-mrmsg
  (lambda [/dev/tcpout code db addr0 addrn [memory #false]]
    (define header (make-bytes header-size))
    (define tail (make-bytes tail-size))
    (define dsize (if (bytes? memory) (- addrn addr0 -1) 0))

    (bytes-set! header 0 (char->integer #\$))
    (bytes-set! header 1 code)
    (integer->integer-bytes db 2 #false #true header 2)
    (integer->integer-bytes addr0 2 #false #true header 4)
    (integer->integer-bytes addrn 2 #false #true header 6)
    (integer->integer-bytes dsize 2 #false #true header 8)

    (define checksum (min (+ (foldsum header 0 #false) (foldsum memory addr0 addrn)) #xFFFF))
    (integer->integer-bytes checksum 2 #false #true tail 0)
    (integer->integer-bytes #x0D0A 2 #false #true tail 2)
    
    (define hsize (write-bytes header /dev/tcpout 0))
    (define msize (if (bytes? memory) (write-bytes memory /dev/tcpout addr0 (+ addrn 1)) 0))
    (define tsize (write-bytes tail /dev/tcpout 0))

    (flush-output /dev/tcpout)

    (printf "[sending message signature: ~a ~a]~n" (bytes->hex-string header) (bytes->hex-string tail))
    
    (+ hsize msize tsize)))
