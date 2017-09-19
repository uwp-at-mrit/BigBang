#lang racket

(require racket/udp)

(define datagram (make-bytes 4096))

(define (server)
  (define listener (udp-open-socket))
  (udp-bind! listener #false 18030 #true)
  (define-values (lhost lport _r _p) (udp-addresses listener #true))
  (printf "> ~a:~a~n" lhost lport)
  (let wait-recv-print-loop ()
    (define-values (size remote rport) (udp-receive!/enable-break listener datagram))
    (define /dev/stdout (current-output-port))
    (fprintf /dev/stdout "[~a:~a] " remote rport)
    (write-bytes datagram /dev/stdout 0 size)
    (newline /dev/stdout)
    (wait-recv-print-loop)))

(with-handlers ([exn:break? void])
  (server))
