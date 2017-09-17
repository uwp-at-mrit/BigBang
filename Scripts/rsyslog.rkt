#lang racket

(require racket/udp)
(require racket/date)

(define datagram (make-bytes 4096))

(define (server)
  (define listener (udp-open-socket))
  (udp-bind! listener #false 18030 #true)
  (let wait-recv-print-loop ()
    (define-values (size remote rport) (udp-receive!/enable-break listener datagram))
    (define /dev/stdout (current-output-port))
    (define now (seconds->date (* (current-inexact-milliseconds) 0.001)))
    (define ms (~a (exact-round (/ (date*-nanosecond now) 1000000)) #:min-width 3 #:left-pad-string "0"))
    (fprintf /dev/stdout "[~a:~a@~a.~a] " remote rport (date->string now #true) ms)
    (write-bytes datagram /dev/stdout 0 size)
    (newline /dev/stdout)
    (wait-recv-print-loop)))

(date-display-format 'iso-8601)
(server)
