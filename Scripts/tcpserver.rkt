#lang racket

(require racket/tcp)

(define (server)
  (define listener (tcp-listen 18030 4 #true))
  (define-values (/dev/tcpin /dev/tcpout) (tcp-accept/enable-break listener))

  (displayln (read-line /dev/tcpin))
  (displayln "Greetings" /dev/tcpout)
  (flush-output /dev/tcpout)
  (tcp-abandon-port /dev/tcpin)
  (tcp-abandon-port /dev/tcpout)
  (tcp-close listener)
  (server))

(server)
