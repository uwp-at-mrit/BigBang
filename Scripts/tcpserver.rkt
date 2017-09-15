#lang racket

(require racket/tcp)

(define (server)
  (define listener (tcp-listen 18030 4 #true))
  (define-values (/dev/tcpin /dev/tcpout) (tcp-accept/enable-break listener))

  (define-values (local lport remote rport) (tcp-addresses /dev/tcpout #true))
  (displayln (read-line /dev/tcpin))
  (fprintf /dev/tcpout "Greetings, ~a:~a~n" remote rport)
  (flush-output /dev/tcpout)
  (tcp-abandon-port /dev/tcpin)
  (tcp-abandon-port /dev/tcpout)
  (tcp-close listener)
  (server))

(server)
