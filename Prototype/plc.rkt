#lang racket

(require racket/tcp)

(define-values (/dev/tcpin /dev/tcpout) (tcp-connect/enable-break "192.168.1.147" 18030))

(write "Hello, Visual C++/CX, I am Racket!" /dev/tcpout)
(newline /dev/tcpout)
(flush-output /dev/tcpout)
(tcp-abandon-port /dev/tcpout)
(read-line /dev/tcpin)
(tcp-abandon-port /dev/tcpin)
