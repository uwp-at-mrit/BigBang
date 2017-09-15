#lang racket

(require racket/udp)

(define client (udp-open-socket))
(udp-send-to client "172.16.8.1" 18030 #"Hello, Rsyslog!")
(udp-send-to client "172.16.8.1" 18030 #"I am Racket!")
