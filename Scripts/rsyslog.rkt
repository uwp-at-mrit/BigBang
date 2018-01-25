#lang racket

(require racket/udp)

(require "tamer/echo.rkt")

(define datagram (make-bytes 4096))

(define (log-color message)
  (define px:level (regexp-match #px"^<[^>]+>\\s+[[]([^]]+)[]]" message))
  (and (pair? px:level)
       (case (string->symbol (string-downcase (cadr px:level)))
         [(debug) "gray"]
         [(info) "green"]
         [(notice) 154 #|YellowGreen|#]
         [(warning) "yellow"]
         [(error) "red"]
         [(critical alert panic) "darkred"]
         [else #false])))

(define (server)
  (define listener (udp-open-socket))
  (udp-bind! listener #false 18030 #true)
  (define-values (lhost lport _r _p) (udp-addresses listener #true))
  (printf "> ~a:~a~n" lhost lport)
  (let wait-recv-print-loop ()
    (define-values (size remote rport) (udp-receive!/enable-break listener datagram))
    (define message (bytes->string/utf-8 datagram #false 0 size))
    (echof #:fgcolor (log-color message) "[~a:~a] ~a~n" remote rport message)
    (wait-recv-print-loop)))

(with-handlers ([exn:break? void])
  (server))
