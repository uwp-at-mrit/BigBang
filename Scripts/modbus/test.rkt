#lang racket

(provide (all-defined-out))

(define UT_BITS_ADDRESS            #x130)
(define UT_BITS_NB                 #x25)
(define UT_BITS_TAB                #(#xCD #x6B #xB2 #x0E #x1B))

(define UT_INPUT_BITS_ADDRESS      #x1C4)
(define UT_INPUT_BITS_NB           #x16)
(define UT_INPUT_BITS_TAB          #(#xAC #xDB #x35))

(define UT_REGISTERS_ADDRESS       #x160)
(define UT_REGISTERS_NB            #x3)
(define UT_REGISTERS_NB_MAX        #x20)
(define UT_REGISTERS_TAB           #(#x022B #x0001 #x0064))

(define UT_INPUT_REGISTERS_ADDRESS #x108)
(define UT_INPUT_REGISTERS_NB      #x1)
(define UT_INPUT_REGISTERS_TAB     #(#x000A))

;; Raise a manual exception when this address is used for the first byte
(define UT_REGISTERS_ADDRESS_SPECIAL              #x170)
;; The response of the server will contains an invalid TID or slave
(define UT_REGISTERS_ADDRESS_INVALID_TID_OR_SLAVE #x171)
;; The server will wait for 1 second before replying to test timeout
(define UT_REGISTERS_ADDRESS_SLEEP_500_MS         #x172)
;; The server will wait for 5 ms before sending each byte
(define UT_REGISTERS_ADDRESS_BYTE_SLEEP_5_MS      #x173)

;; If the following value is used, a bad response is sent.
;   It's better to test with a lower value than UT_REGISTERS_NB_POINTS to try to raise a segfault.
(define UT_REGISTERS_NB_SPECIAL #x2)
