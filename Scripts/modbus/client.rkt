#lang racket

(require rackunit)
(require "unsafe.rkt")
(require "test.rkt")

(define host "127.0.0.1")

(define ctx (modbus_new_tcp host UT_TCP_DEFAULT_PORT))
(modbus_set_debug ctx 1)

(define (check-bits tab_bits TAB_BITS i nb-points)
  (when (> nb-points 0)
    (define nb-bits (min nb-points 8))
    (define value (modbus_get_byte_from_bits tab_bits (* i 8) nb-bits))
    (define expected (vector-ref TAB_BITS i))
    (check-eq? value expected (format "FAILED (#x~a != #x~a)" (~hex value) (~hex expected)))
    (check-bits tab_bits TAB_BITS (add1 i) (- nb-points nb-bits))))

(with-handlers ([exn? (λ [e] (fprintf (current-error-port) "~a~n" (exn-message e)))])
  (printf "> No response timeout modification on connect~n")
  (let-values ([(&sec:old &usec:old) (values (box 0) (box 0))]
               [(&sec:new &usec:new) (values (box 0) (box 0))])
    (modbus_get_response_timeout ctx &sec:old &usec:old)
    (modbus_connect ctx)
    (modbus_get_response_timeout ctx &sec:new &usec:new)
    (test-case "modbus_connect"
               (check-equal? &sec:old &sec:new "Second has been modified on connect")
               (check-equal? &usec:old &usec:new "Macrosecond has been modified on connect")))

  ;; Allocate and initialize the memory to store the bits and registers
  (define tab_rp_bits (malloc/uint8 (max UT_BITS_NB UT_INPUT_BITS_NB) 'fill-zero))
  (define tab_rp_registers (malloc/uint8 (max UT_REGISTERS_NB UT_INPUT_REGISTERS_NB) 'fill-zero))

  (printf "> Read/Write Single Coil Bit~n")
  (let ([rc (modbus_write_bit ctx UT_BITS_ADDRESS 1)])
    (test-eq? "modbus_write_bit" rc 1))
  (let ([rc (modbus_read_bits ctx UT_BITS_ADDRESS 1 tab_rp_bits)]
        [bit (uint8-ref tab_rp_bits 0)])
    (test-case "modbus_read_bits"
               (check-eq? rc 1 (format "FAILED (nb points ~a)" rc))
               (check-eq? bit 1 (format "FAILED (#x~a != #x1)" (~hex bit)))))

  (printf "> Read/Write Multiple Coil Bits~n")
  (let ([tab_value (malloc/uint8 UT_BITS_NB)])
    (modbus_set_bits_from_bytes tab_value 0 UT_BITS_NB UT_BITS_TAB)
    (let ([rc (modbus_write_bits ctx UT_BITS_ADDRESS UT_BITS_NB tab_value)])
      (test-eq? "modbus_write_bits" rc UT_BITS_NB))
    (let ([rc (modbus_read_bits ctx UT_BITS_ADDRESS UT_BITS_NB tab_rp_bits)])
      (test-case "modbus_read_bits"
                 (check-eq? rc UT_BITS_NB (format "FAILED (nb points ~a)" rc))
                 (check-bits tab_rp_bits UT_BITS_TAB 0 UT_BITS_NB))))

  (printf "> DISCRETE INPUTS~n"))

(modbus_close ctx)
(modbus_free ctx)
