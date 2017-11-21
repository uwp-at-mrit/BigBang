#lang racket

(require "tamer.rkt")

(tamer-taming-start)

(module story racket
  (require "unsafe.rkt")
  (require "test.rkt")
  (require "tamer.rkt")
  (require "format.rkt")

  (define host "127.0.0.1")
  (define ctx (modbus_new_tcp host UT_TCP_DEFAULT_PORT))

  ;; Allocate and initialize the memory to store the bits and registers
  (define tab_rp_bits (uint8-calloc (max UT_BITS_NB UT_INPUT_BITS_NB)))
  (define tab_rp_registers (uint16-calloc (max UT_REGISTERS_NB UT_INPUT_REGISTERS_NB)))

  (define rc (make-parameter #false))

  (define (check-bits tab_bits TAB_BITS nb-points [i 0])
    (when (> nb-points 0)
      (define nb-bits (min nb-points 8))
      (define given (modbus_get_byte_from_bits tab_bits (* i 8) nb-bits))
      (define expected (vector-ref TAB_BITS i))
      (check-eq? given expected (format "FAILED (#x~a != #x~a)" (~hex given) (~hex expected)))
      (check-bits tab_bits TAB_BITS (- nb-points nb-bits) (add1 i))))

  (define (check-registers tab_registers TAB_REGISTERS nb-points [i 0])
    (when (> nb-points i)
      (define given (uint16-ref tab_registers i))
      (define expected (vector-ref TAB_REGISTERS i))
      (check-eq? given expected (format "FAILED (#x~a != #x~a)" (~hex given) (~hex expected)))
      (check-registers tab_registers TAB_REGISTERS nb-points (add1 i))))

  (define (equal_dword tab_reg value)
    (and (eq? (uint16-ref tab_reg 0) (arithmetic-shift value -16))
         (eq? (uint16-ref tab_reg 1) (bitwise-and value #xFFFF))))
  
  (define-tamer-suite modbus-client "Modbus Client Unit Tests"
    #:before (λ [] (modbus_set_debug ctx 1))
    #:after (λ [] (modbus_close ctx) (modbus_free ctx))
    (let-values ([(&sec:old &usec:old) (values (box 0) (box 0))]
                 [(&sec:new &usec:new) (values (box 0) (box 0))])
      (test-suite "Connection"
                  #:before (λ [] (void (modbus_get_response_timeout ctx &sec:old &usec:old)
                                       (modbus_connect ctx)
                                       (modbus_get_response_timeout ctx &sec:new &usec:new)))
                  (test-case "No response timeout modification on connect"
                             (check-equal? &sec:old &sec:new "Second has been modified on connect")
                             (check-equal? &usec:old &usec:new "Macrosecond has been modified on connect"))))
    
    (test-suite "Coil Bits"
                (test-suite "Single Bit"
                            (test-spec "modbus_write_bit"
                                       #:before (λ [] (rc (modbus_write_bit ctx UT_BITS_ADDRESS 1)))
                                       (check-eq? (rc) 1))
                            (test-spec "modbus_read_bits"
                                       #:before (λ [] (rc (modbus_read_bits ctx UT_BITS_ADDRESS 1 tab_rp_bits)))
                                       (check-eq? (rc) 1 (format "FAILED (nb points ~a)" (rc)))
                                       (let ([bit (uint8-ref tab_rp_bits 0)])
                                         (check-eq? bit 1 (format "FAILED (#x~a != #x1)" (~hex bit))))))
                (let ([tab_value (uint8-malloc UT_BITS_NB)])
                  (test-suite "Multiple Bits"
                              #:before (λ [] (modbus_set_bits_from_bytes tab_value 0 UT_BITS_NB UT_BITS_TAB))
                              (test-spec "modbus_write_bit"
                                         #:before (λ [] (rc (modbus_write_bits ctx UT_BITS_ADDRESS UT_BITS_NB tab_value)))
                                         (check-eq? (rc) UT_BITS_NB))
                              (test-spec "modbus_read_bits"
                                         #:before (λ [] (rc (modbus_read_bits ctx UT_BITS_ADDRESS UT_BITS_NB tab_rp_bits)))
                                         (check-eq? (rc) UT_BITS_NB (format "FAILED (nb points ~a)" (rc)))
                                         (check-bits tab_rp_bits UT_BITS_TAB UT_BITS_NB)))))
    
    (test-suite "Discrete Inputs"
                (test-spec "modbus_read_input_bits"
                           #:before (λ [] (rc (modbus_read_input_bits ctx UT_INPUT_BITS_ADDRESS UT_INPUT_BITS_NB tab_rp_bits)))
                           (check-eq? (rc) UT_INPUT_BITS_NB (format "FAILED (nb points ~a)" (rc)))
                           (check-bits tab_rp_bits UT_INPUT_BITS_TAB UT_INPUT_BITS_NB)))

    (test-suite "Holding Registers"
                (test-suite "Single Register"
                            (test-spec "modbus_write_register"
                                       #:before (λ [] (rc (modbus_write_register ctx UT_REGISTERS_ADDRESS #x1234)))
                                       (check-eq? (rc) 1))
                            (test-spec "modbus_read_registers"
                                       #:before (λ [] (rc (modbus_read_registers ctx UT_REGISTERS_ADDRESS 1 tab_rp_registers)))
                                       (check-eq? (rc) 1 (format "FAILED (nb points ~a)" (rc)))
                                       (let ([register (uint16-ref tab_rp_registers 0)])
                                         (check-eq? register #x1234 (format "FAILED (#x~a != #x1234)" (~hex register))))))
                (test-suite "Many Registers"
                            (test-spec "modbus_write_registers"
                                       #:before (λ [] (rc (modbus_write_registers ctx UT_REGISTERS_ADDRESS UT_REGISTERS_NB (vector->uint16 UT_REGISTERS_TAB))))
                                       (check-eq? (rc) UT_REGISTERS_NB))
                            (test-spec "modbus_read_registers"
                                       #:before (λ [] (rc (modbus_read_registers ctx UT_REGISTERS_ADDRESS UT_REGISTERS_NB tab_rp_registers)))
                                       (check-eq? (rc) UT_REGISTERS_NB (format "FAILED (nb points ~a)" (rc)))
                                       (check-registers tab_rp_registers UT_REGISTERS_TAB UT_REGISTERS_NB))
                            (test-spec "modbus_read_registers (0)"
                                       (check-exn exn:modbus? (λ [] (modbus_read_registers ctx UT_REGISTERS_ADDRESS 0 tab_rp_registers)) "FAILED"))
                            (test-spec "modbus_write_and_read_registers"
                                       #:before (λ [] (void (uint16-memset tab_rp_registers 0 (max UT_REGISTERS_NB UT_INPUT_REGISTERS_NB))
                                                            ; Write registers to zero from tab_rp_registers and store read registers into tab_rp_registers.
                                                            ; So the read registers must set to 0, except the first one because there is an offset of 1 register on write.
                                                            (rc (modbus_write_and_read_registers ctx (+ UT_REGISTERS_ADDRESS 1) (- UT_REGISTERS_NB 1) tab_rp_registers
                                                                                                 UT_REGISTERS_ADDRESS UT_REGISTERS_NB tab_rp_registers))))
                                       (check-eq? (rc) UT_REGISTERS_NB (format "FAILED (nb points ~a != ~a)" (rc) UT_REGISTERS_NB))
                                       (let ([ADJUSTED_TAB (make-vector UT_REGISTERS_NB 0)])
                                         (vector-set! ADJUSTED_TAB 0 (vector UT_REGISTERS_TAB 0))
                                         (check-registers tab_rp_registers ADJUSTED_TAB UT_REGISTERS_NB))))
                (test-suite "Input Registers"
                            (test-spec "modbus_read_input_registers"
                                       #:before (λ [] (rc (modbus_read_input_registers ctx UT_INPUT_REGISTERS_ADDRESS UT_INPUT_REGISTERS_NB tab_rp_registers)))
                                       (check-eq? (rc) UT_INPUT_REGISTERS_NB (format "FAILED (nb points ~a)" (rc)))
                                       (check-registers tab_rp_registers UT_INPUT_REGISTERS_TAB UT_INPUT_REGISTERS_NB)))
                (test-suite "Masks (bonus)"
                            (test-spec "modbus_mask_write_register"
                                       #:before (λ [] (modbus_write_register ctx UT_REGISTERS_ADDRESS #x12))
                                       (check-not-exn (thunk (rc (modbus_mask_write_register ctx UT_REGISTERS_ADDRESS #xF2 #x25))) "FAILED"))
                            (test-spec "modbus_read_registers"
                                       #:before (λ [] (rc (modbus_read_registers ctx UT_REGISTERS_ADDRESS 1 tab_rp_registers)))
                                       (let ([register (uint16-ref tab_rp_registers 0)])
                                         (check-eq? register #x17 (format "FAILED (#x~a != #x17)" (~hex register)))))))

    (test-suite "Floats"
                (test-suite "ABCD"
                            (test-spec "modbus_set_float_abcd"
                                       #:before (λ [] (modbus_set_float_abcd UT_REAL tab_rp_registers))
                                       (check-true (equal_dword tab_rp_registers UT_IREAL_ABCD) "FAILED Set float ABCD"))
                            (test-spec "modbus_get_float_abcd"
                                       #:before (λ [] (rc (modbus_get_float_abcd tab_rp_registers)))
                                       (check-eqv? (rc) UT_REAL (format "FAILED (~a != ~a)" (rc) UT_REAL))))
                (test-suite "DCBA"
                            (test-spec "modbus_set_float_dcba"
                                       #:before (λ [] (modbus_set_float_dcba UT_REAL tab_rp_registers))
                                       (check-true (equal_dword tab_rp_registers UT_IREAL_DCBA) "FAILED Set float DCBA"))
                            (test-spec "modbus_get_float_dcba"
                                       #:before (λ [] (rc (modbus_get_float_dcba tab_rp_registers)))
                                       (check-eqv? (rc) UT_REAL (format "FAILED (~a != ~a)" (rc) UT_REAL))))
                (test-suite "BADC"
                            (test-spec "modbus_set_float_badc"
                                       #:before (λ [] (modbus_set_float_badc UT_REAL tab_rp_registers))
                                       (check-true (equal_dword tab_rp_registers UT_IREAL_BADC) "FAILED Set float BADC"))
                            (test-spec "modbus_get_float_badc"
                                       #:before (λ [] (rc (modbus_get_float_badc tab_rp_registers)))
                                       (check-eqv? (rc) UT_REAL (format "FAILED (~a != ~a)" (rc) UT_REAL))))
                (test-suite "CDAB"
                            (test-spec "modbus_set_float_cdab"
                                       #:before (λ [] (modbus_set_float_cdab UT_REAL tab_rp_registers))
                                       (check-true (equal_dword tab_rp_registers UT_IREAL_CDAB) "FAILED Set float CDAB"))
                            (test-spec "modbus_get_float_cdab"
                                       #:before (λ [] (rc (modbus_get_float_cdab tab_rp_registers)))
                                       (check-eqv? (rc) UT_REAL (format "FAILED (~a != ~a)" (rc) UT_REAL)))))

    (let ([regxiladd (+ UT_REGISTERS_ADDRESS UT_REGISTERS_NB_MAX)])
      (test-suite "Illegal Data Address"
                  ; The mapping begins at the defined addresses and ends at (address + nb_points) so these addresses are not valid
                  #:before (λ [] (saved-errno 0))
                  (test-suite "modbus_read_bits"
                              (test-exn "0" exn:modbus? (λ [] (modbus_read_bits ctx 0 1 tab_rp_bits)))
                              (test-exn "max" exn:modbus? (λ [] (modbus_read_bits ctx UT_BITS_ADDRESS (add1 UT_BITS_NB) tab_rp_bits))))
                  (test-suite "modbus_read_input_bits"
                              (test-exn "0" exn:modbus? (λ [] (modbus_read_input_bits ctx 0 1 tab_rp_bits)))
                              (test-exn "max" exn:modbus? (λ [] (modbus_read_input_bits ctx UT_INPUT_BITS_ADDRESS (add1 UT_INPUT_BITS_NB) tab_rp_bits))))
                  (test-suite "modbus_read_registers"
                              (test-exn "0" exn:modbus? (λ [] (modbus_read_registers ctx 0 1 tab_rp_registers)))
                              (test-exn "max" exn:modbus? (λ [] (modbus_read_registers ctx UT_REGISTERS_ADDRESS (add1 UT_REGISTERS_NB_MAX) tab_rp_registers))))
                  (test-suite "modbus_read_input_registers"
                              (test-exn "0" exn:modbus? (λ [] (modbus_read_input_registers ctx 0 1 tab_rp_registers)))
                              (test-exn "max" exn:modbus? (λ [] (modbus_read_input_registers ctx UT_INPUT_REGISTERS_ADDRESS (add1 UT_INPUT_REGISTERS_NB) tab_rp_registers))))
                  (test-suite "modbus_write_bit"
                              (test-exn "0" exn:modbus? (λ [] (modbus_write_bit ctx 0 1)))
                              (test-exn "max" exn:modbus? (λ [] (modbus_write_bit ctx (+ UT_BITS_ADDRESS UT_BITS_NB) 1))))
                  (test-suite "modbus_write_bits"
                              (test-exn "0" exn:modbus? (λ [] (modbus_write_bits ctx 0 1 tab_rp_bits)))
                              (test-exn "max" exn:modbus? (λ [] (modbus_write_bits ctx (+ UT_BITS_ADDRESS UT_BITS_NB) UT_BITS_NB tab_rp_bits))))
                  (test-suite "modbus_write_register"
                              (test-exn "0" exn:modbus? (λ [] (modbus_write_register ctx 0 (uint16-ref tab_rp_registers 0))))
                              (test-exn "max" exn:modbus? (λ [] (modbus_write_register ctx (+ UT_BITS_ADDRESS UT_BITS_NB) (uint16-ref tab_rp_registers 0)))))
                  (test-suite "modbus_write_registers"
                              (test-exn "0" exn:modbus? (λ [] (modbus_write_registers ctx 0 1 tab_rp_registers)))
                              (test-exn "max" exn:modbus? (λ [] (modbus_write_registers ctx regxiladd UT_REGISTERS_NB tab_rp_registers))))
                  (test-suite "modbus_mask_write_register"
                              (test-exn "0" exn:modbus? (λ [] (modbus_mask_write_register ctx 0 #xF2 #x25)))
                              (test-exn "max" exn:modbus? (λ [] (modbus_mask_write_register ctx regxiladd #xF2 #x25))))
                  (test-suite "modbus_write_and_read_registers"
                              (test-exn "0" exn:modbus? (λ [] (modbus_write_and_read_registers ctx 0 1 tab_rp_registers 0 1 tab_rp_registers)))
                              (test-exn "max" exn:modbus? (λ [] (modbus_write_and_read_registers ctx regxiladd UT_REGISTERS_NB tab_rp_registers regxiladd UT_REGISTERS_NB tab_rp_registers))))))

    (test-suite "Too Many Data Error"
                (test-exn "modbus_read_bits" exn:modbus? (λ [] (modbus_read_bits ctx UT_BITS_ADDRESS (add1 MODBUS_MAX_READ_BITS) tab_rp_bits)))
                (test-exn "modbus_read_input_bits" exn:modbus? (λ [] (modbus_read_input_bits ctx UT_INPUT_BITS_ADDRESS (add1 MODBUS_MAX_READ_BITS) tab_rp_bits)))
                (test-exn "modbus_read_registers" exn:modbus? (λ [] (modbus_read_registers ctx UT_REGISTERS_ADDRESS (add1 MODBUS_MAX_READ_REGISTERS) tab_rp_registers)))
                (test-exn "modbus_read_input_registers" exn:modbus? (λ [] (modbus_read_input_registers ctx UT_INPUT_REGISTERS_ADDRESS (add1 MODBUS_MAX_READ_REGISTERS) tab_rp_registers)))
                (test-exn "modbus_write_bits" exn:modbus? (λ [] (modbus_read_bits ctx UT_BITS_ADDRESS (add1 MODBUS_MAX_WRITE_BITS) tab_rp_bits)))
                (test-exn "modbus_write_registers" exn:modbus? (λ [] (modbus_read_registers ctx UT_REGISTERS_ADDRESS (add1 MODBUS_MAX_WRITE_REGISTERS) tab_rp_registers))))))
  