#lang racket

(require "unsafe.rkt")
(require "test.rkt")

(define query (uint8-malloc MODBUS_TCP_MAX_ADU_LENGTH))
(define ctx (modbus_new_tcp #false UT_TCP_DEFAULT_PORT))
(define header_length (modbus_get_header_length ctx))
(modbus_set_debug ctx 1)

(define mb_mapping
  (modbus_mapping_new_start_address UT_BITS_ADDRESS UT_BITS_NB
                                    UT_INPUT_BITS_ADDRESS UT_INPUT_BITS_NB
                                    UT_REGISTERS_ADDRESS UT_REGISTERS_NB_MAX
                                    UT_INPUT_REGISTERS_ADDRESS UT_INPUT_REGISTERS_NB))


(define modbus-special-reply
  (lambda [ctx query]
    ; Special server behavior to test client
    (when (= (uint8-ref query header_length) MODBUS_FC_READ_HOLDING_REGISTERS)
      (define idx+3 (+ header_length 3))
      (define idx+1 (+ header_length 1))
      (cond [(= (MODBUS_GET_INT16_FROM_INT8 query idx+3) UT_REGISTERS_NB_SPECIAL)
             (printf "> Set an incorrect number of values~n")
             (MODBUS_SET_INT16_TO_INT8 query idx+3 (sub1 UT_REGISTERS_NB_SPECIAL))]
            [(= (MODBUS_GET_INT16_FROM_INT8 query idx+1) UT_REGISTERS_ADDRESS_SPECIAL)
             (printf "> Reply to this special register address by an exception~n")
             (modbus_reply_exception ctx query MODBUS_EXCEPTION_SLAVE_OR_SERVER_BUSY)]
            [(= (MODBUS_GET_INT16_FROM_INT8 query idx+1) UT_REGISTERS_ADDRESS_INVALID_TID_OR_SLAVE)
             (printf "> Reply with an invalid TID or slave~n")
             (modbus_send_raw_request ctx #(#xFF #x03 #x02 #x00 #x00))]
            [(= (MODBUS_GET_INT16_FROM_INT8 query idx+1) UT_REGISTERS_ADDRESS_SLEEP_500_MS)
             (printf "> Sleep 0.5s before replying~n")
             (sync/timeout/enable-break 0.5 never-evt)]
            [(= (MODBUS_GET_INT16_FROM_INT8 query idx+1) UT_REGISTERS_ADDRESS_BYTE_SLEEP_5_MS)
             (with-handlers ([exn:fail? (λ [e] (fprintf (current-error-port) "> ~a~n" (exn-message e)))])
               ; Catch the reply and send reply byte a byte
               (define w_s (modbus_get_socket ctx))
               (define req (make-bytes #x00 #x1C #x00 #x00 #x05 #xFF #x03 #x02 #x00 #x00))
               
               ; Copy TID
               (bytes-set! req 1 (uint8-ref query 1))
               (for ([octet (in-bytes req)])
                 (printf "(~a~a)" (if (<= octet #xF) "0" "") (string-upcase (number->string octet 16)))
                 (sync/timeout/enable-break 0.005 never-evt)
                 (send w_s (string (integer->char octet)) #x0))
               (newline))]))))

;; Examples from PI_MODBUS_300.pdf, Only the read-only input values are assigned.

; Initialize input values that's can be only done server side.
(modbus_set_bits_from_bytes (modbus_mapping-tab_input_bits mb_mapping) 0 UT_INPUT_BITS_NB UT_INPUT_BITS_TAB)

; Initialize values of INPUT REGISTERS
(for ([i (in-range UT_INPUT_REGISTERS_NB)])
  (modbus-mapping-tab-input-registers-set! mb_mapping i (vector-ref UT_INPUT_REGISTERS_TAB i)))

(define serverfd (modbus_tcp_listen ctx 1))
(printf "## localhost:~a~n" UT_TCP_DEFAULT_PORT)

(define clientfd (modbus_tcp_accept ctx serverfd))
(printf ">> PLC:~a~n" clientfd)

(with-handlers ([exn? (λ [e] (fprintf (current-error-port) "~a~n" (exn-message e)))])
  (let wait-receive-reply-loop ()
    (define query-count
      (let filter-out-zero-queries ()
        (define count (modbus_receive ctx query))
        (cond [(positive? count) count]
              [else (filter-out-zero-queries)])))
    (modbus-special-reply ctx query)
    (modbus_reply ctx query query-count mb_mapping)
    (wait-receive-reply-loop)))

(close serverfd)
(modbus_mapping_free mb_mapping)
(modbus_close ctx)
(modbus_free ctx)
