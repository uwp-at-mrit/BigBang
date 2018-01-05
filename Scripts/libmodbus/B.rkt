#lang racket

(require "unsafe.rkt")
(require "test.rkt")

(define-values (inregister0 inregistern) (values 0 573))
(define query (uint8-malloc MODBUS_TCP_MAX_ADU_LENGTH))
(define ctx (modbus_new_tcp #false UT_TCP_DEFAULT_PORT))
(define addr-idx (+ (modbus_get_header_length ctx) 1))
(define q-idx (+ addr-idx 2))
(modbus_set_debug ctx 1)

(define mb_mapping (modbus_mapping_new_start_address 0 0 0 51 0 0 inregister0 inregistern))

(define serverfd (modbus_tcp_listen ctx 8))
(printf "## 0.0.0.0:~a~n" UT_TCP_DEFAULT_PORT)

(with-handlers ([exn:break? (λ [e] (newline))])
  (define clientfd (modbus_tcp_accept ctx serverfd))
  (printf ">> PLC:~a~n" clientfd)

  (with-handlers ([exn? (λ [e] (fprintf (current-error-port) "~a~n" (exn-message e)))])
    (let wait-receive-reply-loop ()
      (define query-count
        (let filter-out-zero-queries ()
          (define count (modbus_receive ctx query))
          (cond [(positive? count) count]
                [else (filter-out-zero-queries)])))
      (define address (MODBUS_GET_INT16_FROM_INT8 query addr-idx))
      (define quantity (MODBUS_GET_INT16_FROM_INT8 query q-idx))
      (for ([idx (in-range address (+ address quantity))])
        (modbus-mapping-tab-input-registers-set! mb_mapping idx (random 100)))
      (modbus_reply ctx query query-count mb_mapping)
      (wait-receive-reply-loop))))

(close serverfd)
(modbus_mapping_free mb_mapping)
(modbus_close ctx)
(modbus_free ctx)
