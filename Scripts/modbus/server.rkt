#lang racket

(require "unsafe.rkt")
(require "test.rkt")

(define query (malloc/uint8 MODBUS_TCP_MAX_ADU_LENGTH))
(define ctx (modbus_new_tcp "127.0.0.1" 1502))
(define header_length (modbus_get_header_length ctx))
(modbus_set_debug ctx 1)

(define mb_mapping
  (modbus_mapping_new_start_address UT_BITS_ADDRESS UT_BITS_NB
                                    UT_INPUT_BITS_ADDRESS UT_INPUT_BITS_NB
                                    UT_REGISTERS_ADDRESS UT_REGISTERS_NB_MAX
                                    UT_INPUT_REGISTERS_ADDRESS UT_INPUT_REGISTERS_NB))

;; Examples from PI_MODBUS_300.pdf, Only the read-only input values are assigned.

; Initialize input values that's can be only done server side.
(modbus_set_bits_from_bytes (modbus_mapping-tab_input_bits mb_mapping) 0 UT_INPUT_BITS_NB UT_INPUT_BITS_TAB)

; Initialize values of INPUT REGISTERS
(for ([i (in-range UT_INPUT_REGISTERS_NB)])
  (modbus-mapping-tab-input-registers-set! mb_mapping i (vector-ref UT_INPUT_REGISTERS_TAB i)))

(with-handlers ([exn? (λ [e] (fprintf (current-error-port) "~a~n" (exn-message e)))])
  (define serverfd (modbus_tcp_listen ctx 1))
  (modbus_tcp_accept ctx serverfd)
  (modbus_receive ctx query))

#|
    for (;;) {
        do {
            rc = modbus_receive(ctx, query);
            /* Filtered queries return 0 */
        } while (rc == 0);

        /* The connection is not closed on errors which require on reply such as
           bad CRC in RTU. */
        if (rc == -1 && errno != EMBBADCRC) {
            /* Quit */
            break;
        }

        /* Special server behavior to test client */
        if (query[header_length] == 0x03) {
            /* Read holding registers */

            if (MODBUS_GET_INT16_FROM_INT8(query, header_length + 3)
                == UT_REGISTERS_NB_SPECIAL) {
                printf("Set an incorrect number of values\n");
                MODBUS_SET_INT16_TO_INT8(query, header_length + 3,
                                         UT_REGISTERS_NB_SPECIAL - 1);
            } else if (MODBUS_GET_INT16_FROM_INT8(query, header_length + 1)
                       == UT_REGISTERS_ADDRESS_SPECIAL) {
                printf("Reply to this special register address by an exception\n");
                modbus_reply_exception(ctx, query,
                                       MODBUS_EXCEPTION_SLAVE_OR_SERVER_BUSY);
                continue;
            } else if (MODBUS_GET_INT16_FROM_INT8(query, header_length + 1)
                       == UT_REGISTERS_ADDRESS_INVALID_TID_OR_SLAVE) {
                const int RAW_REQ_LENGTH = 5;
                uint8_t raw_req[] = {
                    (use_backend == RTU) ? INVALID_SERVER_ID : 0xFF,
                    0x03,
                    0x02, 0x00, 0x00
                };

                printf("Reply with an invalid TID or slave\n");
                modbus_send_raw_request(ctx, raw_req, RAW_REQ_LENGTH * sizeof(uint8_t));
                continue;
            } else if (MODBUS_GET_INT16_FROM_INT8(query, header_length + 1)
                       == UT_REGISTERS_ADDRESS_SLEEP_500_MS) {
                printf("Sleep 0.5 s before replying\n");
                usleep(500000);
            } else if (MODBUS_GET_INT16_FROM_INT8(query, header_length + 1)
                       == UT_REGISTERS_ADDRESS_BYTE_SLEEP_5_MS) {
                /* Test low level only available in TCP mode */
                /* Catch the reply and send reply byte a byte */
                uint8_t req[] = "\x00\x1C\x00\x00\x00\x05\xFF\x03\x02\x00\x00";
                int req_length = 11;
                int w_s = modbus_get_socket(ctx);
                if (w_s == -1) {
                    fprintf(stderr, "Unable to get a valid socket in special test\n");
                    continue;
                }

                /* Copy TID */
                req[1] = query[1];
                for (i=0; i < req_length; i++) {
                    printf("(%.2X)", req[i]);
                    usleep(5000);
                    rc = send(w_s, (const char*)(req + i), 1, MSG_NOSIGNAL);
                    if (rc == -1) {
                        break;
                    }
                }
                continue;
            }
        }

        rc = modbus_reply(ctx, query, rc, mb_mapping);
        if (rc == -1) {
            break;
        }
    }
|#

(modbus_mapping_free mb_mapping)
(modbus_close ctx)
(modbus_free ctx)
