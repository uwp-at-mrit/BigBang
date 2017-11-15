#lang racket

(provide (all-defined-out))
(provide (all-from-out "constants.rkt"))

(require ffi/unsafe)
(require ffi/unsafe/define)
(require ffi/unsafe/alloc)

(require "constants.rkt")

(define-syntax (_fun* stx)
  (syntax-case stx [->]
    [(_ argl ... #:-> ctype #:N vlen)
     #'(_fun _modbus_t* argl ... [dest : (_vector o ctype (* vlen (ctype-sizeof ctype)))] -> [retcode : _int] -> (safe-array dest retcode))]
    [(_ argl ...) #'(_fun _modbus_t* argl ...)]))

(define-ffi-definer define-modbus (ffi-lib "libmodbus"))

(define safe-array
  (lambda [src retcode]
    (vector-take src retcode)))

(define _modbus_t* (_cpointer 'modbus_t))
(define _modbus_mapping_t* (_cpointer 'modbus_mapping_t))

(define-modbus modbus_strerror (_fun _int -> _string))

(define-modbus modbus_mapping_free (_fun _modbus_mapping_t* -> _void) #:wrap (deallocator))
(define-modbus modbus_mapping_new
  (_fun [nb_bits : _int]
        [nb_input_bits : _int]
        [nb_registers : _int]
        [nb_input_registers : _int]
        -> _modbus_mapping_t*)
  #:wrap (allocator modbus_mapping_free))

(define-modbus modbus_free (_fun* -> _void) #:wrap (deallocator))
(define-modbus modbus_new_tcp (_fun _string _short -> _modbus_t*) #:wrap (allocator modbus_free))
(define-modbus modbus_new_tcp_pi (_fun _string _string -> _modbus_t*) #:wrap (allocator modbus_free))

(define-modbus modbus_connect (_fun* -> _int))
(define-modbus modbus_flush (_fun* -> _int))
(define-modbus modbus_close (_fun* -> _void))

(define-modbus modbus_tcp_listen (_fun* [nb_connecttion : _int] -> _int))
(define-modbus modbus_tcp_pi_listen (_fun* [nb_connecttion : _int] -> _int))
(define-modbus modbus_tcp_accept (_fun* [s : (_ptr o _int)] -> _int))
(define-modbus modbus_tcp_pi_accept (_fun* [s : (_ptr o _int)] -> _int))
(define-modbus modbus_receive (_fun* [req : (_ptr o _uint8)] -> _int))

(define-modbus modbus_report_slave_id (_fun* [max_dest : _int] [dest : (_vector o _uint8 max_dest)] -> _int -> dest))
(define-modbus modbus_write_bit (_fun* [addr : _int] [status : _int] -> _int))
(define-modbus modbus_write_bits (_fun* [addr : _int] [src : (_vector i _uint8)] -> _int))
(define-modbus modbus_write_register (_fun* [addr : _int] [value : _int] -> _int))
(define-modbus modbus_write_registers (_fun* [addr : _int] [src : (_vector i _uint16)] -> _int))
(define-modbus modbus_read_bits (_fun* [addr : _int] [nb : _int] #:-> _uint8 #:N nb))
(define-modbus modbus_read_input_bits (_fun* [addr : _int] [nb : _int] #:-> _uint8 #:N nb))
(define-modbus modbus_read_registers (_fun* [addr : _int] [nb : _int] #:-> _uint16 #:N nb))
(define-modbus modbus_read_input_registers (_fun* [addr : _int] [nb : _int] #:-> _uint16 #:N nb))
(define-modbus modbus_write_and_read_registers (_fun* [waddr : _int] [wnb : _int] [src : (_vector i _uint16)] [raddr : _int] [rnb : _int] #:-> _uint16 #:N rnb))

(define-modbus modbus_send_raw_request (_fun* [raw_req : (_vector i _uint8)] [raw_length : _int = (vector-length raw_req)] -> _int))
(define-modbus modbus_receive_confirmation (_fun* #:-> _uint8 #:N MODBUS_MAX_ADU_LENGTH))

(define-modbus modbus_set_socket (_fun* [s : _int] -> _int))
(define-modbus modbus_get_socket (_fun* -> [s : _int]))
(define-modbus modbus_get_byte_timeout (_fun* [s : (_ptr o _uint32)] [us : (_ptr o _uint32)] -> _int -> (values s us)))
(define-modbus modbus_set_byte_timeout (_fun* [s : _uint32] [us : _uint32] -> _int))
(define-modbus modbus_get_response_timeout (_fun* [s : (_ptr o _uint32)] [us : (_ptr o _uint32)] -> _int -> (values s us)))
(define-modbus modbus_set_response_timeout (_fun* [s : _uint32] [us : _uint32] -> _int))
(define-modbus modbus_set_slave (_fun* [slave : _int] -> _int))
(define-modbus modbus_set_debug (_fun* [flag : _int] -> _int))
(define-modbus modbus_get_header_length (_fun* -> _int))

(define-modbus modbus_set_bits_from_byte (_fun [dest : (_ptr o _uint8)] [index : _int] [value : _uint8] -> _void -> dest))
(define-modbus modbus_set_bits_from_bytes (_fun [dest : (_ptr o _uint8)] [index : _int] [nb_bits : _uint] [tab_byte : (_vector i _uint8)] -> _void -> dest))
(define-modbus modbus_get_byte_from_bits (_fun [src : (_vector i _uint8)] [index : _int] [nb_bits : _uint] -> _uint8))

(define-modbus modbus_get_float_abcd (_fun [src : (_vector i _uint16)] -> _float))
(define-modbus modbus_set_float_abcd (_fun _float [dest : (_vector o _uint16 2)] -> _void -> dest))
(define-modbus modbus_get_float_badc (_fun [src : (_vector i _uint16)] -> _float))
(define-modbus modbus_set_float_badc (_fun _float [dest : (_vector o _uint16 2)] -> _void -> dest))
(define-modbus modbus_get_float_cdab (_fun [src : (_vector i _uint16)] -> _float))
(define-modbus modbus_set_float_cdab (_fun _float [dest : (_vector o _uint16 2)] -> _void -> dest))
(define-modbus modbus_get_float_dcba (_fun [src : (_vector i _uint16)] -> _float))
(define-modbus modbus_set_float_dcba (_fun _float [dest : (_vector o _uint16 2)] -> _void -> dest))
