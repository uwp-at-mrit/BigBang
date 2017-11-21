#lang racket

(provide (all-defined-out))
(provide (all-from-out ffi/unsafe))
(provide (all-from-out "constants.rkt"))

(require ffi/unsafe)
(require ffi/unsafe/define)
(require ffi/unsafe/alloc)

(require "constants.rkt")

(define-syntax (define-modbus* stx)
  (syntax-case stx [_fun ->]
    [(_ func (_fun argl ... #:?> ctype return-expr) rest ...)
     #'(define-modbus func
         (_fun #:save-errno 'posix _modbus_t* argl ...
               -> [retcode : ctype]
               -> (begin (on-error-break 'func retcode -1) return-expr))
         rest ...)]
    [(_ func (_fun argl ... #:?> ctype) rest ...)
     #'(define-modbus func
         (_fun #:save-errno 'posix _modbus_t* argl ...
               -> [retcode : ctype]
               -> (on-error-break 'func retcode -1))
         rest ...)]
    [(_ func (_fun argl ... #:*> ctype) rest ...)
     #'(define-modbus func
         (_fun #:save-errno 'posix argl ...
               -> [retcode : ctype]
               -> (on-error-break 'func retcode #false))
         rest ...)]
    [(_ func (_fun argl ...) rest ...)
     #'(define-modbus func
         (_fun #:save-errno 'posix _modbus_t* argl ...)
         rest ...)]))

(define on-error-break
  (lambda [func retcode errcode]
    (when (eq? retcode errcode)
      (error func "~a" (modbus_strerror (saved-errno))))
    retcode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-ffi-definer define-modbus (ffi-lib "libmodbus" #:global? #true))
(define-ffi-definer define-posix (ffi-lib #false #:global? #true))

(define _modbus_t* (_cpointer 'modbus_t))
(define _uint8* (_cpointer 'uint8))
(define _uint16* (_cpointer 'uint16))

(define-cstruct _modbus_mapping
  ([nb_bits               _int]
   [start_bits            _int]
   [nb_input_bits         _int]
   [start_input_bits      _int]
   [nb_input_registers    _int]
   [start_input_registers _int]
   [nb_registers          _int]
   [start_registers       _int]
   [tab_bits              _uint8*]
   [tab_input_bits        _uint8*]
   [tab_input_registers   _uint16*]
   [tab_registers         _uint16*]))

(define _modbus_mapping_t* _modbus_mapping-pointer)

(define-values (modbus-mapping-tab-bits modbus-mapping-tab-input-bits modbus-mapping-tab-registers modbus-mapping-tab-input-registers)
  (let ([cast* (λ [ptr type n] (cast ptr _pointer (_vector o type n)))])
    (values (λ [mb] (cast* (modbus_mapping-tab_bits mb) _uint8 (modbus_mapping-nb_bits mb)))
            (λ [mb] (cast* (modbus_mapping-tab_input_bits mb) _uint8 (modbus_mapping-nb_bits mb)))
            (λ [mb] (cast* (modbus_mapping-tab_registers mb) _uint16 (modbus_mapping-nb_registers mb)))
            (λ [mb] (cast* (modbus_mapping-tab_input_registers mb) _uint16 (modbus_mapping-nb_input_registers mb))))))

(define-values (modbus-mapping-tab-bits-ref modbus-mapping-tab-input-bits-ref modbus-mapping-tab-registers-ref modbus-mapping-tab-input-registers-ref)
  (let ([ptr-ref* (λ [ptr type idx v] (ptr-ref ptr type idx))])
    (values (λ [mb idx] (ptr-ref* (modbus_mapping-tab_bits mb) _uint8 idx))
            (λ [mb idx] (ptr-ref* (modbus_mapping-tab_input_bits mb) _uint8 idx))
            (λ [mb idx] (ptr-ref* (modbus_mapping-tab_registers mb) _uint16 idx))
            (λ [mb idx] (ptr-ref* (modbus_mapping-tab_input_registers mb) _uint16 idx)))))

(define-values (modbus-mapping-tab-bits-set! modbus-mapping-tab-input-bits-set! modbus-mapping-tab-registers-set! modbus-mapping-tab-input-registers-set!)
  (let ([ptr-set* (λ [ptr type idx v] (ptr-set! ptr type idx v))])
    (values (λ [mb idx v] (ptr-set* (modbus_mapping-tab_bits mb) _uint8 idx v))
            (λ [mb idx v] (ptr-set* (modbus_mapping-tab_input_bits mb) _uint8 idx v))
            (λ [mb idx v] (ptr-set* (modbus_mapping-tab_registers mb) _uint16 idx v))
            (λ [mb idx v] (ptr-set* (modbus_mapping-tab_input_registers mb) _uint16 idx v)))))

(define vector->uint8
  (lambda [src]
    (define dest (uint8-calloc (vector-length src)))
    (for ([v (in-vector src)] [i (in-naturals)]) (ptr-set! dest _uint8 i v))
    dest))

(define vector->uint16
  (lambda [src]
    (define dest (uint16-calloc (vector-length src)))
    (for ([v (in-vector src)] [i (in-naturals)]) (ptr-set! dest _uint16 i v))
    dest))

(define uint8-ref (λ [a idx] (ptr-ref a _uint8 idx)))
(define uint16-ref (λ [a idx] (ptr-ref a _uint16 idx)))

(define uint8-memset (λ [ptr c len] (memset ptr c len _uint8)))
(define uint16-memset (λ [ptr c len] (memset ptr c len _uint16)))

(define uint8-malloc (λ [n] (cast (malloc _uint8 n) _pointer _uint8*)))
(define uint16-malloc (λ [n] (cast (malloc _uint16 n) _pointer _uint16*)))

(define uint8-calloc (λ [n] (let ([ptr (uint8-malloc n)]) (uint8-memset ptr 0 n) ptr)))
(define uint16-calloc (λ [n] (let ([ptr (uint16-malloc n)]) (uint16-memset ptr 0 n) ptr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-modbus modbus_free (_fun _modbus_t* -> _void) #:wrap (deallocator))
(define-modbus modbus_mapping_free (_fun _modbus_mapping_t* -> _void) #:wrap (deallocator))
(define-modbus modbus_strerror (_fun _int -> _string))

(define (MODBUS_GET_HIGH_BYTE data)
  (bitwise-and (arithmetic-shift data -8) #xFF))

(define (MODBUS_GET_LOW_BYTE data)
  (bitwise-and data #xFF))

(define (MODBUS_GET_INT64_FROM_INT16 src idx)
  (+ (arithmetic-shift (ptr-ref src _int16 (+ idx 0)) 48)
     (arithmetic-shift (ptr-ref src _int16 (+ idx 1)) 32)
     (arithmetic-shift (ptr-ref src _int16 (+ idx 2)) 16)
     (ptr-ref src _int16 (+ idx 3))))

(define (MODBUS_GET_INT32_FROM_INT16 src idx)
  (+ (arithmetic-shift (ptr-ref src _int16 (+ idx 0)) 16)
     (ptr-ref src _int16 (+ idx 1))))

(define (MODBUS_GET_INT16_FROM_INT8 src idx)
  (+ (arithmetic-shift (ptr-ref src _int8 (+ idx 0)) 8)
     (ptr-ref src _int8 (+ idx 1))))

(define (MODBUS_SET_INT16_TO_INT8 dest idx val)
  (ptr-set! dest _int8 (+ idx 0) (arithmetic-shift val -8))
  (ptr-set! dest _int8 (+ idx 1) (bitwise-and val #xFF)))

(define (MODBUS_SET_INT32_TO_INT16 dest idx val)
  (ptr-set! dest _int16 (+ idx 0) (arithmetic-shift val -16))
  (ptr-set! dest _int16 (+ idx 1) val))

(define (MODBUS_SET_INT64_TO_INT16 dest idx val)
  (ptr-set! dest _int16 (+ idx 0) (arithmetic-shift val -48))
  (ptr-set! dest _int16 (+ idx 1) (arithmetic-shift val -32))
  (ptr-set! dest _int16 (+ idx 2) (arithmetic-shift val -16))
  (ptr-set! dest _int16 (+ idx 3) val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-modbus modbus_mapping_new_start_address
  (_fun [start_bits : _int]
        [nb_bits : _int]
        [start_input_bits : _int]
        [nb_input_bits : _int]
        [start_registers : _int]
        [nb_registers : _int]
        [start_input_registers : _int]
        [nb_input_registers : _int]
        -> _modbus_mapping_t*)
  #:wrap (allocator modbus_mapping_free))

(define-modbus modbus_mapping_new
  (_fun [nb_bits : _int]
        [nb_input_bits : _int]
        [nb_registers : _int]
        [nb_input_registers : _int]
        -> _modbus_mapping_t*)
  #:wrap (allocator modbus_mapping_free))

(define-modbus* modbus_new_tcp (_fun _string _short #:*> _modbus_t*) #:wrap (allocator modbus_free))
(define-modbus* modbus_new_tcp_pi (_fun _string _string #:*> _modbus_t*) #:wrap (allocator modbus_free))

(define-modbus* modbus_connect (_fun #:?> _int))
(define-modbus* modbus_flush (_fun #:?> _int))
(define-modbus* modbus_close (_fun -> _void))

(define-modbus* modbus_tcp_listen (_fun [nb_connecttion : _int] #:?> _int))
(define-modbus* modbus_tcp_pi_listen (_fun [nb_connecttion : _int] #:?> _int))
(define-modbus* modbus_tcp_accept (_fun [s : (_ptr i _int)] #:?> _int))
(define-modbus* modbus_tcp_pi_accept (_fun [s : (_ptr i _int)] #:?> _int))
(define-modbus* modbus_receive (_fun [req : _uint8*] #:?> _int))
(define-modbus* modbus_reply (_fun [req : _uint8*] [req_length : _int] _modbus_mapping_t* #:?> _int))
(define-modbus* modbus_reply_exception (_fun [req : _uint8*] [exception_code : _uint] #:?> _int))

(define-modbus* modbus_report_slave_id (_fun [max_dest : _int] [dest : (_vector o _uint8 max_dest)] #:?> _int dest))
(define-modbus* modbus_write_bit (_fun [addr : _int] [status : _int] #:?> _int))
(define-modbus* modbus_write_bits (_fun [addr : _int] [nb : _int] [src : _uint8*] #:?> _int))
(define-modbus* modbus_write_register (_fun [addr : _int] [value : _int] #:?> _int))
(define-modbus* modbus_write_registers (_fun [addr : _int] [nb : _int] [src : _uint16*] #:?> _int))
(define-modbus* modbus_mask_write_register (_fun [addr : _int] [and_mask : _int] [or_mask : _int] #:?> _int))
(define-modbus* modbus_read_bits (_fun [addr : _int] [nb : _int] [dest : _uint8*] #:?> _int))
(define-modbus* modbus_read_input_bits (_fun [addr : _int] [nb : _int] [dest : _uint8*] #:?> _int))
(define-modbus* modbus_read_registers (_fun [addr : _int] [nb : _int] [dest : _uint16*] #:?> _int))
(define-modbus* modbus_read_input_registers (_fun [addr : _int] [nb : _int] [dest : _uint16*] #:?> _int))
(define-modbus* modbus_write_and_read_registers (_fun [waddr : _int] [wnb : _int] [src : _uint16*] [raddr : _int] [rnb : _int] [dest : _uint16*] #:?> _int))

(define-modbus* modbus_send_raw_request (_fun [raw_req : (_vector i _uint8)] [raw_length : _size = (vector-length raw_req)] #:?> _int))
(define-modbus* modbus_receive_confirmation (_fun [rsp : _uint8*] #:?> _int))

(define-modbus* modbus_set_socket (_fun [s : _int] #:?> _int (void)))
(define-modbus* modbus_get_socket (_fun #:?> _int))
(define-modbus* modbus_get_byte_timeout (_fun [s : (_box _uint32)] [us : (_box _uint32)] #:?> _int (void)))
(define-modbus* modbus_set_byte_timeout (_fun [s : _uint32] [us : _uint32] #:?> _int (void)))
(define-modbus* modbus_get_response_timeout (_fun [s : (_box _uint32)] [us : (_box _uint32)] #:?> _int (void)))
(define-modbus* modbus_set_response_timeout (_fun [s : _uint32] [us : _uint32] #:?> _int (void)))
(define-modbus* modbus_set_slave (_fun [slave : _int] #:?> _int (void)))
(define-modbus* modbus_set_debug (_fun [flag : _int] #:?> _int (void)))
(define-modbus* modbus_get_header_length (_fun -> _int))

(define-modbus modbus_set_bits_from_byte (_fun [dest : _uint8*] [index : _int] [value : _uint8] -> _void))
(define-modbus modbus_set_bits_from_bytes (_fun [dest : _uint8*] [index : _int] [nb_bits : _uint] [tab_byte : (_vector i _uint8)] -> _void))
(define-modbus modbus_get_byte_from_bits (_fun [src : _uint8*] [index : _int] [nb_bits : _uint] -> _uint8))

(define-modbus modbus_get_float_abcd (_fun [src : _uint16*] -> _float))
(define-modbus modbus_set_float_abcd (_fun _float [dest : _uint16*] -> _void))
(define-modbus modbus_get_float_badc (_fun [src : _uint16*] -> _float))
(define-modbus modbus_set_float_badc (_fun _float [dest : _uint16*] -> _void))
(define-modbus modbus_get_float_cdab (_fun [src : _uint16*] -> _float))
(define-modbus modbus_set_float_cdab (_fun _float [dest : _uint16*] -> _void))
(define-modbus modbus_get_float_dcba (_fun [src : _uint16*] -> _float))
(define-modbus modbus_set_float_dcba (_fun _float [dest : _uint16*] -> _void))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-posix close (_fun [socketfd : _int] -> _int -> (void)))
(define-posix send (_fun #:save-errno 'posix [socketfd : _int] [buffer : _string] [length : _size = (string-length buffer)] [flag : _int] -> _int))
