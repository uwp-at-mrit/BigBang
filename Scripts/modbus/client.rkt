#lang racket

(require "unsafe.rkt")

(modbus_free (modbus_new_tcp #false 502))

(modbus_get_float_abcd (vector #x0020 #xF147))
(modbus_get_float_badc (vector #x2000 #x47F1))
(modbus_get_float_cdab (vector #xF147 #x0020))
(modbus_get_float_dcba (vector #x4465 #x229A))
