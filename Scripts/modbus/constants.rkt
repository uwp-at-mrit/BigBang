#lang racket

(provide (all-defined-out))

;;; https://github.com/stephane/libmodbus/blob/master/src/modbus.h

;; Modbus function codes
(define MODBUS_FC_READ_COILS                #x01)
(define MODBUS_FC_READ_DISCRETE_INPUTS      #x02)
(define MODBUS_FC_READ_HOLDING_REGISTERS    #x03)
(define MODBUS_FC_READ_INPUT_REGISTERS      #x04)
(define MODBUS_FC_WRITE_SINGLE_COIL         #x05)
(define MODBUS_FC_WRITE_SINGLE_REGISTER     #x06)
(define MODBUS_FC_READ_EXCEPTION_STATUS     #x07)
(define MODBUS_FC_WRITE_MULTIPLE_COILS      #x0F)
(define MODBUS_FC_WRITE_MULTIPLE_REGISTERS  #x10)
(define MODBUS_FC_REPORT_SLAVE_ID           #x11)
(define MODBUS_FC_MASK_WRITE_REGISTER       #x16)
(define MODBUS_FC_WRITE_AND_READ_REGISTERS  #x17)

(define MODBUS_BROADCAST_ADDRESS 0)

;; Modbus_Application_Protocol_V1_1b.pdf (chapter 6 section 1 page 12)
;; Quantity of Coils to read (2 bytes): 1 to 2000 (#x7D0)
;; (chapter 6 section 11 page 29)
;; Quantity of Coils to write (2 bytes): 1 to 1968 (#x7B0)

(define MODBUS_MAX_READ_BITS              2000)
(define MODBUS_MAX_WRITE_BITS             1968)

;; Modbus_Application_Protocol_V1_1b.pdf (chapter 6 section 3 page 15)
;; Quantity of Registers to read (2 bytes): 1 to 125 (#x7D)
;; (chapter 6 section 12 page 31)
;; Quantity of Registers to write (2 bytes) 1 to 123 (#x7B)
;; (chapter 6 section 17 page 38)
;; Quantity of Registers to write in R/W registers (2 bytes) 1 to 121 (#x79)
(define MODBUS_MAX_READ_REGISTERS          125)
(define MODBUS_MAX_WRITE_REGISTERS         123)
(define MODBUS_MAX_WR_WRITE_REGISTERS      121)
(define MODBUS_MAX_WR_READ_REGISTERS       125)

;; The size of the MODBUS PDU is limited by the size constraint inherited from
;; the first MODBUS implementation on Serial Line network (max. RS485 ADU = 256
;; bytes). Therefore, MODBUS PDU for serial line communication = 256 - Server
;; address (1 byte) - CRC (2 bytes) = 253 bytes.
(define MODBUS_MAX_PDU_LENGTH              253)

;; Consequently:
;; - RTU MODBUS ADU = 253 bytes + Server address (1 byte) + CRC (2 bytes) = 256 bytes.
;; - TCP MODBUS ADU = 253 bytes + MBAP (7 bytes) = 260 bytes.
;; so the maximum of both backend in 260 bytes. This size can used to allocate
;; an array of bytes to store responses and it will be compatible with the two
;; backends.
(define MODBUS_MAX_ADU_LENGTH              260)

;; Random number to avoid errno conflicts
(define MODBUS_ENOBASE 112345678)

;; Protocol exceptions
(define MODBUS_EXCEPTION_ILLEGAL_FUNCTION        #x01)
(define MODBUS_EXCEPTION_ILLEGAL_DATA_ADDRESS    #x02)
(define MODBUS_EXCEPTION_ILLEGAL_DATA_VALUE      #x03)
(define MODBUS_EXCEPTION_SLAVE_OR_SERVER_FAILURE #x04)
(define MODBUS_EXCEPTION_ACKNOWLEDGE             #x05)
(define MODBUS_EXCEPTION_SLAVE_OR_SERVER_BUSY    #x06)
(define MODBUS_EXCEPTION_NEGATIVE_ACKNOWLEDGE    #x07)
(define MODBUS_EXCEPTION_MEMORY_PARITY           #x08)
(define MODBUS_EXCEPTION_NOT_DEFINED             #x09)
(define MODBUS_EXCEPTION_GATEWAY_PATH            #x0A)
(define MODBUS_EXCEPTION_GATEWAY_TARGET          #x0B)
(define MODBUS_EXCEPTION_MAX                     #x0C)

(define EMBXILFUN   (+ MODBUS_ENOBASE MODBUS_EXCEPTION_ILLEGAL_FUNCTION))
(define EMBXILADD   (+ MODBUS_ENOBASE MODBUS_EXCEPTION_ILLEGAL_DATA_ADDRESS))
(define EMBXILVAL   (+ MODBUS_ENOBASE MODBUS_EXCEPTION_ILLEGAL_DATA_VALUE))
(define EMBXSFAIL   (+ MODBUS_ENOBASE MODBUS_EXCEPTION_SLAVE_OR_SERVER_FAILURE))
(define EMBXACK     (+ MODBUS_ENOBASE MODBUS_EXCEPTION_ACKNOWLEDGE))
(define EMBXSBUSY   (+ MODBUS_ENOBASE MODBUS_EXCEPTION_SLAVE_OR_SERVER_BUSY))
(define EMBXNACK    (+ MODBUS_ENOBASE MODBUS_EXCEPTION_NEGATIVE_ACKNOWLEDGE))
(define EMBXMEMPAR  (+ MODBUS_ENOBASE MODBUS_EXCEPTION_MEMORY_PARITY))
(define EMBXGPATH   (+ MODBUS_ENOBASE MODBUS_EXCEPTION_GATEWAY_PATH))
(define EMBXGTAR    (+ MODBUS_ENOBASE MODBUS_EXCEPTION_GATEWAY_TARGET))

;; Native libmodbus error codes
(define EMBBADCRC   (+ EMBXGTAR 1))
(define EMBBADDATA  (+ EMBXGTAR 2))
(define EMBBADEXC   (+ EMBXGTAR 3))
(define EMBUNKEXC   (+ EMBXGTAR 4))
(define EMBMDATA    (+ EMBXGTAR 5))
(define EMBBADSLAVE (+ EMBXGTAR 6))

;;; https://github.com/stephane/libmodbus/blob/master/src/modbus-tcp.h
(define MODBUS_TCP_DEFAULT_PORT   502)
(define MODBUS_TCP_SLAVE          #xFF)

;; Modbus_Application_Protocol_V1_1b.pdf Chapter 4 Section 1 Page 5
;; TCP MODBUS ADU = 253 bytes + MBAP (7 bytes) = 260 bytes
(define MODBUS_TCP_MAX_ADU_LENGTH  260)
