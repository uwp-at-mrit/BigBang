#pragma once

#include <cinttypes>

static const uint16 MODBUS_PROTOCOL         = 0x00;
static const uint16 MODBUS_MBAP_LENGTH      = (uint16)(2 + 2 + 2 + 1);

static const uint16 MODBUS_TCP_DEFAULT_PORT = 502;
static const uint16 MODBUS_TCP_SLAVE        = 0xFF;

/* Modbus function codes */
static const uint8 MODBUS_READ_COILS                 = 0x01;
static const uint8 MODBUS_READ_DISCRETE_INPUTS       = 0x02;
static const uint8 MODBUS_READ_HOLDING_REGISTERS     = 0x03;
static const uint8 MODBUS_READ_INPUT_REGISTERS       = 0x04;
static const uint8 MODBUS_WRITE_SINGLE_COIL          = 0x05;
static const uint8 MODBUS_WRITE_SINGLE_REGISTER      = 0x06;
static const uint8 MODBUS_READ_EXCEPTION_STATUS      = 0x07;
static const uint8 MODBUS_DIAGNOSTIC                 = 0x08;
static const uint8 MODBUS_GET_COM_EVENT_COUNTER      = 0x0B;
static const uint8 MODBUS_GET_COM_EVENT_LOG          = 0x0C;
static const uint8 MODBUS_WRITE_MULTIPLE_COILS       = 0x0F;
static const uint8 MODBUS_WRITE_MULTIPLE_REGISTERS   = 0x10;
static const uint8 MODBUS_REPORT_SLAVE_ID            = 0x11;
static const uint8 MODBUS_READ_FILE_RECORD           = 0x14;
static const uint8 MODBUS_WRITE_FILE_RECORD          = 0x15;
static const uint8 MODBUS_MASK_WRITE_REGISTER        = 0x16;
static const uint8 MODBUS_WRITE_AND_READ_REGISTERS   = 0x17;
static const uint8 MODBUS_READ_FIFO_QUEUES           = 0x18;
static const uint8 MODBUS_READ_DEVICE_IDENTIFICATION = 0x2B;

/* Modbus limits */
static const uint16 MODBUS_MAX_READ_BITS          = 0x7D0; // MAP: Page 12
static const uint16 MODBUS_MAX_WRITE_BITS         = 0x7B0; // MAP: Page 29
static const uint16 MODBUS_MAX_READ_REGISTERS     = 0x07D; // MAP: Page 15
static const uint16 MODBUS_MAX_WRITE_REGISTERS    = 0x07B; // MAP: Page 30
static const uint16 MODBUS_MAX_WR_READ_REGISTERS  = 0x07D; // MAP: Page 38
static const uint16 MODBUS_MAX_WR_WRITE_REGISTERS = 0x079; // MAP: Page 38

/**
* The size of the MODBUS PDU is limited by the size constraint inherited from
* the first MODBUS implementation on Serial Line network (max. RS485 ADU = 256
* uint8s). Therefore, MODBUS PDU for serial line communication = 256 - Server
* address (1 uint8) - CRC (2 uint8s) = 253 uint8s.
*/
static const uint8  MODBUS_MAX_PDU_LENGTH     = 253;
static const uint16 MODBUS_TCP_MAX_ADU_LENGTH = 260; // 253 uint8s + MBAP (7 uint8s) = 260 uint8s
static const uint16 MODBUS_RTU_MAX_ADU_LENGTH = 256; // 253 uint8s + Server address (1 uint8) + CRC (2 uint8s)
static const uint16 MODBUS_MAX_ADU_LENGTH     = 260; // max(MODBUS_TCP_MAX_ADU_LENGTH, MODBUS_RTU_MAX_ADU_LENGTH)

/* Modbus Exception Codes. MAP: Page 48 */
static const uint8 MODBUS_EXN_ILLEGAL_FUNCTION              = 0x01;
static const uint8 MODBUS_EXN_ILLEGAL_DATA_ADDRESS          = 0x02;
static const uint8 MODBUS_EXN_ILLEGAL_DATA_VALUE            = 0x03;
static const uint8 MODBUS_EXN_DEVICE_FAILURE                = 0x04;
static const uint8 MODBUS_EXN_ACKNOWLEDGE                   = 0x05;
static const uint8 MODBUS_EXN_DEVICE_BUSY                   = 0x06;
static const uint8 MODBUS_EXN_NEGATIVE_ACKNOWLEDGE          = 0x07;
static const uint8 MODBUS_EXN_MEMORY_PARITY_ERROR           = 0x08;
static const uint8 MODBUS_EXN_GATEWAY_PATH_UNAVAILABLE      = 0x0A;
static const uint8 MODBUS_EXN_GATEWAY_TARGET_DEVICE_FAILURE = 0x0B;
