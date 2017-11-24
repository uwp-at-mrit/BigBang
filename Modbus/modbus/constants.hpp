#pragma once

static const unsigned short MODBUS_TCP_DEFAULT_PORT = 502;
static const unsigned short MODBUS_TCP_SLAVE        = 0xFF;

/* Modbus function codes */
static const unsigned char MODBUS_READ_COILS               = 0x01;
static const unsigned char MODBUS_READ_DISCRETE_INPUTS     = 0x02;
static const unsigned char MODBUS_READ_HOLDING_REGISTERS   = 0x03;
static const unsigned char MODBUS_READ_INPUT_REGISTERS     = 0x04;
static const unsigned char MODBUS_WRITE_SINGLE_COIL        = 0x05;
static const unsigned char MODBUS_WRITE_SINGLE_REGISTER    = 0x06;
static const unsigned char MODBUS_READ_EXCEPTION_STATUS    = 0x07;
static const unsigned char MODBUS_WRITE_MULTIPLE_COILS     = 0x0F;
static const unsigned char MODBUS_WRITE_MULTIPLE_REGISTERS = 0x10;
static const unsigned char MODBUS_REPORT_SLAVE_ID          = 0x11;
static const unsigned char MODBUS_MASK_WRITE_REGISTER      = 0x16;
static const unsigned char MODBUS_WRITE_AND_READ_REGISTERS = 0x17;

#define MODBUS_BROADCAST_ADDRESS    0

static const unsigned short MODBUS_MAX_READ_BITS          = 2000; // MAP: Page 12
static const unsigned short MODBUS_MAX_WRITE_BITS         = 1968; // MAP: Page 29
static const unsigned short MODBUS_MAX_READ_REGISTERS     = 125;  // MAP: Page 15
static const unsigned short MODBUS_MAX_WRITE_REGISTERS    = 123;  // MAP: Page 31
static const unsigned short MODBUS_MAX_WR_READ_REGISTERS  = 125;  // MAP: Page 38
static const unsigned short MODBUS_MAX_WR_WRITE_REGISTERS = 121;  // MAP: Page 38

/**
* The size of the MODBUS PDU is limited by the size constraint inherited from
* the first MODBUS implementation on Serial Line network (max. RS485 ADU = 256
* bytes). Therefore, MODBUS PDU for serial line communication = 256 - Server
* address (1 byte) - CRC (2 bytes) = 253 bytes.
*/
static const unsigned char MODBUS_MAX_PDU_LENGTH      = 253;
static const unsigned short MODBUS_TCP_MAX_ADU_LENGTH = 260; // 253 bytes + MBAP (7 bytes) = 260 bytes
static const unsigned short MODBUS_RTU_MAX_ADU_LENGTH = 256; // 253 bytes + Server address (1 byte) + CRC (2 bytes)
static const unsigned short MODBUS_MAX_ADU_LENGTH     = 260; // max(MODBUS_TCP_MAX_ADU_LENGTH, MODBUS_RTU_MAX_ADU_LENGTH)

/* Modbus Exception Codes. MAP: Page 48 */
static const unsigned char MODBUS_EXN_ILLEGAL_FUNCTION              = 0x01;
static const unsigned char MODBUS_EXN_ILLEGAL_DATA_ADDRESS          = 0x02;
static const unsigned char MODBUS_EXN_ILLEGAL_DATA_VALUE            = 0x03;
static const unsigned char MODBUS_EXN_DEVICE_FAILURE                = 0x04;
static const unsigned char MODBUS_EXN_ACKNOWLEDGE                   = 0x05;
static const unsigned char MODBUS_EXN_DEVICE_BUSY                   = 0x06;
static const unsigned char MODBUS_EXN_NEGATIVE_ACKNOWLEDGE          = 0x07;
static const unsigned char MODBUS_EXN_MEMORY_PARITY_ERROR           = 0x08;
static const unsigned char MODBUS_EXN_GATEWAY_PATH_UNAVAILABLE      = 0x0A;
static const unsigned char MODBUS_EXN_GATEWAY_TARGET_DEVICE_FAILURE = 0x0B;

#define MODBUS_GET_HIGH_BYTE(data) (((data) >> 8) & 0xFF)
#define MODBUS_GET_LOW_BYTE(data) ((data) & 0xFF)

#define MODBUS_GET_INT64_FROM_INT16(tab_int16, index) \
    (((int64_t)tab_int16[(index)    ] << 48) + \
     ((int64_t)tab_int16[(index) + 1] << 32) + \
     ((int64_t)tab_int16[(index) + 2] << 16) + \
      (int64_t)tab_int16[(index) + 3])

#define MODBUS_GET_INT32_FROM_INT16(tab_int16, index) ((tab_int16[(index)] << 16) + tab_int16[(index) + 1])
#define MODBUS_GET_INT16_FROM_INT8(tab_int8, index) ((tab_int8[(index)] << 8) + tab_int8[(index) + 1])

#define MODBUS_SET_INT16_TO_INT8(tab_int8, index, value) \
    do { \
        tab_int8[(index)] = (value) >> 8;  \
        tab_int8[(index) + 1] = (value) & 0xFF; \
    } while (0)

#define MODBUS_SET_INT32_TO_INT16(tab_int16, index, value) \
    do { \
        tab_int16[(index)    ] = (value) >> 16; \
        tab_int16[(index) + 1] = (value); \
    } while (0)

#define MODBUS_SET_INT64_TO_INT16(tab_int16, index, value) \
    do { \
        tab_int16[(index)    ] = (value) >> 48; \
        tab_int16[(index) + 1] = (value) >> 32; \
        tab_int16[(index) + 2] = (value) >> 16; \
        tab_int16[(index) + 3] = (value); \
    } while (0)
