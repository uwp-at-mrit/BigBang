#pragma once

#include "modbus/constants.hpp"

#define MODBUS_DISCARD_BYTES(mbin, count) do { for (uint16 i = 0; i < count; i++) { mbin->ReadByte(); } } while(0)
#define MODBUS_READ_BYTES(mbin, dest, count) do { for (uint16 i = 0; i < count; i++) { dest[i] = mbin->ReadByte(); } } while(0)
#define MODBUS_READ_DOUBLES(mbin, dest, count) do { for (uint16 i = 0; i < count; i++) { dest[i] = mbin->ReadUInt16(); } } while(0)
#define MODBUS_WRITE_BYTES(mbout, src, count) do { for (uint16 i = 0; i < count; i++) { mbout->WriteByte(src[i]); } } while(0)
#define MODBUS_COIL_NStar(quantity) ((uint8)((quantity / 8) + ((quantity % 8) ? 1 : 0)))
#define MODBUS_REGISTER_NStar(quantity) ((uint8)(quantity * 2))
#define MODBUS_QUEUE_NStar(quantity) ((uint8)(quantity * 2))

#define MODBUS_GET_HIGH_uint8(data) (((data) >> 8) & 0xFF)
#define MODBUS_GET_LOW_uint8(data) ((data) & 0xFF)

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

uint16 modbus_read_mbap(Windows::Storage::Streams::IDataReader^ mbin,
    uint16* transaction, uint16* protocol, uint16* length, uint8* unit);

void modbus_write_adu(Windows::Storage::Streams::IDataWriter^ mbout,
    uint16 transaction, uint16 protocol, uint8 unit,
    uint8 function_code, uint8* data, uint16 data_length);

void modbus_write_exn_adu(Windows::Storage::Streams::IDataWriter^ mbout,
    uint16 transaction, uint16 protocol, uint8 unit,
    uint8 raw_funcode, uint8 reason);

int modbus_read_coils(uint8 *src, uint16 address, uint16 quantity, uint8 *dest);
int modbus_read_registers(uint16 *src, uint16 address, uint16 quantity, uint8 *dest);
void modbus_write_registers(uint16 *dest, uint16 address, uint16 quantity, uint8 *src);

void modbus_set_bits_from_byte(uint8 *dest, uint16 idx, uint8 src);
void modbus_set_bits_from_bytes(uint8 *dest, uint16 idx, uint16 count, const uint8 *src);
uint8 modbus_get_byte_from_bits(const uint8 *src, uint16 idx, uint16 count);
