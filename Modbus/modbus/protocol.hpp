#pragma once

#include "modbus/constants.hpp"

#define MODBUS_READ_BYTES(mbin, dest, count) do { for (size_t i = 0; i < count; i++) { dest[i] = mbin->ReadByte(); } } while(0)
#define MODBUS_WRITE_BYTES(mbout, src, count) do { for (size_t i = 0; i < count; i++) { mbout->WriteByte(src[i]); } } while(0)

uint16 modbus_read_mbap(Windows::Storage::Streams::IDataReader^ mbin,
    uint16* transaction, uint16* protocol, uint16* length, uint8* unit);

void modbus_write_adu(Windows::Storage::Streams::IDataWriter^ mbout,
    uint16 transaction, uint16 protocol, uint8 unit,
    uint8 function_code, uint8* data, size_t data_length);

void modbus_write_exn_adu(Windows::Storage::Streams::IDataWriter^ mbout,
    uint16 transaction, uint16 protocol, uint8 unit,
    uint8 raw_funcode, uint8 reason);
