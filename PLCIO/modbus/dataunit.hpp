#pragma once

#include "shared/databytes.hpp"

#define MODBUS_COIL_NStar(quantity) ((uint8)((quantity / 8) + ((quantity % 8) ? 1 : 0)))
#define MODBUS_REGISTER_NStar(quantity) ((uint8)(quantity * 2))
#define MODBUS_QUEUE_NStar(quantity) ((uint8)(quantity * 2))

uint16 modbus_read_mbap(Windows::Storage::Streams::IDataReader^ mbin,
    uint16* transaction, uint16* protocol, uint16* length, uint8* unit);

void modbus_write_adu(Windows::Storage::Streams::IDataWriter^ mbout,
    uint16 transaction, uint16 protocol, uint8 unit,
    uint8 function_code, uint8* data, uint16 data_length);

void modbus_write_exn_adu(Windows::Storage::Streams::IDataWriter^ mbout,
    uint16 transaction, uint16 protocol, uint8 unit,
    uint8 raw_funcode, uint8 reason);
