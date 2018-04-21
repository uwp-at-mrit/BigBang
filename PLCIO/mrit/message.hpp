#pragma once

#include "shared/databytes.hpp"

uint16 mr_read_header(Windows::Storage::Streams::IDataReader^ mrin, uint8* head, uint8* fcode, uint16* db_id, uint16* addr0, uint16* addrn, uint16* size);
uint16 mr_write_header(Windows::Storage::Streams::IDataWriter^ mrout, uint8 fcode, uint16 db_id, uint16 addr0, uint16 addrn);

void mr_read_tail(Windows::Storage::Streams::IDataReader^ mrin, uint16 size, uint8* data, uint16* checksum, uint16* eom);
uint16 mr_write_tail(Windows::Storage::Streams::IDataWriter^ mrout, uint8* data, uint16 size, uint16 header_checksum, uint16 msgsize);
