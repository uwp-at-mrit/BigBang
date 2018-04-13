#include "modbus/dataunit.hpp"
#include "shared/databytes.hpp"
#include "syslog.hpp"

using namespace Windows::Storage::Streams;

static void inline modbus_write_mbap(IDataWriter^ mrout, uint16 transaction, uint16 protocol, uint16 length, uint8 unit) {
    mrout->WriteUInt16(transaction);
    mrout->WriteUInt16(protocol);
    mrout->WriteUInt16(length);
    mrout->WriteByte(unit);
}

uint16 mr_read_header(IDataReader^ mrin, uint8* head, uint8* fcode, uint16* db_id, uint16* addr0, uint16* addrn, uint16* size) {
	(*head) = mrin->ReadByte();
	(*fcode) = mrin->ReadByte();
	(*db_id) = mrin->ReadUInt16();
	(*addr0) = mrin->ReadUInt16();
	(*addrn) = mrin->ReadUInt16();
	(*size) = mrin->ReadUInt16();

	return (*size) + 4;
}

void mr_read_tail(Windows::Storage::Streams::IDataReader^ mrin, uint16 size, uint8* data, uint16* checksum, uint16* eom) {
	READ_BYTES(mrin, data, size);
	(*checksum) = mrin->ReadUInt16();
	(*eom) = mrin->ReadUInt16();
}
