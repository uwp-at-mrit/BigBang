#include "mrit/magic.hpp"

#include "shared/databytes.hpp"

using namespace Windows::Storage::Streams;

uint16 mr_read_header(IDataReader^ mrin, uint8* head, uint8* fcode, uint16* db_id, uint16* addr0, uint16* addrn, uint16* size) {
	(*head) = mrin->ReadByte();
	(*fcode) = mrin->ReadByte();
	(*db_id) = mrin->ReadUInt16();
	(*addr0) = mrin->ReadUInt16();
	(*addrn) = mrin->ReadUInt16();
	(*size) = mrin->ReadUInt16();

	return (*size) + 4;
}

void mr_read_tail(IDataReader^ mrin, uint16 size, uint8* data, uint16* checksum, uint16* eom) {
	READ_BYTES(mrin, data, size);
	(*checksum) = mrin->ReadUInt16();
	(*eom) = mrin->ReadUInt16();
}

void mr_write_header(IDataWriter^ mrout, uint8 fcode, uint16 db_id, uint16 addr0, uint16 addrn) {
	mrout->WriteByte(MR_PROTOCOL_HEAD);
	mrout->WriteByte(fcode);
	mrout->WriteUInt16(db_id);
	mrout->WriteUInt16(addr0);
	mrout->WriteUInt16(addrn);
}

void mr_write_tail(IDataWriter^ mrout, uint8* data, uint16 size) {
	if (data == nullptr) {
		mrout->WriteUInt16(0);
	} else {
		mrout->WriteUInt16(size);
		WRITE_BYTES(mrout, data, size);
	}

	mrout->WriteUInt16(0xFFFF); // checksum, but it is not used in the current implementation
	mrout->WriteUInt16(MR_PROTOCOL_END);
}
