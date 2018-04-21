#include "mrit/magic.hpp"

#include "shared/databytes.hpp"

using namespace Windows::Storage::Streams;

inline static uint16 foldsum(uint16 word) {
	return (word >> 8) + (word & 0xFF);
}

inline static uint16 foldsum(uint8* data, uint16 size) {
	uint16 checksum = 0;

	for (uint16 i = 0; i < size; i++) {
		checksum += data[i];
	}

	return checksum;
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

void mr_read_tail(IDataReader^ mrin, uint16 size, uint8* data, uint16* checksum, uint16* eom) {
	READ_BYTES(mrin, data, size);
	(*checksum) = mrin->ReadUInt16();
	(*eom) = mrin->ReadUInt16();
}

uint16 mr_write_header(IDataWriter^ mrout, uint8 fcode, uint16 db_id, uint16 addr0, uint16 addrn) {
	mrout->WriteByte(MR_PROTOCOL_HEAD);
	mrout->WriteByte(fcode);
	mrout->WriteUInt16(db_id);
	mrout->WriteUInt16(addr0);
	mrout->WriteUInt16(addrn);

	return MR_PROTOCOL_HEAD + fcode
		+ foldsum(db_id)
		+ foldsum(addr0)
		+ foldsum(addrn);
}

uint16 mr_write_tail(IDataWriter^ mrout, uint8* data, uint16 size, uint16 header_checksum, uint16 msgsize) {
	uint8 padding_start = MR_PROTOCOL_HEADER_LENGTH + MR_PROTOCOL_TAIL_LENGTH;
	uint16 checksum = ((header_checksum + foldsum(size) + foldsum(data, size)) & 0xFFFF);

	padding_start += size;
	mrout->WriteUInt16(size);
	WRITE_BYTES(mrout, data, size);
	mrout->WriteUInt16(checksum);
	mrout->WriteUInt16(MR_PROTOCOL_END);

	if (msgsize > 0) {
		for (uint8 i = padding_start; i < msgsize; i++) {
			mrout->WriteByte(0xFF);
		}
	}

	return checksum;
}
