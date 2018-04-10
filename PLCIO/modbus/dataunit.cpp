#include "modbus/dataunit.hpp"
#include "shared/databytes.hpp"
#include "syslog.hpp"

using namespace Windows::Storage::Streams;

static void inline modbus_write_mbap(IDataWriter^ mbout, uint16 transaction, uint16 protocol, uint16 length, uint8 unit) {
    mbout->WriteUInt16(transaction);
    mbout->WriteUInt16(protocol);
    mbout->WriteUInt16(length);
    mbout->WriteByte(unit);
}

uint16 modbus_read_mbap(IDataReader^ mbin, uint16* transaction, uint16* protocol, uint16* length, uint8* unit) {
	(*transaction) = mbin->ReadUInt16();
	(*protocol) = mbin->ReadUInt16();
	(*length) = mbin->ReadUInt16();
	(*unit) = mbin->ReadByte();

	return (*length) - 1;
}

void modbus_write_adu(IDataWriter^ mbout, uint16 transaction, uint16 protocol, uint8 unit
    , uint8 function_code, uint8* data, uint16 data_length) {
    modbus_write_mbap(mbout, transaction, protocol, data_length + (uint16)2, unit);
    mbout->WriteByte(function_code);
    WRITE_BYTES(mbout, data, data_length);
}

void modbus_write_exn_adu(IDataWriter^ mbout, uint16 transaction, uint16 protocol, uint8 unit, uint8 raw_funcode, uint8 reason) {
    modbus_write_mbap(mbout, transaction, protocol, 3, unit);
    mbout->WriteByte(raw_funcode + (uint8)0x80);
    mbout->WriteByte(reason);
}
