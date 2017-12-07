#include "modbus/protocol.hpp"

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
    MODBUS_WRITE_BYTES(mbout, data, data_length);
}

void modbus_write_exn_adu(IDataWriter^ mbout, uint16 transaction, uint16 protocol, uint8 unit, uint8 raw_funcode, uint8 reason) {
    modbus_write_mbap(mbout, transaction, protocol, 3, unit);
    mbout->WriteByte(raw_funcode + (uint8)0x80);
    mbout->WriteByte(reason);
}

int modbus_read_coils(uint8 *src, uint16 address, uint16 quantity, uint8 *dest) {
    uint8 shift = 0;
    uint8 byte = 0;
	uint8 d_idx = 0;

    for (uint16 i = address; i < address + quantity; i++) {
        byte |= src[i] << shift;
        if (shift < 7) {
            shift += 1;
        } else {
			dest[d_idx++] = byte;
            byte = shift = 0;
        }
    }

    if (shift != 0) {
		dest[d_idx++] = byte;
    }

    return d_idx;
}

int modbus_read_registers(uint16 *src, uint16 address, uint16 quantity, uint8 *dest) {
	uint8 d_idx = 0;

	for (uint16 i = address; i < address + quantity; i++) {
		dest[d_idx++] = src[i] >> 8;
		dest[d_idx++] = src[i] & 0xFF;
	}

	return d_idx;
}

void modbus_write_registers(uint16 *dest, uint16 address, uint16 quantity, uint8 *src) {
	uint8 d_idx = 0;

	for (size_t i = 0; i < quantity; i++) {
		dest[address + i] = MODBUS_GET_INT16_FROM_INT8(src, d_idx);
		d_idx += 2;
	}
}

void modbus_set_bits_from_byte(uint8 *dest, uint16 idx, uint8 src) {
    for (size_t i = 0; i < 8; i++) {
        dest[idx + i] = (src & (1 << i)) ? 1 : 0;
    }
}


void modbus_set_bits_from_bytes(uint8 *dest, uint16 idx, uint16 count, const uint8 *src) {
    uint8 shift = 0;

    for (size_t i = idx; i < idx + count; i++) {
        dest[i] = src[(i - idx) / 8] & (1 << shift) ? 1 : 0;
        shift = (shift + 1) % 8;
    }
}

uint8 modbus_get_byte_from_bits(const uint8 *src, uint16 idx, uint16 count) {
    uint8 value = 0;

	if (count > 8) {
		// In fact, the count as an input argument should not be greater than 8
		count = 8;
	}

    for (size_t i = 0; i < count; i++) {
        value |= (src[idx + i] << i);
    }

    return value;
}
