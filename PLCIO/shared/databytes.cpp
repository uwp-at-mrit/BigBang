#include <ppltasks.h>

#include "shared/databytes.hpp"

using namespace Windows::Storage::Streams;

unsigned int discard_dirty_bytes(DataReader^ din) {
	unsigned int rest = din->UnconsumedBufferLength;

	if (rest > 0) {
		din->ReadBuffer(rest);
	}

	return rest;
}

/*************************************************************************************************/
int read_bits(uint8 *src, uint16 address, uint16 quantity, uint8 *dest) {
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

int read_words(uint16 *src, uint16 address, uint16 quantity, uint8 *dest) {
	uint8 d_idx = 0;

	for (uint16 i = address; i < address + quantity; i++) {
		dest[d_idx++] = src[i] >> 8;
		dest[d_idx++] = src[i] & 0xFF;
	}

	return d_idx;
}

void write_words(uint16 *dest, uint16 address, uint16 quantity, uint8 *src) {
	uint8 d_idx = 0;

	for (size_t i = 0; i < quantity; i++) {
		dest[address + i] = GET_INT16_FROM_INT8(src, d_idx);
		d_idx += 2;
	}
}

void set_bits_from_byte(uint8 *dest, uint16 idx, uint8 src) {
    for (unsigned int i = 0; i < 8; i++) {
        dest[idx + i] = (src & (1 << i)) ? 1 : 0;
    }
}


void set_bits_from_bytes(uint8 *dest, uint16 idx, uint16 count, const uint8 *src) {
    uint8 shift = 0;

    for (unsigned short i = idx; i < idx + count; i++) {
        dest[i] = src[(i - idx) / 8] & (1 << shift) ? 1 : 0;
        shift = (shift + 1) % 8;
    }
}

uint8 get_byte_from_bits(const uint8 *src, uint16 idx, uint16 count) {
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

/*************************************************************************************************/
void read_floats(uint8* src, uint16 address, uint16 quantity, float* dest) {
	for (uint16 i = 0; i < quantity; i++) {
		dest[i] = get_float_dcba(src, address + i * sizeof(float));
	}
}

float get_float_dcba(uint8* src, uint16 idx) {
	uint8 flbytes[sizeof(float)];
	float dest = 0.0F;

	flbytes[0] = src[idx + 3];
	flbytes[1] = src[idx + 2];
	flbytes[2] = src[idx + 1];
	flbytes[3] = src[idx + 0];

	memcpy((void*)&dest, (void*)flbytes, sizeof(float));

	return dest;
}
