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
void read_bigendian_floats(uint8* src, size_t address, size_t quantity, float* dest) {
	for (size_t i = 0; i < quantity; i++) {
		dest[i] = bigendian_float_ref(src, address + i * sizeof(float));
	}
}

uint8 bigendian_uint8_ref(const uint8* src, size_t idx) {
	return src[idx];
}

void bigendian_uint8_set(uint8* dest, size_t idx, uint8 x) {
	dest[idx] = x;
}

uint16 bigendian_uint16_ref(const uint8* src, size_t idx) {
	return GET_INT16_FROM_INT8(src, idx);
}

void bigendian_uint16_set(uint8* dest, size_t idx, uint16 x) {
	SET_INT16_TO_INT8(dest, idx, x);
}

uint32 bigendian_uint32_ref(const uint8* src, size_t idx) {
	uint32 msb16 = GET_INT16_FROM_INT8(src, idx);
	uint32 lsb16 = GET_INT16_FROM_INT8(src, idx + 2);

	return (msb16 << 16) + lsb16;
}

void bigendian_uint32_set(uint8* dest, size_t idx, uint32 x) {
	uint16 msb16 = x >> 16;
	uint16 lsb16 = x & 0xFFFF;

	SET_INT16_TO_INT8(dest, idx + 0, msb16);
	SET_INT16_TO_INT8(dest, idx + 2, lsb16);
}

uint64 bigendian_uint64_ref(const uint8* src, size_t idx) {
	uint64 msb32 = bigendian_uint32_ref(src, idx);
	uint64 lsb32 = bigendian_uint32_ref(src, idx + 4);

	return (msb32 << 32) + lsb32;
}

void bigendian_uint64_set(uint8* dest, size_t idx, uint64 x) {
	uint32 msb32 = x >> 32;
	uint32 lsb32 = x & 0xFFFFFFFF;

	bigendian_uint32_set(dest, idx, msb32);
	bigendian_uint32_set(dest, idx + 4, lsb32);
}

float bigendian_float_ref(const uint8* src, size_t idx) {
	uint8 flbytes[4];
	float dest = 0.0F;

	flbytes[0] = src[idx + 3];
	flbytes[1] = src[idx + 2];
	flbytes[2] = src[idx + 1];
	flbytes[3] = src[idx + 0];

	memcpy((void*)&dest, (void*)flbytes, 4);

	return dest;
}

void bigendian_float_set(uint8* dest, size_t idx, float src) {
	uint8 flbytes[4];

	memcpy((void*)flbytes, (void*)&src, 4);

	dest[idx + 3] = flbytes[0];
	dest[idx + 2] = flbytes[1];
	dest[idx + 1] = flbytes[2];
	dest[idx + 0] = flbytes[3];
}

float bigendian_flword_ref(const uint8* src, size_t idx, float scale) {
	uint16 u16 = bigendian_uint16_ref(src, idx);

	return float(u16) / scale;
}

void bigendian_flword_set(uint8* dest, size_t idx, float x, float scale) {
	uint16 u16 = (uint16)(roundf(x * scale));

	bigendian_uint16_set(dest, idx, u16);
}

bool quantity_bit_ref(const uint8* src, size_t idx, uint8 bit) {
	return ((src[idx] && (0x1 << bit)) != 0);
}
