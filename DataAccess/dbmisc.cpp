#include "dbmisc.hpp"
#include "time.hpp"

#include <mutex>

using namespace WarGrey::SCADA;

// WARNING: the RAND_MAX is 0x7FFF
static inline uint8 urnd08() {
	return (uint8)(rand() % 0xFFU);
}

static inline uint16 urnd16() {
	return (uint16)((urnd08() << 8U) | urnd08());
}

static inline uint32 urnd32() {
	return (uint32)((urnd16() << 16U) | urnd16());
}

/*************************************************************************************************/
/** WARNING
 * The actual generated 64bit primary keys only have 63 significant bits(3bits therefore are used to represent `version`),
 *  since integers in SQLite3 are signed and the overflowed ones will be converted to `real`s unexpectedly.
 */

int64 WarGrey::SCADA::pk64_timestamp() {
	static std::mutex clock;
	static uint16 clock_seq = 0;

	int64 version = 0b001;
	int64 now_us = current_microseconds();
	int64 ts32 = (now_us / 1000000) & 0xFFFFU;
	int64 us20 = (now_us % 1000000);

	clock.lock();
	int64 clock_seq8 = clock_seq;
	clock_seq = (clock_seq + 1) & 0xFFU;
	clock.unlock();

	return (version << 60U) | (ts32 << 28U) | (us20 << 8U) | clock_seq8;
}

int64 WarGrey::SCADA::pk64_random() {
	int64 version = 0b100;
	int64 ts32 = urnd32();
	int64 u28_16 = urnd16();
	int64 u28_12 = rand() % 0xFFFU + 1U;
	int64 clock_seq4 = rand() % 0b1111U + 1U;

	return (version << 60U) | (ts32 << 28U) | (u28_16 << 12U) | (u28_12 << 4) | clock_seq4;
}
