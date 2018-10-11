#include "plc.hpp"
#include "box.hpp"

using namespace WarGrey::SCADA;

float WarGrey::SCADA::AI_ref(const uint8* db, size_t idx, float scale) {
	return bigendian_flword_ref(db, (idx - 1) * 2, scale);
}

bool WarGrey::SCADA::DI_ref(const uint8* db, size_t idx) {
	return quantity_bit_ref(db, idx / 8, idx % 8);
}

unsigned int WarGrey::SCADA::DI_ref(const uint8* db, size_t idx0, size_t idxn) {
	unsigned int dest;
	unsigned int d_idx = 0;

	for (size_t idx = idx0; idx < idxn; idx++) {
		bool bit = DI_ref(db, idx);

		if (bit) {
			dest |= (0b1U << d_idx);
		}

		d_idx++;
	}

	return dest;
}

/*************************************************************************************************/
PLCMaster::PLCMaster(Syslog* alarm) : MRMaster(alarm) {
	MrMessageConfiguration config(98, 40L, 1000);

	config.set_fcode(char(0x31));

	this->set_message_preference(config);
}

/*************************************************************************************************/
void PLCConfirmation::on_all_signals(size_t addr0, size_t addrn, uint8* data, size_t size, Syslog* logger) {
	size_t digital_quantity_size = 200U;
	size_t analog_quantity_size = 800U;
	size_t total = digital_quantity_size + analog_quantity_size;
	uint8* analog_data = data + digital_quantity_size;

	if (size == total) {
		this->on_digital_input_data(data, digital_quantity_size, logger);
		this->on_analog_input(analog_data, size - digital_quantity_size, logger);
	} else {
		logger->log_message(Log::Warning, L"data size contract is broken, expected %d, given %d!", total, size);
	}
}

