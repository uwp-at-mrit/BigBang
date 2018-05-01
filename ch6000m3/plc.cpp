#include "plc.hpp"
#include "box.hpp"

using namespace WarGrey::SCADA;

static const uint16 MRDB_REALTIME           = 2;
static const uint16 MRDB_FORAT              = 20;
static const uint16 MRDB_DIGITAL_INPUT      = 205;
static const uint16 MRDB_DIGITAL_INPUT_RAW  = 4;
static const uint16 MRDB_DIGITAL_OUTPUT_RAW = 6;
static const uint16 MRDB_ANALOG_INPUT       = 203;
static const uint16 MRDB_ANALOG_INPUT_RAW   = 3;
static const uint16 MRDB_ANALOG_OUTPUT      = 204;
static const uint16 MRDB_ANALOG_OUTPUT_RAW  = 5;

static bool fill_signal_preferences(size_t type, size_t* count, size_t* addr0, size_t* addrn) {
	bool has_set = true;
	size_t c = 0;
	size_t start = 0;
	size_t end = 0;

	switch (type) {
	case MRDB_ANALOG_INPUT_RAW:   c = 280; start = 1;    end = 1120; break; // DB3
	case MRDB_ANALOG_INPUT:       c = 280; start = 1121; end = 2240; break; // DB203
	case MRDB_ANALOG_OUTPUT_RAW:  c = 48;  start = 2241; end = 2432; break; // DB5
	case MRDB_ANALOG_OUTPUT:      c = 48;  start = 2433; end = 2624; break; // DB204
	case MRDB_FORAT:              c = 166; start = 2625; end = 3282; break; // DB20
	case MRDB_REALTIME:           c = 176; start = 3283; end = 3986; break; // DB2

	case MRDB_DIGITAL_INPUT_RAW:  c = 124; start = 3987; end = 4110; break; // DB4
	case MRDB_DIGITAL_OUTPUT_RAW: c = 76;  start = 4111; end = 4186; break; // DB6
	case MRDB_DIGITAL_INPUT:      c = 385; start = 4187; end = 4571; break; // DB205

	default: has_set = false; break;
	}

	if (has_set) {
		SET_BOX(count, c);
		SET_BOX(addr0, start);
		SET_BOX(addrn, end);
	}

	return has_set;
}

static bool valid_address(Syslog* logger, size_t db, size_t addr0, size_t addrn, size_t count, size_t unit_size, size_t total) {
	bool valid = ((addr0 + count * unit_size) <= total);

	if (((addrn - addr0 + 1) != (count * unit_size))) {
		logger->log_message(Log::Warning,
			L"the address range [%u, %u] of DB%u is misconfigured or mistyped(count: %u != %u)",
			db, addr0, addrn, count, (addrn - addr0 + 1) / unit_size);
	}

	if (!valid) {
		logger->log_message(Log::Error,
			L"the end address of DB%u is misconfigured or mistyped(address: %u > %u)",
			db, addrn, total);
	}

	return valid;
}

/*************************************************************************************************/
float WarGrey::SCADA::RealData(const uint8* src, size_t idx) {
	return get_bigendian_float(src, idx * 4U);
}

/*************************************************************************************************/
PLCMaster::PLCMaster(Syslog* alarm) : MRMaster(alarm), tidemark(0.0F) {
	this->append_confirmation_receiver(this);
}

void PLCMaster::send_scheduled_request(long long count, long long interval, long long uptime) {
	// TODO: why the initial tidemark has a negative float value?
	this->read_all_signal(98, 0, 0x11DB, this->tidemark);
}

void PLCMaster::on_realtime_data(const uint8* db2, size_t count, Syslog* logger) {
	this->tidemark = get_bigendian_float(db2, 0);
}

/*************************************************************************************************/
void PLCConfirmation::on_all_signals(size_t addr0, size_t addrn, uint8* data, size_t size, Syslog* logger) {
	size_t count, subaddr0, subaddrn;
	size_t analog_dbs[] = { 3, 203, 5, 204, 20, 2 };
	size_t digital_dbs[] = { 4, 6, 205 };
	size_t dqcount = 2;
	size_t analog_size = sizeof(float);
	size_t digital_size = sizeof(uint8);

	for (size_t i = 0; i < sizeof(analog_dbs) / sizeof(size_t); i++) {
		if (fill_signal_preferences(analog_dbs[i], &count, &subaddr0, &subaddrn)) {
			if (analog_dbs[i] == MRDB_FORAT) {
				// this is a special case, some digital data is stored in the first two bytes. 
				subaddr0 += dqcount;
				count -= dqcount;
			}

			if (valid_address(logger, analog_dbs[i], subaddr0, subaddrn, count, analog_size, size)) {
				uint8* raw = data + subaddr0;
				size_t real_count = count * analog_size;
				
				switch (analog_dbs[i]) {
				case MRDB_REALTIME: this->on_realtime_data(raw, real_count, logger); break;
				case MRDB_FORAT: this->on_forat_data(raw - dqcount, dqcount, raw, real_count, logger); break;
				case MRDB_ANALOG_INPUT: this->on_analog_input_data(raw, real_count, logger); break;
				case MRDB_ANALOG_INPUT_RAW: this->on_raw_analog_input_data(raw, real_count, logger); break;
				case MRDB_ANALOG_OUTPUT: this->on_analog_output_data(raw, real_count, logger); break;
				case MRDB_ANALOG_OUTPUT_RAW: this->on_raw_analog_output_data(raw, real_count, logger); break;
				}
			}
		} else {
			logger->log_message(Log::Warning, L"missing configuration for data block %hu", analog_dbs[i]);
		}
	}

	for (size_t i = 0; i < sizeof(digital_dbs) / sizeof(size_t); i++) {
		if (fill_signal_preferences(digital_dbs[i], &count, &subaddr0, &subaddrn)) {
			if (valid_address(logger, digital_dbs[i], subaddr0, subaddrn, count, digital_size, size)) {
				uint8* raw = data + subaddr0;
				
				switch (digital_dbs[i]) {
				case MRDB_DIGITAL_INPUT: this->on_digital_input(raw, count, logger); break;
				case MRDB_DIGITAL_INPUT_RAW: this->on_raw_digital_input(raw, count, logger); break;
				case MRDB_DIGITAL_OUTPUT_RAW: this->on_raw_digital_output(raw, count, logger); break;
				}
			}
		} else {
			logger->log_message(Log::Warning, L"missing configuration for data block %hu", digital_dbs[i]);
		}
	}
}
