#include "plc.hpp"
#include "box.hpp"

using namespace WarGrey::SCADA;

PLCMaster::PLCMaster(Syslog* alarm) : MRMaster(alarm) {
	this->set_message_alignment_size(40);
}

void PLCMaster::send_scheduled_request(long long count, long long interval, long long uptime) {
	this->read_all_signal(98, 0, 0x11DB, 0.0F);
}

bool PLCMaster::fill_signal_preferences(uint16 type, uint16* count, uint16* addr0, uint16* addrn) {
	bool has_set = true;
	uint16 c = 0;
	uint16 start = 0;
	uint16 end = 0;

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
