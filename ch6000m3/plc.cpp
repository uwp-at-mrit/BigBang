#include "plc.hpp"
#include "box.hpp"

using namespace WarGrey::SCADA;

PLCClient::PLCClient(Syslog* alarm) : MRMaster(alarm) {}

void PLCClient::send_scheduled_request(long long count, long long interval, long long uptime) {
	this->read_all_signal(98, 1, 4571);
}

bool PLCClient::fill_signal_preferences(MRSignal type, uint16* db, uint16* addr0, uint16* addrn) {
	bool has_set = true;
	uint16 data_block, end;

	switch (type) {
	case MRSignal::Realtime:         data_block = 2;   end = 600 - 1; break;

	case MRSignal::DigitalInput:     data_block = 205; end = 300 - 1; break;
	case MRSignal::DigitalInputRaw:  data_block = 4;   end = 104 - 1; break;
	case MRSignal::DigitalOutputRaw: data_block = 6;   end = 100 - 1; break;

	case MRSignal::AnalogInput:      data_block = 203; end = 768 - 1; break;
	case MRSignal::AnalogInputRaw:   data_block = 3;   end = 768 - 1; break;
	case MRSignal::AnalogOutput:     data_block = 204; end = 256 - 1; break;
	case MRSignal::AnalogOutputRaw:  data_block = 5;   end = 256 - 1; break;
	
	default: has_set = false; break;
	}

	if (has_set) {
		SET_BOX(db, data_block);
		SET_BOX(addr0, 0);
		SET_BOX(addrn, end);
	}

	return has_set;
}
