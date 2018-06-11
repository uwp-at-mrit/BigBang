#include "plc.hpp"
#include "box.hpp"

using namespace WarGrey::SCADA;

PLCMaster::PLCMaster(Syslog* alarm) : MRMaster(alarm) {
	MrMessageConfiguration config(98, 40L, 1000);

	config.set_fcode(char(0x31));

	this->set_message_preference(config);
}

void PLCMaster::send_scheduled_request(long long count, long long interval, long long uptime) {
	// do nothing, just waiting.
}

/*************************************************************************************************/
void PLCConfirmation::on_all_signals(size_t addr0, size_t addrn, uint8* data, size_t size, Syslog* logger) {
	
}

