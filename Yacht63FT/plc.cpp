#include "plc.hpp"
#include "box.hpp"

using namespace WarGrey::SCADA;

PLCMaster::PLCMaster(Syslog* alarm) : MRMaster(alarm) {
	this->append_confirmation_receiver(this);
}

void PLCMaster::send_scheduled_request(long long count, long long interval, long long uptime) {
	this->read_all_signal(98, 0, 0x11DB, 0.0F);
}

/*************************************************************************************************/
void PLCConfirmation::on_all_signals(size_t addr0, size_t addrn, uint8* data, size_t size, Syslog* logger) {
	
}

