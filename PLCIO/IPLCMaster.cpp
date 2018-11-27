#include <ppltasks.h>

#include "IPLCMaster.hpp"

#include "time.hpp"
#include "system.hpp"

#include "syslog.hpp"

using namespace WarGrey::SCADA;

using namespace Concurrency;

using namespace Windows::Foundation;
using namespace Windows::Networking;
using namespace Windows::Networking::Sockets;
using namespace Windows::Storage::Streams;

/*************************************************************************************************/
bool IPLCMaster::authorized() {
	return (this->mode != PLCMasterMode::User);
}

void IPLCMaster::set_mode(WarGrey::SCADA::PLCMasterMode mode) {
	this->mode = mode;
}

PLCMasterMode IPLCMaster::get_mode() {
	return this->mode;
}

void IPLCMaster::append_plc_status_listener(WarGrey::SCADA::IPLCStatusListener* listener) {
	if (listener != nullptr) {
		this->listeners.push_back(listener);
		listener->on_plc_connectivity_changed(this, this->connected());
	}
}

void IPLCMaster::notify_connectivity_changed() {
	bool connected = this->connected();

	for (auto lt = this->listeners.begin(); lt != this->listeners.end(); lt++) {
		(*lt)->on_plc_connectivity_changed(this, connected);
	}
}

void IPLCMaster::notify_data_sent(long long bytes, double span_ms) {
	double timestamp = current_inexact_milliseconds();

	for (auto lt = this->listeners.begin(); lt != this->listeners.end(); lt++) {
		(*lt)->on_send_data(this, bytes, span_ms, timestamp);
	}
}

void IPLCMaster::notify_data_received(long long bytes, double span_ms) {
	double timestamp = current_inexact_milliseconds();

	for (auto lt = this->listeners.begin(); lt != this->listeners.end(); lt++) {
		(*lt)->on_receive_data(this, bytes, span_ms, timestamp);
	}
}

void IPLCMaster::notify_data_confirmed(long long bytes, double span_ms) {
	double timestamp = current_inexact_milliseconds();

	for (auto lt = this->listeners.begin(); lt != this->listeners.end(); lt++) {
		(*lt)->on_confirm_data(this, bytes, span_ms, timestamp);
	}
}
