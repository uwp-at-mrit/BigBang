#include "IPLCMaster.hpp"

#include "time.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::UI::Xaml;

using namespace Windows::Networking;
using namespace Windows::Networking::Sockets;
using namespace Windows::Storage::Streams;

private class TimerListener : public IPLCStatusListener {
public:
	TimerListener() {
		this->last_receive_time = current_milliseconds();
	}

public:
	void check_timeout(IPLCMaster* master, long long timeout) {
		long long now = current_milliseconds();

		if ((now - this->last_receive_time) > timeout) {
			master->suicide();
			this->last_receive_time = now;
		}
	}

public:
	void on_receive_data(IPLCMaster* master, long long bytes, double span_ms, double timestamp_ms) override {
		this->last_receive_time = (long long)(std::round(timestamp_ms));
	}

private:
	long long last_receive_time;
};

private ref class PLCMasterKiller {
public:
	virtual ~PLCMasterKiller() {
		/** NOTE
		 * By design, the TimerListener shares the same life cycle with the master,
		 * therefore, there is no need to remove it from its status listener list.
		 *
		 * TODO
		 * However, a mechanism to remove any listener from the list should be done.
		 */

		delete this->listener;
	}

internal:
	PLCMasterKiller(IPLCMaster* master, long long ms) : master(master) {
		this->timer = ref new DispatcherTimer();
		this->timer->Tick += ref new EventHandler<Platform::Object^>(this, &PLCMasterKiller::check_timeout);

		this->listener = new TimerListener();
		this->master->push_plc_status_listener(this->listener);
	}

public:
	void set_timeout(long long ms) {
		this->timeout = ms;

		if (ms > 0) {
			this->timer->Interval = make_timespan_from_milliseconds(ms);
			this->timer->Start(); // starting or restarting
			this->timer->Start(); // ensured restarting
		} else {
			this->timer->Stop();
		}
	}

public:
	void check_timeout(Platform::Object^ whocares, Platform::Object^ useless) {
		this->listener->check_timeout(this->master, this->timeout);
	}

private:
	Windows::UI::Xaml::DispatcherTimer^ timer;
	long long timeout;

private:
	IPLCMaster* master;
	TimerListener* listener;
};

/*************************************************************************************************/
IPLCMaster::~IPLCMaster() {
	this->set_suicide_timeout(0);
}

bool IPLCMaster::authorized() {
	return (this->mode != PLCMasterMode::User);
}

void IPLCMaster::set_mode(WarGrey::SCADA::PLCMasterMode mode) {
	this->mode = mode;
}

PLCMasterMode IPLCMaster::get_mode() {
	return this->mode;
}

void IPLCMaster::push_plc_status_listener(WarGrey::SCADA::IPLCStatusListener* listener) {
	if (listener != nullptr) {
		this->listeners.push_back(listener);
		listener->on_plc_connectivity_changed(this, this->connected());
	}
}

void IPLCMaster::set_suicide_timeout(long long ms) {
	PLCMasterKiller^ killer = ((this->killer == nullptr)
		? (ref new PLCMasterKiller(this, ms))
		: dynamic_cast<PLCMasterKiller^>(this->killer));

	killer->set_timeout(ms);

	if (this->killer == nullptr) {
		this->killer = killer;
	}
}

void IPLCMaster::notify_connectivity_changed() {
	bool connected = this->connected();

	for (auto it = this->listeners.begin(); it != this->listeners.end(); it++) {
		(*it)->on_plc_connectivity_changed(this, connected);
	}
}

void IPLCMaster::notify_data_sent(long long bytes, double span_ms) {
	double timestamp = current_inexact_milliseconds();

	for (auto it = this->listeners.begin(); it != this->listeners.end(); it++) {
		(*it)->on_send_data(this, bytes, span_ms, timestamp);
	}
}

void IPLCMaster::notify_data_received(long long bytes, double span_ms) {
	double timestamp = current_inexact_milliseconds();

	for (auto it = this->listeners.begin(); it != this->listeners.end(); it++) {
		(*it)->on_receive_data(this, bytes, span_ms, timestamp);
	}
}

void IPLCMaster::notify_data_confirmed(long long bytes, double span_ms) {
	double timestamp = current_inexact_milliseconds();

	for (auto it = this->listeners.begin(); it != this->listeners.end(); it++) {
		(*it)->on_confirm_data(this, bytes, span_ms, timestamp);
	}
}
