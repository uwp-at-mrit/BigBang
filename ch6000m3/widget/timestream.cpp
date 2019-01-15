#include "widget/timestream.hpp"
#include "configuration.hpp"

#include "page/hydraulics.hpp"

#include "box.hpp"
#include "system.hpp"
#include "module.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::Storage;

using namespace Microsoft::Graphics::Canvas::UI;

/*************************************************************************************************/
private class TimeStream : public TimeMachine, public PLCConfirmation {
public:
	TimeStream(long long time_speed, int frame_rate)
		: TimeMachine(L"timemachine", time_speed * 1000LL, frame_rate, make_system_logger(default_logging_level, __MODULE__))
		, last_timepoint(current_milliseconds()) {}

	void fill_extent(float* width, float* height) {
		float margin = normal_font_size * 2.0F;
		Size size = system_screen_size();

		SET_BOX(width, size.Width - margin);
		SET_BOX(height, size.Height - margin);
	}

public:
	void on_all_signals(size_t addr0, size_t addrn, uint8* data, size_t size, WarGrey::SCADA::Syslog* logger) override {
		long long timestamp = current_milliseconds();

		if ((timestamp - last_timepoint) >= this->get_time_speed()) {
			this->save_snapshot(timestamp, addr0, addrn, (const char*)(data), size);
			this->last_timepoint = timestamp;
		}
	}

public:
	void construct(CanvasCreateResourcesReason reason) override {
		this->pickup(new HydraulicsPage());
	}

private:
	long long last_timepoint;
};

/*************************************************************************************************/
static TimeStream* the_timemachine = nullptr;

void WarGrey::SCADA::initialize_the_timemachine(PLCMaster* plc, long long speed, int frame_rate) {
	if (the_timemachine == nullptr) {
		the_timemachine = new TimeStream(speed, frame_rate);

		plc->push_confirmation_receiver(the_timemachine);
	}
}

void WarGrey::SCADA::launch_the_timemachine() {
	if (the_timemachine != nullptr) {
		the_timemachine->show();
	}
}
