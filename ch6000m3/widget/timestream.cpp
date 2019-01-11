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
	TimeStream(int frame_rate, long long snapshot_interval = 2)
		: TimeMachine(L"timemachine", frame_rate, make_system_logger(default_logging_level, __MODULE__))
		, snapshot_interval(snapshot_interval * 1000LL), last_timepoint(current_seconds()) {}

	void fill_extent(float* width, float* height) {
		float margin = normal_font_size * 2.0F;
		Size size = system_screen_size();

		SET_BOX(width, size.Width - margin);
		SET_BOX(height, size.Height - margin);
	}

public:
	void on_all_signals(size_t addr0, size_t addrn, uint8* data, size_t size, WarGrey::SCADA::Syslog* logger) override {
		long long timestamp = current_milliseconds();

		if ((timestamp - last_timepoint) >= this->snapshot_interval) {
			this->save_snapshot(timestamp, addr0, addrn, (const char*)(data), size);
			this->last_timepoint = timestamp;
		}
	}

public:
	void construct(CanvasCreateResourcesReason reason) override {
		this->push_timeline(new HydraulicsPage());
	}

private:
	long long last_timepoint;
	long long snapshot_interval;
};

/*************************************************************************************************/
static TimeStream* the_timemachine = nullptr;

void WarGrey::SCADA::initialize_the_timemachine(PLCMaster* plc, int frame_rate, long long snapshot_interval) {
	if (the_timemachine == nullptr) {
		the_timemachine = new TimeStream(frame_rate, snapshot_interval);

		plc->push_confirmation_receiver(the_timemachine);
	}
}

void WarGrey::SCADA::launch_the_timemachine() {
	if (the_timemachine != nullptr) {
		the_timemachine->show();
	}
}
