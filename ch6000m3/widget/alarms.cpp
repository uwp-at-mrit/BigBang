#include <map>

#include "widget/alarms.hpp"
#include "stone/tongue/alarm.hpp"
#include "configuration.hpp"

#include "graphlet/shapelet.hpp"

#include "satellite.hpp"
#include "system.hpp"
#include "module.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;

#include "time.hpp"

/*************************************************************************************************/
private class Alarms : public ISatellite, public PLCConfirmation {
public:
	Alarms() : ISatellite(default_logging_level, __MODULE__) {}

	void fill_extent(float* width, float* height) {
		float margin = large_font_size * 4.0F;
		Size size = system_screen_size();

		SET_BOX(width, size.Width - margin * 2.0F);
		SET_BOX(height, size.Height - margin);
	}

public:
	void pre_read_data(Syslog* logger) override {
		this->enter_critical_section();
		this->begin_update_sequence();
	}

	void on_digital_input(long long timepoint_ms, const uint8* DB4, size_t count4, const uint8* DB205, size_t count205, Syslog* logger) override {
		Alarm* alarm = Alarm::first();

		while (alarm != nullptr) {
			const uint8* db = ((alarm->ToIndex() > 3000L) ? DB205 : DB4);
			bool alerting = DBX(db, alarm->ToIndex());

			if (alerting) {
				logger->log_message(Log::Alarm, alarm->ToLocalString());
			} else {
				//logger->log_message(Log::Info, alarm->ToLocalString());
			}

			alarm = alarm->foreward();
		}
	}

	void post_read_data(Syslog* logger) override {
		this->end_update_sequence();
		this->leave_critical_section();
	}

public:
	void load(CanvasCreateResourcesReason reason, float width, float height) override {
	}

	void reflow(float width, float height) override {
	}

public:
	bool Alarms::can_select(IGraphlet* g) override {
		return false;
	}

private: // never delete these graphlets manually.

private:
};

/*************************************************************************************************/
static Alarms* the_alarmer = nullptr;

void WarGrey::SCADA::initialize_the_alarm(PLCMaster* plc) {
	if (the_alarmer == nullptr) {
		the_alarmer = new Alarms();

		plc->push_confirmation_receiver(the_alarmer);
	}
}

void WarGrey::SCADA::display_the_alarm() {
	if (the_alarmer != nullptr) {
		the_alarmer->show();
	}
}
