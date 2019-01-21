#include <map>

#include "widget/alarms.hpp"
#include "stone/tongue/alarm.hpp"
#include "configuration.hpp"

#include "graphlet/tablet.hpp"
#include "schema/datalet/alarm_tbl.hpp"

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
private class AlarmMS : public ISatellite, public PLCConfirmation {
public:
	virtual ~AlarmMS() noexcept {
		this->datasource->destroy();
	}

	AlarmMS() : ISatellite(default_logging_level, __MODULE__), margin(0.0F) {
		Syslog* logger = make_system_logger(default_logging_level, "AlarmHistory");

		this->style.resolve_column_width_percentage = alarm_column_width_configure;
		this->style.fill_cell_alignment = alarm_cell_alignment_configure;

		this->datasource = new AlarmDataSource(logger, RotationPeriod::Daily);
		this->datasource->reference();
	}

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
		Alarms* alarm = Alarms::first();
		unsigned int db, dbx;
		Alarm event;

		double now = current_inexact_milliseconds();
	
		while (alarm != nullptr) {
			alarm_index_translate(alarm->ToIndex(), &db, &dbx);
			bool alerting = DBX(((db == 4) ? DB4 : DB205), dbx);

			if (alerting) {
				this->datasource->save(timepoint_ms, alarm->ToIndex(), event);
				logger->log_message(Log::Alarm, alarm->ToLocalString());
			} else {
				alarm->ToLocalString();
			}

			alarm = alarm->foreward();
		}

		this->get_logger()->log_message(Log::Info, L"it took %lfms",
			current_inexact_milliseconds() - now);
	}

	void post_read_data(Syslog* logger) override {
		this->end_update_sequence();
		this->leave_critical_section();
	}

public:
	void load(CanvasCreateResourcesReason reason, float width, float height) override {
		float inset = this->margin * 2.0F;

		this->table = this->insert_one(new Tablet<AMS>(__MODULE__, width - inset, height - inset));
		this->table->set_style(this->style);
	}

	void reflow(float width, float height) override {
		this->move_to(this->table, this->margin, this->margin);
	}

public:
	bool can_select(IGraphlet* g) override {
		return false;
	}

private: // never delete these graphlets manually.
	Tablet<AMS>* table;

private:
	AlarmDataSource* datasource;
	TableStyle style;
	float margin;
};

/*************************************************************************************************/
static AlarmMS* the_alarmer = nullptr;

void WarGrey::SCADA::initialize_the_alarm(PLCMaster* plc) {
	if (the_alarmer == nullptr) {
		the_alarmer = new AlarmMS();

		plc->push_confirmation_receiver(the_alarmer);
	}
}

void WarGrey::SCADA::display_the_alarm() {
	if (the_alarmer != nullptr) {
		the_alarmer->show();
	}
}
