#include <map>

#include "page/alarm.hpp"
#include "configuration.hpp"

#include "graphlet/bitmaplet.hpp"
#include "graphlet/textlet.hpp"

#include "tongue.hpp"
#include "text.hpp"

#include "sqlite3.hpp"
#include "schema/event.hpp"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

private class WarGrey::SCADA::AlarmBoard final {
public:
	virtual ~AlarmBoard() noexcept {
		delete this->sqlite3;
	}

	AlarmBoard(AlarmPage* master) : master(master), displayed_record_count(0) {
		this->sqlite3 = new SQLite3();
	}

public:
	void load_and_flow(float width, float height) {
	}

	void update(long long count, long long interval, long long uptime) {
		this->sqlite3->get_logger()->log_message(Log::Info,
			L"count: %lld, interval: %lld, uptime: %lld",
			count, interval, uptime);
	}

private:
	long long displayed_record_count;
		
private:
	SQLite3* sqlite3;
	AlarmPage* master;
};

/*************************************************************************************************/
AlarmPage::AlarmPage(PLCMaster* device, Platform::String^ name) : Planet(name) {}

AlarmPage::~AlarmPage() {
	if (this->dashboard != nullptr) {
		delete this->dashboard;
	}
}

void AlarmPage::load(CanvasCreateResourcesReason reason, float width, float height) {
	if (this->dashboard == nullptr) {
		this->dashboard = new AlarmBoard(this);
		this->dashboard->load_and_flow(width, height);
	}
}

void AlarmPage::update(long long count, long long interval, long long uptime) {
	this->dashboard->update(count, interval, uptime);
	Planet::update(count, interval, uptime);
}

void AlarmPage::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool controlled) {
#ifdef _DEBUG
	Planet::on_tap(g, local_x, local_y, shifted, controlled);
#endif
}
