#include <map>

#include "page/alarm.hpp"
#include "configuration.hpp"

#include "graphlet/bitmaplet.hpp"
#include "graphlet/textlet.hpp"
#include "graphlet/statuslet.hpp"

#include "tongue.hpp"
#include "text.hpp"

#include "sqlite3.hpp"
#include "schema/event.hpp"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

private class AlarmBoard final {
public:
	AlarmBoard(AlarmPage* master) : master(master) {
	}

public:
	void load_and_flow(float width, float height) {
		this->xterm = this->master->insert_one(new Statuslinelet(Log::Debug, 0));
		
		AlarmEvent events[] = {
			{ 42, "Error", "Fire", 1 , 2 },
	    	{ 43, "Fatal", "Propeller", false, false },
		};

		SQLite3* sqlite3 = new SQLite3();
		sqlite3->get_logger()->append_log_receiver(xterm);

		create_event(sqlite3);
		sqlite3->table_info(L"sqlite_master");
		sqlite3->table_info(L"event");

		insert_event(sqlite3, events, 2);
		
		auto aes = select_event(sqlite3);
		for (auto lt = aes.begin(); lt != aes.end(); lt++) {
			AlarmEvent e = (*lt);

			sqlite3->get_logger()->log_message(Log::Info, L"%d, %s, %s, %d, %d",
				e.uuid, e.name->Data(), e.type->Data(), e.mtime, e.ctime);
		}

		drop_event(sqlite3);
		sqlite3->table_info(L"event");
	}

// never deletes these graphlets mannually
private:
	Statuslinelet* xterm;
		
private:
	AlarmPage* master;
};

/*************************************************************************************************/
std::map<AlarmPage*, AlarmBoard*> dashboards;

AlarmPage::AlarmPage(PLCMaster* device, Platform::String^ name) : Planet(name) {
}

AlarmPage::~AlarmPage() {
	auto maybe_dashboard = dashboards.find(this);

	if (maybe_dashboard != dashboards.end()) {
		delete maybe_dashboard->second;
		dashboards.erase(maybe_dashboard);
	}
}

void AlarmPage::load(CanvasCreateResourcesReason reason, float width, float height) {
	if (dashboards.find(this) == dashboards.end()) {
		AlarmBoard* dashboard = new AlarmBoard(this);

		dashboards.insert(std::pair<AlarmPage*, AlarmBoard*>(this, dashboard));
		dashboard->load_and_flow(width, height);
	}
}

void AlarmPage::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool controlled) {
#ifdef _DEBUG
	Planet::on_tap(g, local_x, local_y, shifted, controlled);
#endif
}
