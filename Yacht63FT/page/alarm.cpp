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
		
		AlarmEvent fevent = make_event("Yacht", "Info");
		AlarmEvent events[2];
		AlarmEvent_pk id = event_identity(fevent);

		default_event(events[0], "Fire", "Error");
		default_event(events[1], "Propeller", "Fatal");

		SQLite3* sqlite3 = new SQLite3();
		sqlite3->get_logger()->append_log_receiver(xterm);

		create_event(sqlite3);
		sqlite3->table_info("sqlite_master");
		sqlite3->table_info("event");

		insert_event(sqlite3, fevent);
		insert_event(sqlite3, events);
		
		auto aes = list_event(sqlite3);
		for (auto lt = aes.begin(); lt != aes.end(); lt++) {
			AlarmEvent_pk pk = (*lt);
			std::optional<AlarmEvent> maybe_e = seek_event(sqlite3, pk);
			
			if (maybe_e.has_value()) {
				AlarmEvent e = maybe_e.value();

				sqlite3->get_logger()->log_message(Log::Info, L"%lld, %S, %S, %lld, %lld",
					e.uuid, e.name.c_str(), e.type.c_str(),
					e.mtime.value_or(false), e.ctime.value_or(false));
			}
		}

		delete_event(sqlite3, id);
		if (!seek_event(sqlite3, id).has_value()) {
			sqlite3->get_logger()->log_message(Log::Info, "`seek_table` works for absent record");
		}

		drop_event(sqlite3);
		sqlite3->table_info("event");
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
