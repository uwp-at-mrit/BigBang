#include <map>

#include "page/alarm.hpp"
#include "configuration.hpp"
#include <optional>

#include "graphlet/bitmaplet.hpp"
#include "graphlet/textlet.hpp"
#include "graphlet/statuslet.hpp"

#include "tongue.hpp"
#include "text.hpp"

#include "sqlite3.hpp"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

//static TableColumnInfo yacht63ft_columns[] = {
//	{ "timestamp", "Integer", true, true, false, nullptr },
//  { "event", "Integer", true, false, false, nullptr }
//};

private class AlarmBoard final {
public:
	AlarmBoard(AlarmPage* master) : master(master) {
	}

public:
	void load_and_flow(float width, float height) {
		this->xterm = this->master->insert_one(new Statuslinelet(Log::Debug, 0));

		SQLite3* sqlite3 = new SQLite3();
		sqlite3->get_logger()->append_log_receiver(xterm);

		//sqlite3->create_table(L"yacht63ft", yacht63ft_columns);
		//sqlite3->table_info(L"sqlite_master");
		//sqlite3->table_info(L"yacht63ft");
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
