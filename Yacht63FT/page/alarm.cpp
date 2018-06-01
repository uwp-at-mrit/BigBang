#include <map>

#include "page/alarm.hpp"
#include "configuration.hpp"

#include "graphlet/bitmaplet.hpp"
#include "graphlet/textlet.hpp"
#include "graphlet/datalet.hpp"

#include "tongue.hpp"
#include "text.hpp"

#include "sqlite3.hpp"
#include "schema/event.hpp"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

private class AlarmProvider : public IDataProvider {
public:
	AlarmProvider() {
		this->sqlite3 = new SQLite3();

		create_event(this->sqlite3, true);
	}

private:
	SQLite3* sqlite3;
};

private class AlarmBoard final {
public:
	AlarmBoard(AlarmPage* master) : master(master) {}

public:
	void load_and_flow(float width, float height) {
		this->datasource = new AlarmProvider();
		this->view = this->master->insert_one(new ListViewlet(this->datasource));
	}

// never deletes these graphlets mannually
private:
	IDataViewlet* view;
		
private:
	AlarmProvider* datasource;
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
