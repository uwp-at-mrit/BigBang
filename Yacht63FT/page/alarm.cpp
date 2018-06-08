#include <map>
#include <ppltasks.h>

#include "page/alarm.hpp"
#include "configuration.hpp"

#include "graphlet/bitmaplet.hpp"
#include "graphlet/textlet.hpp"

#include "tongue.hpp"
#include "text.hpp"
#include "string.hpp"
#include "path.hpp"

#include "sqlite3.hpp"
#include "schema/event.hpp"
#include "graphlet/statuslet.hpp"
#include "schema/dbtest.hpp"

using namespace WarGrey::SCADA;

using namespace Concurrency;

using namespace Windows::Storage;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

private class AlarmEventlet : public IGraphlet {
public:
	AlarmEventlet(AlarmEvent& ae, CanvasTextFormat^ font) : entity(ae), font(font) {}

public:
	void construct() override {
		
	}

	void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override {
		if (this->info != nullptr) {
			this->info->master->fill_actual_extent(w, nullptr);

		}
	}

	void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override {

	}

private:
	CanvasTextFormat^ font;
	CanvasTextLayout^ layout;
	AlarmEvent entity;
};

private class AlarmBoard final : public WarGrey::SCADA::PLCConfirmation {
public:
	virtual ~AlarmBoard() noexcept {
		if (this->sqlite3 != nullptr) {
			delete this->sqlite3;
		}
	}

	AlarmBoard(AlarmPage* master, long long limit) : master(master), fetching_limit(limit), fetching_offset(0) {
		this->font = make_text_format("Microsoft YaHei", design_to_application_height(24.0F));
	}

public:
	void load_and_flow(float width, float height) {
		CreationCollisionOption cco = CreationCollisionOption::OpenIfExists;
		this->term = this->master->insert_one(new Statuslinelet(Log::Debug, 0U));

		create_task(ApplicationData::Current->LocalFolder->CreateFileAsync("log.db", cco)).then([=](StorageFile^ file) {
			this->sqlite3 = new SQLite3(file->Path->Data());
			this->sqlite3->get_logger()->append_log_receiver(this->term);
			dbtest(this->sqlite3);
		});
	}

	void update(long long count, long long interval, long long uptime) {
		float x, y, width, height, Height;
		
		this->master->fill_graphlets_bounds(&x, &y, &width, &height);

		Labellet* record = new Labellet(L"[%f, %f]@(%f, %f)", width, height, x, y);
		record->set_font(this->font);
		this->master->fill_actual_extent(nullptr, &Height);
		record->fill_extent(0.0F, 0.0F, nullptr, &height);
		this->master->enter_critical_section();
		this->master->insert(record, 0.0F, Height - height * float(count));
		this->master->leave_critical_section();
	}

private:
	uint64 fetching_offset;
	uint64 fetching_limit;
		
private:
	CanvasTextFormat^ font;
	Statuslinelet* term;
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
		AlarmBoard* alarmboard = new AlarmBoard(this, 16);

		alarmboard->load_and_flow(width, height);
		this->dashboard = alarmboard;
	}
}

void AlarmPage::update(long long count, long long interval, long long uptime) {
	auto alarmboard = static_cast<AlarmBoard*>(this->dashboard);

	if (alarmboard != nullptr) {
		//alarmboard->update(count, interval, uptime);
	}
}

void AlarmPage::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool controlled) {
#ifdef _DEBUG
	Planet::on_tap(g, local_x, local_y, shifted, controlled);
#endif
}
