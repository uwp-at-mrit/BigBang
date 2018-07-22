#include <map>
#include <ppltasks.h>

#include "page/alogbook.hpp"
#include "stone/tongue/logbook.hpp"
#include "configuration.hpp"

#include "graphlet/bitmaplet.hpp"
#include "graphlet/textlet.hpp"

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

private class Eventlet : public IGraphlet {
public:
	Eventlet(CanvasTextFormat^ font) : font(font) {}
	Eventlet(AlarmEvent& ae, CanvasTextFormat^ font) : entity(ae), font(font) {}

public:
	void construct() override {
		this->width = this->info->master->actual_width();

		this->layouts[0] = make_text_layout(dbspeak(event::uuid), this->font);
		this->layouts[1] = make_text_layout(dbspeak(event::name), this->font);
		this->layouts[2] = make_text_layout(dbspeak(event::timestamp), this->font);
		this->layouts[3] = make_text_layout(dbspeak(event::status), this->font);
		this->layouts[4] = make_text_layout(dbspeak(event::code), this->font);
		this->layouts[5] = make_text_layout(dbspeak(event::note), this->font);
	}

	void fill_extent(float x, float y, float* w = nullptr, float* h = nullptr) override {
		SET_BOX(w, this->width);
		SET_BOX(h, this->layouts[0]->LayoutBounds.Height);
	}

	void draw(Microsoft::Graphics::Canvas::CanvasDrawingSession^ ds, float x, float y, float Width, float Height) override {
		float grid_xs[] = { 0.0F, 0.06F, 0.5F, 0.7F, 0.8F, 0.9F };
		float grid_fs[] = { 1.0F, 0.0F,  0.5F, 0.5F, 0.5F, 0.5F };
		size_t count = sizeof(grid_xs)/sizeof(float);
		float gapsize = this->width * 0.01F;
		float grid_total = this->width - gapsize * float(count + 1);
		
		for (size_t idx = 0; idx < count; idx++) {
			float column_x = x + gapsize * float(idx + 1) + grid_total * grid_xs[idx];
			float column_width = (((idx == count - 1) ? 1.0F : grid_xs[idx + 1]) - grid_xs[idx]) * grid_total;
			float layout_x = column_x + (column_width - this->layouts[idx]->LayoutBounds.Width) * grid_fs[idx];

			ds->DrawTextLayout(this->layouts[idx], layout_x, y, Colours::GhostWhite);
		}
	}

private:
	float width;

private:
	CanvasTextFormat^ font;
	CanvasTextLayout^ layouts[_N(event)];
	AlarmEvent entity;
};

private class LogBoard final : public WarGrey::SCADA::PLCConfirmation {
public:
	virtual ~LogBoard() noexcept {
		if (this->sqlite3 != nullptr) {
			delete this->sqlite3;
		}
	}

	LogBoard(LogbookPage* master, long long limit) : master(master), fetching_limit(limit), fetching_offset(0) {
		this->font = make_text_format("Microsoft YaHei", this->master->sketch_to_application_height(24.0F));
	}

public:
	void load_and_flow(float width, float height) {
		CreationCollisionOption cco = CreationCollisionOption::OpenIfExists;
		this->term = this->master->insert_one(new Statuslinelet(Log::Debug, 0U));

		create_task(ApplicationData::Current->LocalFolder->CreateFileAsync("log.ams", cco)).then([=](StorageFile^ file) {
			this->sqlite3 = new SQLite3(file->Path->Data());
			this->sqlite3->get_logger()->append_log_receiver(this->term);
			//dbtest(this->sqlite3);
		});
	}

	void update(long long count, long long interval, long long uptime) {
		AlarmEvent record;
		float x, y, width, height;
		float Height = this->master->actual_height();
		
		this->master->fill_graphlets_boundary(&x, &y, &width, &height);

		default_event(record, count, std::nullopt, uptime, make_nstring(interval.ToString()));

		this->master->enter_critical_section();
		this->master->insert(new Eventlet(record, this->font), 0.0F, Height - height * float(count));
		this->master->leave_critical_section();
	}

private:
	uint64 fetching_offset;
	uint64 fetching_limit;
		
private:
	CanvasTextFormat^ font;
	Statuslinelet* term;
	SQLite3* sqlite3;
	LogbookPage* master;
};

/*************************************************************************************************/
LogbookPage::LogbookPage(PLCMaster* device, Platform::String^ name) : Planet(name) {}

LogbookPage::~LogbookPage() {
	if (this->dashboard != nullptr) {
		delete this->dashboard;
	}
}

void LogbookPage::load(CanvasCreateResourcesReason reason, float width, float height) {
	if (this->dashboard == nullptr) {
		LogBoard* alarmboard = new LogBoard(this, 16);

		alarmboard->load_and_flow(width, height);
		this->dashboard = alarmboard;
	}
}

void LogbookPage::on_elapse(long long count, long long interval, long long uptime) {
	auto alarmboard = static_cast<LogBoard*>(this->dashboard);

	if (alarmboard != nullptr) {
		alarmboard->update(count, interval, uptime);
	}
}

void LogbookPage::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool controlled) {
#ifdef _DEBUG
	Planet::on_tap(g, local_x, local_y, shifted, controlled);
#endif
}
