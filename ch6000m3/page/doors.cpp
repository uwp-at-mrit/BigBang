#include "page/doors.hpp"
#include "configuration.hpp"
#include "dashboard.hpp"
#include "menu.hpp"

#include "graphlet/symbol/doorlet.hpp"
#include "graphlet/dashboard/cylinderlet.hpp"

#include "decorator/page.hpp"

#include "tongue.hpp"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;

private enum DSMode { WindowUI = 0, Dashboard };

private enum class DSOperation { Open, Stop, Close, Disable, _ };

private enum class DS : unsigned int {
	WaterDepth,
	BowDraught, SternDraught,
	F, G, C, B, DoorPressure
};

private class Doors final
	: public PLCConfirmation
	, public IMenuCommand<DSOperation, IMRMaster*>
	, public DashBoard<DoorsPage, DS> {
public:
	Doors(DoorsPage* master) : master(master), DashBoard(master, __FILE__) {
		this->cpt_font = make_text_format("Microsoft YaHei", large_font_size);
	}

public:
	void load(float width, float height, float vinset) {
		float unitsize = 32.0F;

		this->load_label(this->labels, DS::WaterDepth, Colours::Silver, this->cpt_font);
		this->load_dimension(this->dimensions, DS::WaterDepth, "meter");
	}

	void reflow(float width, float height, float vinset) {
		//float unitsize, halfunit, cellsize;
		float label_max_width = 0.0F;
		float offset = vinset * 0.5F;
		float x0 = offset;
		float y0 = vinset + offset;

		this->master->move_to(this->labels[DS::WaterDepth], 100.0F, vinset);

	}

public:
	void execute(DSOperation cmd, IGraphlet* target, IMRMaster* plc) {
		auto door = dynamic_cast<Credit<BottomDoorlet, DS>*>(target);

		if (door != nullptr) {
			plc->get_logger()->log_message(Log::Info, L"%s %s",
				cmd.ToString()->Data(),
				door->id.ToString()->Data());
		}
	}

private: // never delete these graphlets manually.
	std::map<DoorStatus, BottomDoorlet*> doors;
	std::map<DoorStatus, Dimensionlet*> capacities;
	std::map<DS, Credit<Labellet, DS>*> labels;
	std::map<DS, Credit<Dimensionlet, DS>*> dimensions;

private:
	DoorsPage* master;
	CanvasTextFormat^ cpt_font;
};

/*************************************************************************************************/
DoorsPage::DoorsPage(IMRMaster* plc) : Planet(__MODULE__), device(plc) {
	Doors* dashboard = new Doors(this);

	this->dashboard = dashboard;
	this->operation = make_menu<DSOperation, IMRMaster*>(dashboard, plc);
}

DoorsPage::~DoorsPage() {
	if (this->dashboard != nullptr) {
		delete this->dashboard;
	}
}

void DoorsPage::load(CanvasCreateResourcesReason reason, float width, float height) {
	auto db = dynamic_cast<Doors*>(this->dashboard);

	if (db != nullptr) {
		float vinset = statusbar_height();
		
		{ // load graphlets
			this->change_mode(DSMode::Dashboard);
			db->load(width, height, vinset);

			this->change_mode(DSMode::WindowUI);
			this->statusline = new Statuslinelet(default_logging_level);
			this->statusbar = new Statusbarlet(this->name());
			this->insert(this->statusbar);
			this->insert(this->statusline);
		}

		{ // delayed initializing
			this->append_decorator(new PageDecorator());

#ifdef _DEBUG
			this->append_decorator(new GridDecorator(this->gridsize, 0.0F, 0.0F, vinset));
#endif

			if (this->device != nullptr) {
				this->device->get_logger()->append_log_receiver(this->statusline);
			}
		}
	}
}

void DoorsPage::reflow(float width, float height) {
	auto db = dynamic_cast<Doors*>(this->dashboard);
	
	if (db != nullptr) {
		float vinset = statusbar_height();

		this->change_mode(DSMode::WindowUI);
		this->move_to(this->statusline, 0.0F, height, GraphletAnchor::LB);
		
		this->change_mode(DSMode::Dashboard);
		db->reflow(width, height, vinset);
	}
}

bool DoorsPage::can_select(IGraphlet* g) {
	return (dynamic_cast<BottomDoorlet*>(g) != nullptr);
}

void DoorsPage::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool ctrled) {
	Planet::on_tap(g, local_x, local_y, shifted, ctrled);

	if (this->can_select(g)) {
		menu_popup(this->operation, g, local_x, local_y);
	}
}
