#include "page/doors.hpp"
#include "configuration.hpp"
#include "dashboard.hpp"
#include "menu.hpp"

#include "graphlet/symbol/doorlet.hpp"

#include "decorator/page.hpp"

#include "tongue.hpp"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;

private enum DSMode { WindowUI = 0, Dashboard };

private enum class DSOperation { Open, Stop, Close, Disable, _ };

private enum class DS : unsigned int {
	F
};

private class Doors final
	: public PLCConfirmation
	, public IMenuCommand<DSOperation, IMRMaster*>
	, public DashBoard<DoorsPage, DS> {
public:
	Doors(DoorsPage* master) : master(master), DashBoard(master, __FILE__) {
		this->font = make_bold_text_format("Microsoft YaHei", 16.0F);
	}

public:
	void load(float width, float height) {
		float unitsize = 32.0F;
		Platform::String^ all_captions[] = { "dumpdoor", "upperdoor" };
		
		for (size_t i = 0; i < sizeof(all_captions)/sizeof(Platform::String^); i++) {
			this->captions[i] = make_label(_speak(all_captions[i]) + ":", this->font);
		}

		this->load_primitives(this->hoppers, this->hlabels, unitsize);
		this->load_primitives(this->doors, this->dlabels, unitsize);
	}

	void reflow(float width, float height, float vinset) {
		float unitsize, halfunit, cellsize;
		float label_max_width = 0.0F;
		float offset = vinset * 0.5F;
		float x0 = offset;
		float y0 = vinset + offset;

		for (size_t i = 0; i < GRAPHLETS_LENGTH(this->captions); i++) {
			if (this->captions[i] != nullptr) {
				float label_width;

				this->captions[i]->fill_extent(0.0F, 0.0F, &label_width);
				label_max_width = fmax(label_max_width, label_width);
			}
		}

		this->hoppers[_E0(DoorStatus)]->fill_extent(0.0F, 0.0F, &unitsize);
		halfunit = unitsize * 0.5F;
		cellsize = unitsize * 1.618F;

		for (size_t i = 0; i < GRAPHLETS_LENGTH(this->captions); i++) {
			if (this->captions[i] != nullptr) {
				float y = y0 + halfunit + float(i) * cellsize;

				this->master->move_to(this->captions[i], x0 + label_max_width, y, GraphletAnchor::RC);
			}
		}

		x0 += (label_max_width + offset + halfunit);
		y0 += unitsize;
		this->reflow_primitives(this->hoppers, this->hlabels, x0, y0 + cellsize * 3.0F, cellsize);
		this->reflow_primitives(this->doors, this->dlabels,   x0, y0 + cellsize * 4.0F, cellsize);
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

private:
	Labellet* make_label(Platform::String^ text, CanvasTextFormat^ font = nullptr) {
		Labellet* label = new Labellet(text, font);

		return this->master->insert_one(label);
	}

	template<typename T, typename S>
	void load_primitives(std::unordered_map<S, T*>& gs, std::unordered_map<S, Labellet*>& ls, float unitsize) {
		for (S s = _E0(S); s < S::_; s++) {
			gs[s] = new T(s, unitsize);
			this->master->insert(gs[s]);

			ls[s] = make_label(_speak(s));
		}
	}

	template<typename T, typename S>
	void reflow_primitives(std::unordered_map<S, T*>& gs, std::unordered_map<S, Labellet*>& ls, float x0, float y, float cellsize) {
		for (S i = _E0(S); i < S::_; i++) {
			float x = x0 + _F(i) * cellsize;

			this->master->move_to(gs[i], x, y, GraphletAnchor::CB);
			this->master->move_to(ls[i], x, y, GraphletAnchor::CT);
		}
	}

private: // never delete these graphlets manually.
	Labellet* captions[5];
	std::unordered_map<DoorStatus, BottomDoorlet*> hoppers;
	std::unordered_map<DoorStatus, Labellet*> hlabels;
	std::unordered_map<DoorStatus, UpperHopperDoorlet*> doors;
	std::unordered_map<DoorStatus, Labellet*> dlabels;

private:
	DoorsPage* master;
	CanvasTextFormat^ font;
};

/*************************************************************************************************/
DoorsPage::DoorsPage(IMRMaster* plc) : Planet(__MODULE__), device(plc) {
	Doors* dashboard = new Doors(this);

	this->dashboard = dashboard;
	this->operation = make_menu<DSOperation, IMRMaster*>(dashboard, plc);
	this->gridsize = statusbar_height();
}

DoorsPage::~DoorsPage() {
	if (this->dashboard != nullptr) {
		delete this->dashboard;
	}
}

void DoorsPage::load(CanvasCreateResourcesReason reason, float width, float height) {
	auto db = dynamic_cast<Doors*>(this->dashboard);

	if (db != nullptr) {
		{ // load graphlets
			this->change_mode(DSMode::Dashboard);
			db->load(width, height);

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

		this->move_to(this->statusline, 0.0F, height, GraphletAnchor::LB);
		
		db->reflow(width, height, vinset);
	}
}

bool DoorsPage::can_select(IGraphlet* g) {
	return (dynamic_cast<BottomDoorlet*>(g) != nullptr);
}

void DoorsPage::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool ctrled) {
	//if (dynamic_cast<Tracklet<HS>*>(g) == nullptr) {
	//	this->set_selected(g);
	//}
	// this->set_caret_owner(g);

	Planet::on_tap(g, local_x, local_y, shifted, ctrled);

	if (this->can_select(g)) {
		menu_popup(this->operation, g, local_x, local_y);
	}
}
