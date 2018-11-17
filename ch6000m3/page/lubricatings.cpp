#include <map>

#include "page/lubricatings.hpp"
#include "configuration.hpp"
#include "menu.hpp"

#include "module.hpp"
#include "text.hpp"
#include "paint.hpp"
#include "brushes.hxx"
#include "turtle.hpp"

#include "graphlet/shapelet.hpp"

#include "graphlet/symbol/pump/hydraulic_pumplet.hpp"

#include "schema/ai_pumps.hpp"
#include "schema/di_pumps.hpp"
#include "schema/do_pumps.hpp"

#include "decorator/page.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI::Xaml::Controls;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

private enum LUMode { WindowUI = 0, Dashboard };

private enum class LUGroup { MasterPumps, PSPumps, SBPumps, VisorPumps, _ };
private enum class LUGPOperation { Start, Stop, Cancel, _ };

private enum class HSPOperation { Start, Stop, Reset, _ };
private enum class HSHOperation { Start, Stop, Cancel, Auto, _ };

static CanvasSolidColorBrush^ region_border = Colours::Silver;
static CanvasSolidColorBrush^ region_background = Colours::make(0x006464);

// WARNING: order matters
private enum class LU : unsigned int {
	// Key Labels
	PSUnit, SBUnit, PSPump, SBPump, Master, Spare,
	
	_
};

private class Lubricatings final
	: public PLCConfirmation
	/*, public IMenuCommand<HSPOperation, Credit<HydraulicPumplet, LU>, PLCMaster*>
	, public IGroupMenuCommand<LUGPOperation, LUGroup, PLCMaster*> */ {
public:
	Lubricatings(LubricatingsPage* master) : master(master) {
		this->caption_font = make_bold_text_format("Microsoft YaHei", large_font_size);
		this->label_font = make_bold_text_format("Microsoft YaHei", small_font_size);

		this->dimension_style.number_font = make_bold_text_format("Cambria Math", large_metrics_font_size);
		this->dimension_style.unit_font = make_bold_text_format("Cambria", normal_font_size);
		this->dimension_style.precision = 0;
	}

public:
	void pre_read_data(Syslog* logger) override {
		this->master->enter_critical_section();
		this->master->begin_update_sequence();
	}

	void on_digital_input(const uint8* DB4, size_t count4, const uint8* DB205, size_t count205, Syslog* logger) {
	}

	void post_read_data(Syslog* logger) override {
		this->master->end_update_sequence();
		this->master->leave_critical_section();
	}

public:
	void load(float width, float height, float vinset) {
		float region_width = width * 0.5F * 0.618F;
		float region_height = height * 0.8F;

		this->ps_region = this->master->insert_one(new Rectanglet(region_width, region_height, region_background, region_border, 2.0F));
		this->sb_region = this->master->insert_one(new Rectanglet(region_width, region_height, region_background, region_border, 2.0F));

		//this->load_pump_station(this->stations, LU::PSPump, vinset, vinset);
		//this->load_pump_station(this->stations, LU::SBPump, vinset, vinset);
	}

public:
	void reflow(float width, float height, float vinset) {
		float ps_cx = width * 0.25F;
		float sb_cx = width * 0.75F;
		float cy = height * 0.5F;

		this->master->move_to(this->ps_region, ps_cx, cy, GraphletAnchor::CC);
		this->master->move_to(this->sb_region, sb_cx, cy, GraphletAnchor::CC);

		//this->master->move_to(this->stations[LU::PSPump], this->ps_region, 0.5F, 0.75F, GraphletAnchor::CC);
		//this->master->move_to(this->stations[LU::SBPump], this->sb_region, 0.5F, 0.75F, GraphletAnchor::CC);
	}

private:
	template<class T, typename E>
	void load_pump_station(std::map<E, T*>& ts, E id, float xstep, float ystep) {
		Turtle<E>* turtle = new Turtle<E>(xstep, ystep);

		turtle->move_down()->move_left(3);
		turtle->move_down(3, E::Master)->move_down(3)->move_right(6);
		turtle->move_up(3, E::Spare)->move_up(3)->move_left(3);

		ts[id] = this->master->insert_one(new T(turtle, default_pipe_thickness, default_pipe_color), id);
	}

	template<class G, typename E>
	void load_devices(std::map<E, G*>& gs, E id0, E idn, float radius, double degrees) {
		for (E id = id0; id <= idn; id++) {
			gs[id] = this->master->insert_one(new G(radius, degrees), id);
		}
	}

	template<class G, typename E>
	void load_devices(std::map<E, G*>& gs, std::map<E, Credit<Labellet, E>*>& ls, E id0, E idn, float radius, double degrees) {
		this->load_devices(gs, id0, idn, radius, degrees);

		for (E id = id0; id <= idn; id++) {
			this->load_label(ls, id.ToString(), id, Colours::Silver);
		}
	}

	template<class G, typename E>
	void load_devices(std::map<E, G*>& gs, std::map<E, Credit<Labellet, E>*>& ls, std::map<E, Credit<Labellet, E>*>& cs
		, E id0, E idn, float radius, double degrees, CanvasSolidColorBrush^ color = Colours::Silver) {
		this->load_devices(gs, id0, idn, radius, degrees);

		for (E id = id0; id <= idn; id++) {
			this->load_label(ls, id.ToString(), id, color);
			this->load_label(cs, id, color);
		}
	}

	template<typename E>
	void load_label(std::map<E, Credit<Labellet, E>*>& ls, Platform::String^ caption, E id
		, CanvasSolidColorBrush^ color, CanvasTextFormat^ font = nullptr) {
		ls[id] = this->master->insert_one(new Credit<Labellet, E>(caption, ((font == nullptr) ? this->label_font : font), color), id);
	}

	template<typename E>
	void load_label(std::map<E, Credit<Labellet, E>*>& ls, E id, CanvasSolidColorBrush^ color, CanvasTextFormat^ font = nullptr) {
		this->load_label(ls, _speak(id), id, color, font);
	}

// never deletes these graphlets mannually
private:
	std::map<LU, Credit<Tracklet<LU>, LU>*> stations;
	std::map<LU, Credit<Labellet, LU>*> ps_labels;
	std::map<LU, Credit<Labellet, LU>*> sb_labels;
	std::map<LU, Credit<HydraulicPumplet, LU>*> ps_pumps;
	std::map<LU, Credit<HydraulicPumplet, LU>*> sb_pumps;
	Rectanglet* ps_region;
	Rectanglet* sb_region;
	
private:
	CanvasTextFormat^ caption_font;
	CanvasTextFormat^ label_font;
	DimensionStyle dimension_style;

private:
	LubricatingsPage* master;
};

LubricatingsPage::LubricatingsPage(PLCMaster* plc) : Planet(__MODULE__), device(plc) {
	Lubricatings* dashboard = new Lubricatings(this);

	this->dashboard = dashboard;
	
	this->device->append_confirmation_receiver(dashboard);
	this->append_decorator(new PageDecorator());
}

LubricatingsPage::~LubricatingsPage() {
	if (this->dashboard != nullptr) {
		delete this->dashboard;
	}
}

void LubricatingsPage::load(CanvasCreateResourcesReason reason, float width, float height) {
	auto dashboard = dynamic_cast<Lubricatings*>(this->dashboard);
	
	if (dashboard != nullptr) {
		float vinset = statusbar_height();

		{ // load graphlets
			this->change_mode(LUMode::Dashboard);
			dashboard->load(width, height, vinset);

			this->change_mode(LUMode::WindowUI);
			this->statusbar = this->insert_one(new Statusbarlet(this->name(), this->device));
			this->statusline = this->insert_one(new Statuslinelet(default_logging_level));
		}

		{ // delayed initializing
			this->get_logger()->append_log_receiver(this->statusline);

			if (this->device != nullptr) {
				this->device->get_logger()->append_log_receiver(this->statusline);
			}
		}
	}
}

void LubricatingsPage::reflow(float width, float height) {
	auto dashboard = dynamic_cast<Lubricatings*>(this->dashboard);
	
	if (dashboard != nullptr) {
		float vinset = statusbar_height();
		
		this->change_mode(LUMode::WindowUI);
		this->move_to(this->statusline, 0.0F, height, GraphletAnchor::LB);

		this->change_mode(LUMode::Dashboard);
		dashboard->reflow(width, height, vinset);
	}
}

bool LubricatingsPage::can_select(IGraphlet* g) {
	return ((dynamic_cast<HydraulicPumplet*>(g) != nullptr));
}

bool LubricatingsPage::can_select_multiple() {
	return true;
}

void LubricatingsPage::on_tap_selected(IGraphlet* g, float local_x, float local_y) {
	auto pump = dynamic_cast<HydraulicPumplet*>(g);
	
	if (pump != nullptr) {
		menu_popup(this->pump_op, g, local_x, local_y);
	}
}

void LubricatingsPage::on_gesture(std::list<float2>& anchors, float x, float y) {
	auto dashboard = dynamic_cast<Lubricatings*>(this->dashboard);

	if (dashboard != nullptr) {
	}
}
