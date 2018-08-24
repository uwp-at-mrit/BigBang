#include <map>

#include "page/sealed_waters.hpp"
#include "configuration.hpp"
#include "menu.hpp"

#include "module.hpp"
#include "text.hpp"
#include "paint.hpp"
#include "brushes.hxx"
#include "turtle.hpp"

#include "graphlet/shapelet.hpp"
#include "graphlet/misc/hatchlet.hpp"
#include "graphlet/symbol/pumplet.hpp"
#include "graphlet/symbol/valvelet.hpp"

#include "decorator/page.hpp"
#ifdef _DEBUG
#include "decorator/grid.hpp"
#endif

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

private enum SWMode { WindowUI = 0, Dashboard };

private enum class SWOperation { Start, Stop, Reset, _ };

// WARNING: order matters
private enum class SW : unsigned int {
	// Pumps
	H1, H2, Port, Starboard,
	F1, F2, S13, S24, S15, S26, S17, S28, S19, S20,
	// Valves
	DGV3, DGV4, DGV5, DGV6, DGV7, DGV8, DGV9, DGV10,
	DGV12, DGV11, DGV13, DGV14, DGV15, DGV16, DGV17, DGV18, DGV19, DGV20,
	DGV44, DGV45, DGV46, DGV47, DGV48,
	// Key Labels
	Hatch, ToPipeline,
	// Indicators
	_,
	// anchors used as last jumping points
	d3, d4, d5, d6, d7, d8, d9, d10,
	d12, d11, d13, d14, d15, d16, d17, d18, d19, d20,
	d44, d45, d46, d47, d48,

	// anchors used for unnamed corners
	sea
};

private class SealedWaters final : public PLCConfirmation, public IMenuCommand<SWOperation, IMRMaster*> {
public:
	SealedWaters(SealedWaterPage* master) : master(master) {
		this->label_font = make_bold_text_format("Microsoft YaHei", 12.0F);
		this->number_font = make_bold_text_format("Cambria Math", 20.0F);
		this->unit_font = make_text_format("Microsoft YaHei", 18.0F);
	}

public:
	void on_analog_input_data(const uint8* AI_DB203, size_t count, Syslog* logger) override {
		this->master->enter_critical_section();
		this->master->begin_update_sequence();

		this->master->end_update_sequence();
		this->master->leave_critical_section();
	}

	void on_digital_input(const uint8* DI_db205_X, size_t count, Syslog* logger) {
		this->master->enter_critical_section();
		this->master->begin_update_sequence();

		this->master->end_update_sequence();
		this->master->leave_critical_section();
	}

public:
	void execute(SWOperation cmd, IGraphlet* target, IMRMaster* plc) {
		auto pump = dynamic_cast<Credit<Pumplet, SW>*>(target);

		if (pump != nullptr) {
			plc->get_logger()->log_message(Log::Info, L"%s %s",
				cmd.ToString()->Data(),
				pump->id.ToString()->Data());
		}
	}

public:
	void load_pump_station(float width, float height, float gridsize) {
		Turtle<SW>* pTurtle = new Turtle<SW>(gridsize, false, SW::Hatch);

		pTurtle->move_right(4);
		pTurtle->move_down(1, SW::d12)->move_right(6, SW::DGV12)->move_right(6, SW::F1)->move_right(10);
		pTurtle->move_down(1.5F)->jump_back();
		pTurtle->move_down(3, SW::d11)->move_right(6, SW::DGV11)->move_right(6, SW::F2)->move_right(10);
		pTurtle->move_up(1.5F)->move_right(6, SW::ToPipeline)->jump_back();

		pTurtle->move_down(4, SW::d13)->move_right(6, SW::DGV13)->move_right(6, SW::S13)->move_right(6, SW::DGV3);
		pTurtle->move_right(8)->turn_right_down()->move_down(3)->turn_down_right()->jump_back();
		pTurtle->move_down(4, SW::d14)->move_right(6, SW::DGV14)->move_right(6, SW::S24)->move_right(6, SW::DGV4);
		pTurtle->move_right(12, SW::d44)->move_right(12, SW::DGV44)->move_right(6, SW::H1)->jump_back(SW::d14);

		pTurtle->move_down(4, SW::d15)->move_right(6, SW::DGV15)->move_right(6, SW::S15)->move_right(6, SW::DGV5);
		pTurtle->move_right(12, SW::d45)->move_right(12, SW::DGV45)->move_right(6, SW::H2)->jump_back(SW::d15);
		pTurtle->move_down(4, SW::d16)->move_right(6, SW::DGV16)->move_right(6, SW::S26)->move_right(6, SW::DGV6);
		pTurtle->move_right(8)->turn_right_up()->move_up(3)->turn_up_right()->jump_back();

		pTurtle->move_down(2, SW::sea);

		pTurtle->move_down(2, SW::d17)->move_right(6, SW::DGV17)->move_right(6, SW::S17)->move_right(6, SW::DGV7);
		pTurtle->move_right(8)->turn_right_down()->move_down(3)->turn_down_right()->jump_back();
		pTurtle->move_down(4, SW::d18)->move_right(6, SW::DGV18)->move_right(6, SW::S28)->move_right(6, SW::DGV8);
		pTurtle->move_right(24, SW::DGV46)->move_right(6, SW::Port)->jump_back();

		pTurtle->move_down(4, SW::d19)->move_right(6, SW::DGV19)->move_right(6, SW::S19)->move_right(6, SW::DGV9);
		pTurtle->move_right(24, SW::DGV47)->move_right(6, SW::Starboard)->jump_back();
		pTurtle->move_down(4, SW::d20)->move_right(6, SW::DGV20)->move_right(6, SW::S20)->move_right(6, SW::DGV10);
		pTurtle->move_right(8)->turn_right_up()->move_up(3)->turn_up_right()->jump_back();

		this->station = this->master->insert_one(new Tracklet<SW>(pTurtle, 1.5F, Colours::Gray));
		this->hatch = this->master->insert_one(new Hatchlet(gridsize * 2.5F));
		this->sea = this->master->insert_one(new HLinelet(0.618F, Colours::SeaGreen, make_dash_stroke(CanvasDashStyle::Dash)));
		
		this->load_label(this->captions, SW::Port, Colours::DarkKhaki, this->label_font);
		this->load_label(this->captions, SW::Starboard, Colours::DarkKhaki, this->label_font);
		this->load_label(this->captions, SW::Hatch, Colours::Salmon, this->label_font);
	}

	void load_devices(float width, float height, float gridsize) {
		this->load_devices(this->pumps, this->plabels, SW::F1, SW::S20, gridsize, 0.000, Colours::Salmon);
		this->load_devices(this->valves, this->vlabels, SW::DGV3, SW::DGV47, gridsize, -90.000);
	}

public:
	void reflow_pump_station(float width, float height, float gridsize, float vinset) {
		float cx = width * 0.5F;
		float cy = height * 0.5F;
		float sq1_y, horizon_y;

		this->master->move_to(this->station, cx, cy, GraphletAnchor::CC);
		this->station->map_graphlet_at_anchor(this->hatch, SW::Hatch, GraphletAnchor::LC);
		this->station->fill_anchor_location(SW::DGV12, nullptr, &sq1_y, true);
		
		this->station->map_credit_graphlet(this->captions[SW::Port], GraphletAnchor::CB);
		this->station->map_credit_graphlet(this->captions[SW::Starboard], GraphletAnchor::CB);
		this->master->move_to(this->captions[SW::Hatch], this->hatch, GraphletAnchor::CB, GraphletAnchor::CT);

		this->station->fill_anchor_location(SW::sea, nullptr, &horizon_y, true);
		this->master->move_to(this->sea, 0.0F, horizon_y);
	}
	
	void reflow_devices(float width, float height, float gridsize, float vinset) {
		for (auto it = this->pumps.begin(); it != this->pumps.end(); it++) {
			this->station->map_credit_graphlet(it->second, GraphletAnchor::CC);
			this->master->move_to(this->plabels[it->first], it->second, GraphletAnchor::CT, GraphletAnchor::CB);
		}

		for (auto it = this->valves.begin(); it != this->valves.end(); it++) {
			float dy = gridsize * 0.618F;
		
			this->station->map_credit_graphlet(it->second, GraphletAnchor::CC);
			this->station->map_credit_graphlet(this->vlabels[it->first], GraphletAnchor::CT, 0.0F, dy);
		}
	}

	void reflow_metrics(float width, float height, float gridsize, float vinset) {
		float vgapsize = gridsize * 0.125F;
	}

private:
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
			this->load_label(ls, id, Colours::Silver, this->label_font);
		}
	}

	template<class G, typename E>
	void load_devices(std::map<E, G*>& gs, std::map<E, Credit<Labellet, E>*>& ls
		, E id0, E idn, float radius, double degrees, CanvasSolidColorBrush^ color) {
		this->load_devices(gs, id0, idn, radius, degrees);

		for (E id = id0; id <= idn; id++) {
			this->load_label(ls, id, color, this->label_font);
		}
	}

	template<typename E>
	void load_dimensions(std::map<E, Credit<Dimensionlet, E>*>& ds, E id0, E idn, Platform::String^ unit, Platform::String^ label = nullptr) {
		for (E id = id0; id <= idn; id++) {
			ds[id] = this->master->insert_one(new Credit<Dimensionlet, E>(unit, label, "", this->number_font, this->unit_font), id);
		}
	}

	template<class T, typename E>
	void load_thermometer(std::map<E, Credit<T, E>*>& ts, std::map<E, Credit<Dimensionlet, E>*>& ds, E id, float width) {
		ts[id] = this->master->insert_one(new Credit<T, E>(100.0, width, 0.0F, 2.5F), id);
		ds[id] = this->master->insert_one(new Credit<Dimensionlet, E>("celsius", _speak(id), "", this->number_font, this->unit_font), id);
	}

	template<typename E>
	void load_label(std::map<E, Credit<Labellet, E>*>& ls, E id, CanvasSolidColorBrush^ color, CanvasTextFormat^ font = nullptr) {
		ls[id] = this->master->insert_one(new Credit<Labellet, E>(_speak(id), font, color), id);
	}

// never deletes these graphlets mannually
private:
	Tracklet<SW>* station;
	Hatchlet* hatch;
	HLinelet* sea;
	std::map<SW, Credit<Labellet, SW>*> captions;
	std::map<SW, Credit<Pumplet, SW>*> pumps;
	std::map<SW, Credit<Labellet, SW>*> plabels;
	std::map<SW, Credit<Valvelet, SW>*> valves;
	std::map<SW, Credit<Labellet, SW>*> vlabels;
	std::map<SW, Credit<Dimensionlet, SW>*> pressures;
	std::map<SW, Credit<Dimensionlet, SW>*> temperatures;
	std::map<SW, Credit<Labellet, SW>*> islabels;
	
private:
	CanvasTextFormat^ label_font;
	CanvasTextFormat^ number_font;
	CanvasTextFormat^ unit_font;

private:
	SealedWaterPage* master;
};

SealedWaterPage::SealedWaterPage(IMRMaster* plc) : Planet(__MODULE__), device(plc) {
	SealedWaters* dashboard = new SealedWaters(this);

	this->dashboard = dashboard;
	this->operation = make_menu<SWOperation, IMRMaster*>(dashboard, plc);
	this->gridsize = statusbar_height();

	this->device->append_confirmation_receiver(dashboard);

	{ // load decorators
		this->append_decorator(new PageDecorator());

#ifdef _DEBUG
		this->append_decorator(new GridDecorator(this->gridsize, 0.0F, 0.0F, this->gridsize));
#endif
	}
}

SealedWaterPage::~SealedWaterPage() {
	if (this->dashboard != nullptr) {
		delete this->dashboard;
	}
}

void SealedWaterPage::load(CanvasCreateResourcesReason reason, float width, float height) {
	auto dashboard = dynamic_cast<SealedWaters*>(this->dashboard);
	
	if (dashboard != nullptr) {
		float vinset = statusbar_height();

		{ // load graphlets
			this->change_mode(SWMode::Dashboard);
			dashboard->load_pump_station(width, height, this->gridsize);
			dashboard->load_devices(width, height, this->gridsize);

			this->change_mode(SWMode::WindowUI);
			this->statusline = new Statuslinelet(default_logging_level);
			this->statusbar = new Statusbarlet(this->name(), this->device);
			this->insert(this->statusbar);
			this->insert(this->statusline);
		}

		if (this->device != nullptr) {
			this->device->get_logger()->append_log_receiver(this->statusline);
		}
	}
}

void SealedWaterPage::reflow(float width, float height) {
	auto dashboard = dynamic_cast<SealedWaters*>(this->dashboard);
	
	if (dashboard != nullptr) {
		float vinset = statusbar_height();

		this->change_mode(SWMode::WindowUI);
		this->move_to(this->statusline, 0.0F, height, GraphletAnchor::LB);

		this->change_mode(SWMode::Dashboard);
		dashboard->reflow_pump_station(width, height, this->gridsize, vinset);
		dashboard->reflow_devices(width, height, this->gridsize, vinset);
		dashboard->reflow_metrics(width, height, this->gridsize, vinset);
	}
}

bool SealedWaterPage::can_select(IGraphlet* g) {
	return (dynamic_cast<Pumplet*>(g) != nullptr);
}

void SealedWaterPage::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool ctrled) {
	Planet::on_tap(g, local_x, local_y, shifted, ctrled);

	if (this->can_select(g)) {
		menu_popup(this->operation, g, local_x, local_y);
	}
}
