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
#include "graphlet/symbol/door/hatchlet.hpp"
#include "graphlet/symbol/pump/hydraulic_pumplet.hpp"
#include "graphlet/symbol/pump/hopper_pumplet.hpp"
#include "graphlet/symbol/valve/gate_valvelet.hpp"
#include "graphlet/symbol/valve/manual_valvelet.hpp"

#include "decorator/page.hpp"

using namespace WarGrey::SCADA;
using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

private enum SWMode { WindowUI = 0, Dashboard };

private enum class SWPOperation { Start, Stop, Reset, Auto, _ };
private enum class SWVOperation { Open, Close, FakeOpen, FakeClose, _ };

static CanvasSolidColorBrush^ water_color = Colours::Green;

// WARNING: order matters
private enum class SW : unsigned int {
	// Pumps
	PSHP, SBHP, PSUWP, SBUWP,
	FP1, FP2, SP13, SP14, SP15, SP16, SP17, SP18, SP19, SP20,

	// Manual Valves
	DGV3, DGV4, DGV5, DGV6, DGV1, DGV2, DGV9, DGV10,
	DGV12, DGV11, DGV13, DGV14, DGV15, DGV16, DGV17, DGV18, DGV19, DGV20,
	DGV7, DGV8,
	
	// Gate Valves
	DGV44, DGV45, DGV46, DGV47,
	
	// Labels
	ToFlushs, SS1, SS2, Hatch, Sea,
	
	// Special Arrows
	UA1, UA2, DA1, DA2,
	
	_,
	
	// anchors used as last jumping points
	d3, d4, d5, d6,
	d12, d11, d13, d14, d15, d16, d17, d18, d19, d20,
	d44, d45, d46, d47,

	// anchors used for unnamed corners
	flushs
};

private class SealedWaters final
	: public PLCConfirmation
	, public IMenuCommand<SWPOperation, Credit<HydraulicPumplet, SW>, IMRMaster*>
	, public IMenuCommand<SWVOperation, Credit<GateValvelet, SW>, IMRMaster*> {
public:
	SealedWaters(SealedWaterPage* master) : master(master), sea_oscillation(1.0F) {
		this->label_font = make_bold_text_format("Microsoft YaHei", small_font_size);
		this->dimension_style = make_highlight_dimension_style(large_font_size, 6U);
	}

public:
	void pre_read_data(Syslog* logger) override {
		this->master->enter_critical_section();
		this->master->begin_update_sequence();

		this->station->clear_subtacks();
	}

	void on_analog_input_data(const uint8* AI_DB203, size_t count, Syslog* logger) override {
		this->pressures[SW::FP1]->set_value(RealData(AI_DB203, 34U), GraphletAnchor::LB);

		// for Port hopper pump
		this->pressures[SW::DGV8]->set_value(RealData(AI_DB203, 28U), GraphletAnchor::LB);
		this->flows[SW::DGV8]->set_value(RealData(AI_DB203, 29U), GraphletAnchor::LT);

		// for Starboard hopper pump
		this->pressures[SW::DGV7]->set_value(RealData(AI_DB203, 44U), GraphletAnchor::LB);
		this->flows[SW::DGV7]->set_value(RealData(AI_DB203, 45U), GraphletAnchor::LB);

		// for Port Underwater Pump
		this->pressures[SW::DGV1]->set_value(RealData(AI_DB203, 112U), GraphletAnchor::LB);
		this->pressures[SW::DGV2]->set_value(RealData(AI_DB203, 113U), GraphletAnchor::LB);

		// for Starboard Underwater Pump
		this->pressures[SW::DGV9]->set_value(RealData(AI_DB203, 118U), GraphletAnchor::LB);
		this->pressures[SW::DGV10]->set_value(RealData(AI_DB203, 119U), GraphletAnchor::LB);
	}

	void on_digital_input(const uint8* DI_db205_X, size_t count, Syslog* logger) {
	}

	void post_read_data(Syslog* logger) override {
		this->station->append_subtrack(SW::Hatch, SW::DGV20, water_color);

		this->master->end_update_sequence();
		this->master->leave_critical_section();
	}

public:
	void execute(SWPOperation cmd, Credit<HydraulicPumplet, SW>* pump, IMRMaster* plc) {
		plc->get_logger()->log_message(Log::Info, L"%s %s",
			cmd.ToString()->Data(),
			pump->id.ToString()->Data());
	}

	void execute(SWVOperation cmd, Credit<GateValvelet, SW>* valve, IMRMaster* plc) {
		plc->get_logger()->log_message(Log::Info, L"%s %s",
			cmd.ToString()->Data(),
			valve->id.ToString()->Data());
	}

public:
	void load(float width, float height, float gwidth, float gheight) {
		Turtle<SW>* pTurtle = new Turtle<SW>(gwidth, gheight, false, SW::Hatch);

		pTurtle->move_right(3);
		pTurtle->move_down(1, SW::d12)->move_right(5, SW::DGV12)->move_right(6, SW::FP1);
		pTurtle->move_right(10)->move_down(2)->jump_back();
		pTurtle->move_down(4, SW::d11)->move_right(5, SW::DGV11)->move_right(6, SW::FP2);
		pTurtle->move_right(10)->move_up(2)->move_right(3, SW::ToFlushs)->move_right(3, SW::flushs)->jump_back(SW::d11);

		pTurtle->move_down(4, SW::d13)->move_right(5, SW::DGV13)->move_right(6, SW::SP13)->move_right(6, SW::DGV3);
		pTurtle->move_right(10)->turn_right_down()->move_down(1.5F, SW::DA1)->move_down(1.5F)->turn_down_right()->jump_back();
		pTurtle->move_down(4, SW::d14)->move_right(5, SW::DGV14)->move_right(6, SW::SP14)->move_right(6, SW::DGV4);
		pTurtle->move_right(14, SW::d44)->move_right(9, SW::DGV44)->move_right(10, SW::PSHP)->jump_back();
		pTurtle->move_up_right(2.5F, SW::SS1)->move_up_right(2.5F)->move_right(4, SW::DGV8);
		pTurtle->move_right(10)->move_down(5)->jump_back(SW::d14);

		pTurtle->move_down(5, SW::d15)->move_right(5, SW::DGV15)->move_right(6, SW::SP15)->move_right(6, SW::DGV5);
		pTurtle->move_right(14, SW::d45)->move_right(9, SW::DGV45)->move_right(10, SW::SBHP)->jump_back();
		pTurtle->move_down_right(2.5F, SW::SS2)->move_down_right(2.5F)->move_right(4, SW::DGV7);
		pTurtle->move_right(10)->move_up(5)->jump_back(SW::d15);
		pTurtle->move_down(4, SW::d16)->move_right(5, SW::DGV16)->move_right(6, SW::SP16)->move_right(6, SW::DGV6);
		pTurtle->move_right(10)->turn_right_up()->move_up(1.5F, SW::UA1)->move_up(1.5F)->turn_up_right()->jump_back();

		pTurtle->jump_down(3)->jump_left(SW::Sea)->move_right();

		pTurtle->move_down(2, SW::d17)->move_right(5, SW::DGV17)->move_right(6, SW::SP17)->move_right(6, SW::DGV1);
		pTurtle->move_right(10)->turn_right_down()->move_down(1.5F, SW::DA2)->move_down(1.5F)->turn_down_right()->jump_back();
		pTurtle->move_down(4, SW::d18)->move_right(5, SW::DGV18)->move_right(6, SW::SP18)->move_right(6, SW::DGV2);
		pTurtle->move_right(14, SW::d46)->move_right(9, SW::DGV46)->move_right(10, SW::PSUWP)->jump_back(SW::d18);

		pTurtle->move_down(5, SW::d19)->move_right(5, SW::DGV19)->move_right(6, SW::SP19)->move_right(6, SW::DGV9);
		pTurtle->move_right(14, SW::d47)->move_right(9, SW::DGV47)->move_right(10, SW::SBUWP)->jump_back(SW::d19);
		pTurtle->move_down(4, SW::d20)->move_right(5, SW::DGV20)->move_right(6, SW::SP20)->move_right(6, SW::DGV10);
		pTurtle->move_right(10)->turn_right_up()->move_up(1.5F, SW::UA2)->move_up(1.5F)->turn_up_right()->jump_back();

		this->station = this->master->insert_one(new Tracklet<SW>(pTurtle, default_pipe_thickness, default_pipe_color));
		this->to_flushs = this->master->insert_one(new ArrowHeadlet(gheight, 0.0, Colours::Silver));
		this->sea = this->master->insert_one(new VLinelet(gheight * 2.0F, default_pipe_thickness,
			water_color, make_dash_stroke(CanvasDashStyle::Dash)));
		
		{ // load devices
			float radius = resolve_gridsize(gwidth, gheight);
			float hpradius = std::fminf(gwidth, gheight);

			this->hatch = this->master->insert_one(new Hatchlet(hpradius * 2.5F));

			this->load_devices(this->mvalves, this->labels, Colours::Silver, SW::DGV3, SW::DGV8, radius, 0.0);
			this->load_devices(this->gvalves, this->labels, Colours::Silver, SW::DGV44, SW::DGV47, radius, 0.0);
			this->load_devices(this->pumps, this->labels, Colours::Salmon, SW::FP1, SW::SP20, radius, 0.0);

			this->load_device(this->hoppers, this->captions, SW::PSHP, hpradius, -2.0F);
			this->load_device(this->hoppers, this->captions, SW::SBHP, hpradius, 2.0F);
			this->load_device(this->hoppers, this->captions, SW::PSUWP, hpradius, 2.0F);
			this->load_device(this->hoppers, this->captions, SW::SBUWP, hpradius, -2.0F);
		}

		{ // load labels and dimensions
			this->load_labels(this->captions, SW::ToFlushs, SW::SS2, Colours::Silver);
			this->load_labels(this->captions, SW::Hatch, SW::Sea, Colours::Salmon);

			this->load_dimensions(this->pressures, SW::FP1, SW::FP2, "bar", "P");
			this->load_dimensions(this->pressures, SW::DGV1, SW::DGV10, "bar", "P");
			this->load_dimensions(this->pressures, SW::DGV7, SW::DGV8, "bar", "P");
			this->load_dimensions(this->flows, SW::DGV7, SW::DGV8, "m3ph", "F");
		}
	}

public:
	void reflow(float width, float height, float gwidth, float gheight, float vinset) {
		float text_hspace = vinset * 0.125F;
		
		this->master->move_to(this->station, width * 0.5F, height * 0.5F, GraphletAnchor::CC, -gwidth * 2.0F);
		this->station->map_credit_graphlet(this->captions[SW::Sea], GraphletAnchor::RC, -text_hspace);
		this->station->map_graphlet_at_anchor(this->sea, SW::Sea, GraphletAnchor::LC, 0.0F, this->sea_oscillation);
		this->station->map_graphlet_at_anchor(this->hatch, SW::Hatch, GraphletAnchor::LC);
		this->master->move_to(this->captions[SW::Hatch], this->hatch, GraphletAnchor::CB, GraphletAnchor::CT);

		this->station->map_graphlet_at_anchor(this->to_flushs, SW::flushs, GraphletAnchor::CC);
		this->station->map_credit_graphlet(this->captions[SW::ToFlushs], GraphletAnchor::CB);
		
		this->station->map_credit_graphlet(this->captions[SW::SS1], GraphletAnchor::LC);
		this->station->map_credit_graphlet(this->captions[SW::SS2], GraphletAnchor::LC);

		{ // reflow devices
			float gridsize = resolve_gridsize(gwidth, gheight);
			
			this->reflow_valves(this->mvalves, this->labels, gridsize);
			this->reflow_valves(this->gvalves, this->labels, gridsize);
			
			for (auto it = this->pumps.begin(); it != this->pumps.end(); it++) {
				this->station->map_credit_graphlet(it->second, GraphletAnchor::CC);
				this->master->move_to(this->labels[it->first], it->second, GraphletAnchor::CT, GraphletAnchor::CB);
			}

			for (auto it = this->hoppers.begin(); it != this->hoppers.end(); it++) {
				this->station->map_credit_graphlet(it->second, GraphletAnchor::LC);
				this->master->move_to(this->captions[it->first], it->second,
					GraphletAnchor::RC, GraphletAnchor::LC, text_hspace);
			}
		}

		{ // reflow dimensions
			float xoff = gwidth * 3.0F;
			float yoff = default_pipe_thickness * 2.0F;

			for (auto it = this->pressures.begin(); it != this->pressures.end(); it++) {
				this->station->map_credit_graphlet(it->second, GraphletAnchor::LB, xoff, -yoff);
			}

			for (auto it = this->flows.begin(); it != this->flows.end(); it++) {
				this->station->map_credit_graphlet(it->second, GraphletAnchor::LT, xoff, yoff);
			}
		}
	}

public:
	void on_elapse(long long count, long long interval, long long uptime) {
		this->sea_oscillation *= -1.0F;
		this->master->move(this->sea, 0.0F, this->sea_oscillation);
		this->master->notify_graphlet_updated(this->sea);
	}

private:
	template<class G, typename E>
	void load_devices(std::map<E, G*>& gs, E id0, E idn, float radius, double degrees) {
		for (E id = id0; id <= idn; id++) {
			gs[id] = this->master->insert_one(new G(radius, degrees), id);
		}
	}

	template<class G, typename E>
	void load_devices(std::map<E, G*>& gs, std::map<E, Credit<Labellet, E>*>& ls, CanvasSolidColorBrush^ color
		, E id0, E idn, float radius, double degrees) {
		this->load_devices(gs, id0, idn, radius, degrees);
		this->load_labels(ls, id0, idn, color);
	}

	template<class G, typename E>
	void load_device(std::map<E, G*>& gs, std::map<E, Credit<Labellet, E>*>& ls, E id, float rx, float fy) {
		this->load_label(ls, id, Colours::Salmon);

		gs[id] = this->master->insert_one(new G(rx, std::fabsf(rx) * fy), id);
	}

	template<typename E>
	void load_dimensions(std::map<E, Credit<Dimensionlet, E>*>& ds, E id0, E idn, Platform::String^ unit, Platform::String^label) {
		for (E id = id0; id <= idn; id++) {
			ds[id] = this->master->insert_one(new Credit<Dimensionlet, E>(this->dimension_style, unit, label), id);
		}
	}

	template<typename E>
	void load_label(std::map<E, Credit<Labellet, E>*>& ls, E id, CanvasSolidColorBrush^ color) {
		ls[id] = this->master->insert_one(new Credit<Labellet, E>(_speak(id), this->label_font, color), id);
	}

	template<typename E>
	void load_labels(std::map<E, Credit<Labellet, E>*>& ls, E id0, E idn, CanvasSolidColorBrush^ color) {
		for (E id = id0; id <= idn; id++) {
			this->load_label(ls, id, color);
		}
	}

private:
	template<class G, typename E>
	void reflow_valves(std::map<E, G*>& gs, std::map<E, Credit<Labellet, E>*>& ls, float gridsize) {
		float margin;

		for (auto it = gs.begin(); it != gs.end(); it++) {
			it->second->fill_margin(0.0F, 0.0F, nullptr, nullptr, &margin, nullptr);
			this->station->map_credit_graphlet(it->second, GraphletAnchor::CC);
			this->station->map_credit_graphlet(ls[it->first], GraphletAnchor::CT, 0.0F, gridsize - margin);
		}
	}

// never deletes these graphlets mannually
private:
	Tracklet<SW>* station;
	Hatchlet* hatch;
	ArrowHeadlet* to_flushs;
	Linelet* sea;
	std::map<SW, Credit<Labellet, SW>*> captions;
	std::map<SW, Credit<Labellet, SW>*> labels;
	std::map<SW, Credit<HydraulicPumplet, SW>*> pumps;
	std::map<SW, Credit<HopperPumplet, SW>*> hoppers;
	std::map<SW, Credit<ManualValvelet, SW>*> mvalves;
	std::map<SW, Credit<GateValvelet, SW>*> gvalves;
	std::map<SW, Credit<Dimensionlet, SW>*> pressures;
	std::map<SW, Credit<Dimensionlet, SW>*> flows;
	
private:
	CanvasTextFormat^ label_font;
	DimensionStyle dimension_style;
	float sea_oscillation;

private:
	SealedWaterPage* master;
};

SealedWaterPage::SealedWaterPage(IMRMaster* plc) : Planet(__MODULE__), device(plc) {
	SealedWaters* dashboard = new SealedWaters(this);

	this->dashboard = dashboard;
	this->pump_op = make_menu<SWPOperation, Credit<HydraulicPumplet, SW>, IMRMaster*>(dashboard, plc);
	this->valve_op = make_menu<SWVOperation, Credit<GateValvelet, SW>, IMRMaster*>(dashboard, plc);
	this->grid = new GridDecorator();

	this->device->append_confirmation_receiver(dashboard);

	{ // load decorators
		this->append_decorator(new PageDecorator());

#ifdef _DEBUG
		this->append_decorator(this->grid);
#else
		this->grid->set_active_planet(this);
#endif
	}
}

SealedWaterPage::~SealedWaterPage() {
	if (this->dashboard != nullptr) {
		delete this->dashboard;
	}

#ifndef _DEBUG
	delete this->grid;
#endif
}

void SealedWaterPage::load(CanvasCreateResourcesReason reason, float width, float height) {
	auto dashboard = dynamic_cast<SealedWaters*>(this->dashboard);
	
	if (dashboard != nullptr) {
		float vinset = statusbar_height();
		float gwidth = width / 64.0F;
		float gheight = (height - vinset - vinset) / 44.0F;

		this->grid->set_grid_width(gwidth);
		this->grid->set_grid_height(gheight, vinset);

		{ // load graphlets
			this->change_mode(SWMode::Dashboard);
			dashboard->load(width, height, gwidth, gheight);

			this->change_mode(SWMode::WindowUI);
			this->statusline = new Statuslinelet(default_logging_level);
			this->statusbar = new Statusbarlet(this->name(), this->device);
			this->insert(this->statusbar);
			this->insert(this->statusline);
		}

		{ // delayed initializing
			if (this->device != nullptr) {
				this->device->get_logger()->append_log_receiver(this->statusline);
			}
		}
	}
}

void SealedWaterPage::reflow(float width, float height) {
	auto dashboard = dynamic_cast<SealedWaters*>(this->dashboard);
	
	if (dashboard != nullptr) {
		float vinset = statusbar_height();
		float gwidth = this->grid->get_grid_width();
		float gheight = this->grid->get_grid_height();

		this->change_mode(SWMode::WindowUI);
		this->move_to(this->statusline, 0.0F, height, GraphletAnchor::LB);

		this->change_mode(SWMode::Dashboard);
		dashboard->reflow(width, height, gwidth, gheight, vinset);
	}
}

void SealedWaterPage::on_elapse(long long count, long long interval, long long uptime) {
	auto dashboard = dynamic_cast<SealedWaters*>(this->dashboard);

	if (dashboard != nullptr) {
		dashboard->on_elapse(count, interval, uptime);
	}
}


bool SealedWaterPage::can_select(IGraphlet* g) {
	return ((dynamic_cast<HydraulicPumplet*>(g) != nullptr)
		|| (dynamic_cast<GateValvelet*>(g) != nullptr));
}

void SealedWaterPage::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool ctrled) {
	auto pump = dynamic_cast<HydraulicPumplet*>(g);
	auto gvalve = dynamic_cast<GateValvelet*>(g);
	
	Planet::on_tap(g, local_x, local_y, shifted, ctrled);

	if (gvalve != nullptr) {
		menu_popup(this->valve_op, g, local_x, local_y);
	} else if (pump != nullptr) {
		menu_popup(this->pump_op, g, local_x, local_y);
	}
}
