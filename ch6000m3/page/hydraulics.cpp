#include "page/hydraulics.hpp"
#include "configuration.hpp"
#include "dashboard.hpp"

#include "text.hpp"
#include "paint.hpp"
#include "brushes.hxx"
#include "turtle.hpp"

#include "graphlet/booleanlet.hpp"
#include "graphlet/shapelet.hpp"
#include "graphlet/svglet.hpp"

#include "graphlet/symbol/gaugelet.hpp"
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

private enum HSMode { WindowUI = 0, View };

// WARNING: order matters
private enum class HS : unsigned int {
	// Pumps
	A, B, G, H,
	F, C, D, E,
	J, I,
	Y, L, M, K,
	// Valves
	SQ1, SQ2, SQy, SQi, SQj,
	SQa, SQb, SQg, SQh, SQk2, SQk1,
	SQf, SQc, SQd, SQe,
	// Key Labels
	Heater, VisorTank, Port, Starboard, MasterTank, OilTank,
	// Indicators
	F001Blocked, LevelLow, LevelLow2, LevelHigh, FilterBlocked,
	_,
	// anchors used as last jumping points
	a, b, c, d, e, f, g, h, i, j, y, l, m, k
};

private class Hydraulics final : public PLCConfirmation, public DashBoard<HydraulicSystem, HS> {
public:
	Hydraulics(HydraulicSystem* master) : DashBoard(master, "HS") {
		this->caption_font = make_text_format("Microsoft YaHei", 18.0F);
	}

public:
	void on_analog_input_data(const uint8* AI_DB203, size_t count, Syslog* logger) override {
		this->master->enter_critical_section();

		this->temperatures[HS::Heater]->set_value(RealData(AI_DB203, 18U));
		this->temperatures[HS::VisorTank]->set_value(RealData(AI_DB203, 19U));

		{ // pump pressures
			HS bar_seq[] = { HS::C, HS::F, HS::D, HS::E, HS::A, HS::B, HS::G, HS::H, HS::I, HS::J };

			for (size_t i = 0; i < sizeof(bar_seq) / sizeof(HS); i++) {
				HS id = bar_seq[i];
				Dimensionlet* target = this->bars[id];
				bool need_adjust_position = ((id == HS::F) || (id == HS::C) || (id == HS::D) || (id == HS::E));
				float anchor_x, anchor_y;

				if (need_adjust_position) {
					this->master->fill_graphlet_location(target, &anchor_x, &anchor_y, GraphletAlignment::RB);
				}

				target->set_value(RealData(AI_DB203, 8 + i));

				if (need_adjust_position) {
					this->master->move_to(target, anchor_x, anchor_y, GraphletAlignment::RB);
				}
			}
		}

		this->master->leave_critical_section();
	}

	void on_digital_input(const uint8* DI_db205_X, size_t count, Syslog* logger) {
		this->master->enter_critical_section();

		{ // pump states
			HS pump_seq[] = { HS::A, HS::B, HS::C, HS::D, HS::E, HS::G, HS::G, HS::H, HS::Y, HS::K, HS::L, HS::M, HS::I, HS::J };

			for (size_t i = 0; i < sizeof(pump_seq) / sizeof(HS); i++) {
				Pumplet* target = this->pumps[pump_seq[i]];

				switch (DI_db205_X[i]) {
				case 0b00000001: target->set_status(PumpStatus::Starting); break;
				case 0b00000010: target->set_status(PumpStatus::Stopping); break;
				case 0b00000100: target->set_status(PumpStatus::Unstartable); break;
				case 0b00001000: target->set_status(PumpStatus::Unstoppable); break;
				case 0b00010000: target->set_status(PumpStatus::Running); break;
				case 0b00100000: target->set_status(PumpStatus::Stopped); break;
				case 0b01000000: target->set_status(PumpStatus::Remote); break;
				case 0b10000000: target->set_status(PumpStatus::Ready); break;
				}
			}
		}

		this->master->leave_critical_section();
	}

public:
	void load_pump_station(float width, float height, float gridsize) {
		Turtle<HS>* pTurtle = new Turtle<HS>(gridsize, true, HS::SQ1);

		pTurtle->move_down()->turn_down_right()->move_right(13, HS::l)->turn_right_down()->move_down(6);
		
		pTurtle->move_down(3, HS::f)->move_right(6, HS::SQf)->move_right(8, HS::F)->move_right(6)->jump_back();
		pTurtle->move_down(3, HS::c)->move_right(6, HS::SQc)->move_right(8, HS::C)->move_right(6)->jump_back();
		pTurtle->move_down(3, HS::d)->move_right(6, HS::SQd)->move_right(8, HS::D)->move_right(6)->jump_back();
		pTurtle->move_down(3, HS::e)->move_right(6, HS::SQe)->move_right(8, HS::E)->move_right(6);
		
		pTurtle->move_up(12, HS::Port)->move_up(20)->turn_up_left()->move_left(35)->turn_left_down()->move_down(4);
		pTurtle->jump_left(4)->move_up(4)->turn_up_left()->move_left(31)->turn_left_down()->move_down(20);

		pTurtle->move_down(3, HS::a)->move_right(6, HS::A)->move_right(8, HS::SQa)->move_right(6)->jump_back();
		pTurtle->move_down(3, HS::b)->move_right(6, HS::B)->move_right(8, HS::SQb)->move_right(6)->jump_back();
		pTurtle->move_down(3, HS::g)->move_right(6, HS::G)->move_right(8, HS::SQg)->move_right(6)->jump_back();
		pTurtle->move_down(3, HS::h)->move_right(6, HS::H)->move_right(8, HS::SQh)->move_right(6);

		pTurtle->move_up(12, HS::Starboard)->move_up(6)->turn_up_right()->move_right(13)->turn_right_up()->move_up(1, HS::SQ2);

		pTurtle->jump_back(HS::l);
		pTurtle->jump_left(5, HS::y)->turn_right_up()->move_up(4, HS::SQy)->move_up(4, HS::Y)->move_up(4)->jump_back();
		pTurtle->move_right(5, HS::l)->turn_right_up()->move_up(8, HS::L)->move_up(4)->jump_back();
		pTurtle->move_right(5, HS::m)->turn_right_up()->move_up(8, HS::M)->move_up(4)->jump_back();
		pTurtle->move_right(3, HS::SQk2)->move_right(3, HS::k)->move_up(9, HS::K)->move_up(3)->turn_up_left();
		pTurtle->move_left(21)->turn_left_down()->move_down(1.0F, HS::F001Blocked)->move_down(2.0F);

		pTurtle->jump_back(HS::k)->move_right(3, HS::SQk1)->move_right(2.5F, HS::OilTank);

		pTurtle->jump_back(HS::SQ1)->jump_down(11.8F)->move_down(2, HS::SQi)->move_down(3, HS::I)->move_down(3);
		pTurtle->jump_back(HS::SQ2)->jump_down(11.8F)->move_down(2, HS::SQj)->move_down(3, HS::J)->move_down(3);
		this->stations[0] = new Tracklet<HS>(pTurtle, 1.5F, Colours::DimGray);

		pTurtle->wipe();
		pTurtle->jump_back(HS::SQ1)->move_up(2);
		pTurtle->jump_back(HS::SQ2)->move_up(2);
		this->stations[1] = new Tracklet<HS>(pTurtle, 1.5F, Colours::Yellow); 

		this->load_label(this->captions, HS::MasterTank, Colours::Silver, this->caption_font);
		this->load_label(this->captions, HS::Port, Colours::DarkKhaki, this->caption_font);
		this->load_label(this->captions, HS::Starboard, Colours::DarkKhaki, this->caption_font);
		this->load_label(this->captions, HS::Heater, Colours::Silver, this->caption_font);
		this->load_label(this->captions, HS::VisorTank, Colours::Silver, this->caption_font);
		this->load_label(this->captions, HS::OilTank, Colours::Silver);

		this->oil_tank = new Rectanglelet(gridsize * 2.5F, Colours::DimGray, Colours::WhiteSmoke, 3.0F);

		/** TODO
		 * These two construtions may fail due to D2DERR_BAD_NUMBER(HRESULT: 0x88990011),
		 * If it happens please try to change the `step` to ensure that `height / step >= 0.1F`,
		 * the default `step` is 10.
		 */
		this->heater = new LevelGaugelet(gridsize * 14.0F, gridsize * 6.0F, 1.5F, 6);
		this->visor_tank = new LevelGaugelet(gridsize * 12.0F, gridsize * 5.0F, 0.8F, 8);

		this->master->insert_all(this->stations, true);
		this->master->insert(this->oil_tank);
		this->master->insert(this->heater);
		this->master->insert(this->visor_tank);
	}

	void load_devices(float width, float height, float gridsize) {
		{ // load pumps
			this->load_graphlets(this->pumps, this->plabels, HS::A, HS::H, gridsize, 180.0, this->pcaptions);
			this->load_graphlets(this->pumps, this->plabels, HS::F, HS::E, gridsize, 0.000, this->pcaptions);
			this->load_graphlets(this->pumps, this->plabels, HS::Y, HS::K, gridsize, -90.0, this->pcaptions);
			this->load_graphlets(this->pumps, this->plabels, HS::J, HS::I, gridsize, 90.00, this->pcaptions);

			this->load_dimensions(this->bars, HS::A, HS::I, "bar");
		}

		{ // load valves
			this->load_graphlets(this->valves, this->vlabels, HS::SQ1, HS::SQj, gridsize, 0.000);
			this->load_graphlets(this->valves, this->vlabels, HS::SQa, HS::SQk1, gridsize, -90.0);
			this->load_graphlets(this->valves, this->vlabels, HS::SQf, HS::SQe, gridsize, 90.00);
		}
	}

	void load_state_indicators(float width, float height, float gridsize) {
		float size = gridsize * 1.0F;

		this->load_state_indicator(HS::LevelLow, size, this->heater_states, this->hslabels, Colours::Green);
		this->load_state_indicator(HS::LevelLow2, size, this->heater_states, this->hslabels, Colours::Green);
		this->load_state_indicator(HS::LevelHigh, size, this->heater_states, this->hslabels, Colours::Green);
		this->load_state_indicator(HS::F001Blocked, size, this->heater_states, this->hslabels, Colours::Green);

		this->load_state_indicator(HS::LevelLow, size, this->visor_states, this->vslabels, Colours::Green);
		this->load_state_indicator(HS::LevelLow2, size, this->visor_states, this->vslabels, Colours::Green);
		this->load_state_indicator(HS::FilterBlocked, size, this->visor_states, this->vslabels, Colours::Green);

		this->load_dimensions(this->temperatures, HS::Heater, HS::VisorTank, "celsius", "temperature");
	}

	void reflow_pump_station(float width, float height, float gridsize, float vinset) {
		float sw, sh, sx, sy, s1_x, s1_y;

		this->stations[0]->fill_extent(0.0F, 0.0F, &sw, &sh);
		this->stations[0]->fill_anchor_location(HS::SQ1, &s1_x, &s1_y);

		sx = (width - sw) * 0.5F;
		sy = (height - sh) * 0.5F - vinset * 0.5F;
		this->master->move_to(this->stations[0], sx, sy);

		this->master->move_to(this->stations[1], sx + s1_x, sy + s1_y, GraphletAlignment::RB);
		this->stations[0]->map_graphlet_at_anchor(this->oil_tank, HS::OilTank, GraphletAlignment::LC);
		this->master->move_to(this->heater, sx + (sw - gridsize) * 0.5F, sy + s1_y - gridsize * 1.5F, GraphletAlignment::CB);
		this->master->move_to(this->visor_tank, sx + (sw - gridsize) * 0.5F, sy + s1_y + gridsize * 12.0F, GraphletAlignment::CB);

		this->stations[0]->map_credit_graphlet(this->captions[HS::Port], GraphletAlignment::CB, -gridsize * 10.0F);
		this->stations[0]->map_credit_graphlet(this->captions[HS::Starboard], GraphletAlignment::CB, -gridsize * 10.0F);
		this->master->move_to(this->captions[HS::OilTank], this->oil_tank, GraphletAlignment::CB, GraphletAlignment::CT);
		this->master->move_to(this->captions[HS::Heater], this->heater, GraphletAlignment::LB, GraphletAlignment::LT, gridsize);
		this->master->move_to(this->captions[HS::VisorTank], this->visor_tank, GraphletAlignment::CT, GraphletAlignment::CB, gridsize * 0.5F);
	}
	
	void reflow_devices(float width, float height, float gridsize, float vinset) {
		GraphletAlignment lbl_align, cpt_align, bar_align;
		float lbl_dx, lbl_dy, cpt_dx, cpt_dy, bar_dx, bar_dy;
		float valve_adjust_gridsize = gridsize * 0.618F;
		float text_hspace = vinset * 0.125F;
		float x0 = 0.0F;
		float y0 = 0.0F;

		for (auto it = this->pumps.begin(); it != this->pumps.end(); it++) {
			switch (int(it->second->get_direction_degrees())) {
			case -90: { // for Y, L, M, K
				lbl_dx = x0 - gridsize; lbl_dy = y0; lbl_align = GraphletAlignment::RB;
				cpt_dx = x0 + text_hspace; cpt_dy = y0 - gridsize; cpt_align = GraphletAlignment::LB;
				bar_dx = x0; bar_dy = y0; bar_align = GraphletAlignment::CC; // these devices have no scales
			} break;
			case 90: {  // for J, I
				lbl_dx = x0 + gridsize; lbl_dy = y0; lbl_align = GraphletAlignment::LT;
				cpt_dx = x0; cpt_dy = y0 + gridsize * 3.0F; cpt_align = GraphletAlignment::CT;
				bar_dx = x0 + text_hspace; bar_dy = y0 + gridsize; bar_align = GraphletAlignment::LT;
			} break;
			case 180: { // for A, B, G, H
				lbl_dx = x0 - gridsize; lbl_dy = y0; lbl_align = GraphletAlignment::RT;
				cpt_dx = x0 + gridsize; cpt_dy = y0; cpt_align = GraphletAlignment::LT;
				bar_dx = x0 + gridsize; bar_dy = y0; bar_align = GraphletAlignment::LB;
			} break;
			default: {  // for F, C, D, E
				lbl_dx = x0 + gridsize; lbl_dy = y0; lbl_align = GraphletAlignment::LT;
				cpt_dx = x0 - gridsize; cpt_dy = y0; cpt_align = GraphletAlignment::RT;
				bar_dx = x0 - gridsize; bar_dy = y0; bar_align = GraphletAlignment::RB;
			}
			}

			this->stations[0]->map_credit_graphlet(it->second, GraphletAlignment::CC, x0, y0);
			this->stations[0]->map_credit_graphlet(this->plabels[it->first], lbl_align, lbl_dx, lbl_dy);
			this->stations[0]->map_credit_graphlet(this->pcaptions[it->first], cpt_align, cpt_dx, cpt_dy);

			if (this->bars.find(it->first) != this->bars.end()) {
				this->stations[0]->map_credit_graphlet(this->bars[it->first], bar_align, bar_dx, bar_dy);
			}
		}

		for (auto it = this->valves.begin(); it != this->valves.end(); it++) {
			if (it->second->get_direction_degrees() == 0.0) {
				switch (it->first) {
				case HS::SQ2: case HS::SQy: {
					lbl_dx = x0 - valve_adjust_gridsize; lbl_dy = y0; lbl_align = GraphletAlignment::RC;
				} break;
				default: {
					lbl_dx = x0 + valve_adjust_gridsize; lbl_dy = y0; lbl_align = GraphletAlignment::LC;
				}
				}
			} else {
				lbl_dx = x0; lbl_dy = y0 - valve_adjust_gridsize; lbl_align = GraphletAlignment::CB;
			}

			this->stations[0]->map_credit_graphlet(it->second, GraphletAlignment::CC, x0, y0);
			this->stations[0]->map_credit_graphlet(this->vlabels[it->first], lbl_align, lbl_dx, lbl_dy);
		}
	}

	void reflow_state_indicators(float width, float height, float gridsize, float vinset) {
		this->master->move_to(this->captions[HS::MasterTank], this->stations[0], GraphletAlignment::LT, GraphletAlignment::CB, gridsize * 12.0F, gridsize * 4.0F);
		this->master->move_to(this->heater_states[HS::LevelLow], this->captions[HS::MasterTank], GraphletAlignment::LB, GraphletAlignment::LT, -gridsize, gridsize);
		this->master->move_to(this->heater_states[HS::LevelLow2], this->heater_states[HS::LevelLow], GraphletAlignment::LB, GraphletAlignment::LT, 0.0F, gridsize * 0.5F);
		this->master->move_to(this->heater_states[HS::LevelHigh], this->heater_states[HS::LevelLow2], GraphletAlignment::LB, GraphletAlignment::LT, 0.0F, gridsize * 0.5F);
		this->master->move_to(this->temperatures[HS::Heater], this->heater_states[HS::LevelHigh], GraphletAlignment::LB, GraphletAlignment::LT, 0.0F, gridsize);
		this->stations[0]->map_credit_graphlet(this->heater_states[HS::F001Blocked], GraphletAlignment::CC);

		this->master->move_to(this->visor_states[HS::LevelLow], this->visor_tank, GraphletAlignment::RT, GraphletAlignment::LT, gridsize * 2.0F, 0.0F);
		this->master->move_to(this->visor_states[HS::LevelLow2], this->visor_states[HS::LevelLow], GraphletAlignment::LB, GraphletAlignment::LT, 0.0F, gridsize * 0.5F);
		this->master->move_to(this->visor_states[HS::FilterBlocked], this->visor_states[HS::LevelLow2], GraphletAlignment::LB, GraphletAlignment::LT, 0.0F, gridsize * 0.5F);
		this->master->move_to(this->temperatures[HS::VisorTank], this->visor_states[HS::FilterBlocked], GraphletAlignment::LB, GraphletAlignment::LT, 0.0F, gridsize);

		{ // reflow state labels
			float gapsize = vinset * 0.25F;

			for (auto lt = this->heater_states.begin(); lt != this->heater_states.end(); lt++) {
				this->master->move_to(this->hslabels[lt->first], this->heater_states[lt->first],
					GraphletAlignment::RC, GraphletAlignment::LC, gapsize);
			}

			for (auto lt = this->visor_states.begin(); lt != this->visor_states.end(); lt++) {
				this->master->move_to(this->vslabels[lt->first], this->visor_states[lt->first],
					GraphletAlignment::RC, GraphletAlignment::LC, gapsize);
			}
		}
	}

// never deletes these graphlets mannually
private:
	Tracklet<HS>* stations[2];
	LevelGaugelet* heater;
	LevelGaugelet* visor_tank;
	IShapelet* oil_tank;
	std::map<HS, Credit<Labellet, HS>*> captions;
	std::map<HS, Credit<Pumplet, HS>*> pumps;
	std::map<HS, Credit<Labellet, HS>*> plabels;
	std::map<HS, Credit<Labellet, HS>*> pcaptions;
	std::map<HS, Credit<Valvelet, HS>*> valves;
	std::map<HS, Credit<Labellet, HS>*> vlabels;
	std::map<HS, Credit<Dimensionlet, HS>*> bars;
	std::map<HS, Credit<Dimensionlet, HS>*> temperatures;
	std::map<HS, Credit<Booleanlet, HS>*> heater_states;
	std::map<HS, Credit<Labellet, HS>*> hslabels;
	std::map<HS, Credit<Booleanlet, HS>*> visor_states;
	std::map<HS, Credit<Labellet, HS>*> vslabels;
	
private:
	CanvasTextFormat^ caption_font;
};

HydraulicSystem::HydraulicSystem(IMRMaster* plc) : Planet(":hs:"), device(plc) {
	Hydraulics* dashboard = new Hydraulics(this);

	this->dashboard = dashboard; 
	this->gridsize = statusbar_height();

	this->device->append_confirmation_receiver(dashboard);
}

HydraulicSystem::~HydraulicSystem() {
	if (this->dashboard != nullptr) {
		delete this->dashboard;
	}
}

void HydraulicSystem::load(CanvasCreateResourcesReason reason, float width, float height) {
	auto dashboard = dynamic_cast<Hydraulics*>(this->dashboard);
	
	if (dashboard != nullptr) {
		float vinset = statusbar_height();

		{ // load graphlets
			this->change_mode(HSMode::View);
			dashboard->load_pump_station(width, height, this->gridsize);
			dashboard->load_devices(width, height, this->gridsize);
			dashboard->load_state_indicators(width, height, this->gridsize);

			this->change_mode(HSMode::WindowUI);
			this->statusline = new Statuslinelet(default_logging_level);
			this->statusbar = new Statusbarlet(this->name(), this->device);
			this->insert(this->statusbar);
			this->insert(this->statusline);
		}

		{ // delayed initializing
			PageDecorator* page = new PageDecorator(Colours::GrayText);
#ifdef _DEBUG
			IPlanetDecorator* decorators[] = { page, new GridDecorator(this->gridsize, 0.0F, 0.0F, vinset) };

			this->set_decorator(new CompositeDecorator(decorators));
#else
			this->set_decorator(page);
#endif

			if (this->device != nullptr) {
				this->device->get_logger()->append_log_receiver(this->statusline);
			}
		}
	}
}

void HydraulicSystem::reflow(float width, float height) {
	auto dashboard = dynamic_cast<Hydraulics*>(this->dashboard);
	
	if (dashboard != nullptr) {
		float vinset = statusbar_height();

		this->change_mode(HSMode::WindowUI);
		this->move_to(this->statusline, 0.0F, height, GraphletAlignment::LB);

		this->change_mode(HSMode::View);
		dashboard->reflow_pump_station(width, height, this->gridsize, vinset);
		dashboard->reflow_devices(width, height, this->gridsize, vinset);
		dashboard->reflow_state_indicators(width, height, this->gridsize, vinset);
	}
}

void HydraulicSystem::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool ctrled) {
	if (dynamic_cast<Tracklet<HS>*>(g) == nullptr) {
		this->set_selected(g);
	}
	// this->set_caret_owner(g);
}
