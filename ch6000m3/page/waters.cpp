#include "page/waters.hpp"
#include "configuration.hpp"
#include "dashboard.hpp"

#include "text.hpp"
#include "paint.hpp"
#include "brushes.hxx"
#include "turtle.hpp"

#include "graphlet/booleanlet.hpp"
#include "graphlet/shapelet.hpp"
#include "graphlet/svglet.hpp"

#include "graphlet/device/gaugelet.hpp"
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

private enum WSMode { WindowUI = 0, View };

// WARNING: order matters
private enum class WS : unsigned int {
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

private class Waters final : public PLCConfirmation, public DashBoard<WaterSystemPage, WS> {
public:
	Waters(WaterSystemPage* master) : DashBoard(master, "WS") {
		this->caption_font = make_text_format("Microsoft YaHei", 18.0F);
	}

public:
	void on_analog_input_data(const uint8* AI_DB203, size_t count, Syslog* logger) override {
		this->master->enter_critical_section();

		this->temperatures[WS::Heater]->set_value(RealData(AI_DB203, 18U));
		this->temperatures[WS::VisorTank]->set_value(RealData(AI_DB203, 19U));

		{ // pump pressures
			WS bar_seq[] = { WS::C, WS::F, WS::D, WS::E, WS::A, WS::B, WS::G, WS::H, WS::I, WS::J };

			for (size_t i = 0; i < sizeof(bar_seq) / sizeof(WS); i++) {
				WS id = bar_seq[i];
				Dimensionlet* target = this->bars[id];
				bool need_adjust_position = ((id == WS::F) || (id == WS::C) || (id == WS::D) || (id == WS::E));
				
				if (need_adjust_position) {
					target->set_value(RealData(AI_DB203, 8 + i), GraphletAnchor::RB);
				} else {
					target->set_value(RealData(AI_DB203, 8 + i));
				}
			}
		}

		this->master->leave_critical_section();
	}

	void on_digital_input(const uint8* DI_db205_X, size_t count, Syslog* logger) {
		this->master->enter_critical_section();

		{ // pump states
			WS pump_seq[] = { WS::A, WS::B, WS::C, WS::D, WS::E, WS::G, WS::G, WS::H, WS::Y, WS::K, WS::L, WS::M, WS::I, WS::J };

			for (size_t i = 0; i < sizeof(pump_seq) / sizeof(WS); i++) {
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
		Turtle<WS>* pTurtle = new Turtle<WS>(gridsize, true, WS::SQ1);

		pTurtle->move_down()->turn_down_right()->move_right(13, WS::l)->turn_right_down()->move_down(6);
		
		pTurtle->move_down(3, WS::f)->move_right(6, WS::SQf)->move_right(8, WS::F)->move_right(6)->jump_back();
		pTurtle->move_down(3, WS::c)->move_right(6, WS::SQc)->move_right(8, WS::C)->move_right(6)->jump_back();
		pTurtle->move_down(3, WS::d)->move_right(6, WS::SQd)->move_right(8, WS::D)->move_right(6)->jump_back();
		pTurtle->move_down(3, WS::e)->move_right(6, WS::SQe)->move_right(8, WS::E)->move_right(6);
		
		pTurtle->move_up(12, WS::Port)->move_up(20)->turn_up_left()->move_left(35)->turn_left_down()->move_down(4);
		pTurtle->jump_left(4)->move_up(4)->turn_up_left()->move_left(31)->turn_left_down()->move_down(20);

		pTurtle->move_down(3, WS::a)->move_right(6, WS::A)->move_right(8, WS::SQa)->move_right(6)->jump_back();
		pTurtle->move_down(3, WS::b)->move_right(6, WS::B)->move_right(8, WS::SQb)->move_right(6)->jump_back();
		pTurtle->move_down(3, WS::g)->move_right(6, WS::G)->move_right(8, WS::SQg)->move_right(6)->jump_back();
		pTurtle->move_down(3, WS::h)->move_right(6, WS::H)->move_right(8, WS::SQh)->move_right(6);

		pTurtle->move_up(12, WS::Starboard)->move_up(6)->turn_up_right()->move_right(13)->turn_right_up()->move_up(1, WS::SQ2);

		pTurtle->jump_back(WS::l);
		pTurtle->jump_left(5, WS::y)->turn_right_up()->move_up(4, WS::SQy)->move_up(4, WS::Y)->move_up(4)->jump_back();
		pTurtle->move_right(5, WS::l)->turn_right_up()->move_up(8, WS::L)->move_up(4)->jump_back();
		pTurtle->move_right(5, WS::m)->turn_right_up()->move_up(8, WS::M)->move_up(4)->jump_back();
		pTurtle->move_right(3, WS::SQk2)->move_right(3, WS::k)->move_up(9, WS::K)->move_up(3)->turn_up_left();
		pTurtle->move_left(21)->turn_left_down()->move_down(1.0F, WS::F001Blocked)->move_down(2.0F);

		pTurtle->jump_back(WS::k)->move_right(3, WS::SQk1)->move_right(2.5F, WS::OilTank);

		pTurtle->jump_back(WS::SQ1)->jump_down(11.8F)->move_down(2, WS::SQi)->move_down(3, WS::I)->move_down(3);
		pTurtle->jump_back(WS::SQ2)->jump_down(11.8F)->move_down(2, WS::SQj)->move_down(3, WS::J)->move_down(3);
		this->stations[0] = new Tracklet<WS>(pTurtle, 1.5F, Colours::Gray);

		pTurtle->wipe();
		pTurtle->jump_back(WS::SQ1)->move_up(2);
		pTurtle->jump_back(WS::SQ2)->move_up(2);
		this->stations[1] = new Tracklet<WS>(pTurtle, 1.5F, Colours::Yellow); 

		this->load_label(this->captions, WS::MasterTank, Colours::Silver, this->caption_font);
		this->load_label(this->captions, WS::Port, Colours::DarkKhaki, this->caption_font);
		this->load_label(this->captions, WS::Starboard, Colours::DarkKhaki, this->caption_font);
		this->load_label(this->captions, WS::Heater, Colours::Silver, this->caption_font);
		this->load_label(this->captions, WS::VisorTank, Colours::Silver, this->caption_font);
		this->load_label(this->captions, WS::OilTank, Colours::Silver);

		this->oil_tank = new Rectanglelet(gridsize * 2.5F, Colours::DimGray, Colours::WhiteSmoke, 3.0F);

		/** TODO
		 * These two construtions may fail due to D2DERR_BAD_NUMBER(HRESULT: 0x88990011),
		 * If it happens please try to change the `step` to ensure that `height / step >= 0.1F`,
		 * the default `step` is 10.
		 */
		this->heater = new LevelGaugelet(1.5F, gridsize * 14.0F, gridsize * 6.0F, 6U);
		this->visor_tank = new LevelGaugelet(0.8F, gridsize * 12.0F, gridsize * 5.0F, 8U);

		this->master->insert_all(this->stations, true);
		this->master->insert(this->oil_tank);
		this->master->insert(this->heater);
		this->master->insert(this->visor_tank);
	}

	void load_devices(float width, float height, float gridsize) {
		{ // load pumps
			this->load_graphlets(this->pumps, this->plabels, WS::A, WS::H, gridsize, 180.0, this->pcaptions);
			this->load_graphlets(this->pumps, this->plabels, WS::F, WS::E, gridsize, 0.000, this->pcaptions);
			this->load_graphlets(this->pumps, this->plabels, WS::Y, WS::K, gridsize, -90.0, this->pcaptions);
			this->load_graphlets(this->pumps, this->plabels, WS::J, WS::I, gridsize, 90.00, this->pcaptions);

			this->load_dimensions(this->bars, WS::A, WS::I, "bar");
		}

		{ // load valves
			this->load_graphlets(this->valves, this->vlabels, WS::SQ1, WS::SQj, gridsize, 0.000);
			this->load_graphlets(this->valves, this->vlabels, WS::SQa, WS::SQk1, gridsize, -90.0);
			this->load_graphlets(this->valves, this->vlabels, WS::SQf, WS::SQe, gridsize, 90.00);
		}
	}

	void load_state_indicators(float width, float height, float gridsize) {
		float size = gridsize * 1.0F;

		this->load_state_indicator(WS::LevelLow, size, this->heater_states, this->hslabels, Colours::Green);
		this->load_state_indicator(WS::LevelLow2, size, this->heater_states, this->hslabels, Colours::Green);
		this->load_state_indicator(WS::LevelHigh, size, this->heater_states, this->hslabels, Colours::Green);
		this->load_state_indicator(WS::F001Blocked, size, this->heater_states, this->hslabels, Colours::Green);

		this->load_state_indicator(WS::LevelLow, size, this->visor_states, this->vslabels, Colours::Green);
		this->load_state_indicator(WS::LevelLow2, size, this->visor_states, this->vslabels, Colours::Green);
		this->load_state_indicator(WS::FilterBlocked, size, this->visor_states, this->vslabels, Colours::Green);

		this->load_dimensions(this->temperatures, WS::Heater, WS::VisorTank, "celsius", "temperature");
	}

	void reflow_pump_station(float width, float height, float gridsize, float vinset) {
		float sw, sh, sx, sy, s1_x, s1_y;

		this->stations[0]->fill_extent(0.0F, 0.0F, &sw, &sh);
		this->stations[0]->fill_anchor_location(WS::SQ1, &s1_x, &s1_y);

		sx = (width - sw) * 0.5F;
		sy = (height - sh) * 0.5F - vinset * 0.5F;
		this->master->move_to(this->stations[0], sx, sy);

		this->master->move_to(this->stations[1], sx + s1_x, sy + s1_y, GraphletAnchor::RB);
		this->stations[0]->map_graphlet_at_anchor(this->oil_tank, WS::OilTank, GraphletAnchor::LC);
		this->master->move_to(this->heater, sx + (sw - gridsize) * 0.5F, sy + s1_y - gridsize * 1.5F, GraphletAnchor::CB);
		this->master->move_to(this->visor_tank, sx + (sw - gridsize) * 0.5F, sy + s1_y + gridsize * 12.0F, GraphletAnchor::CB);

		this->stations[0]->map_credit_graphlet(this->captions[WS::Port], GraphletAnchor::CB, -gridsize * 10.0F);
		this->stations[0]->map_credit_graphlet(this->captions[WS::Starboard], GraphletAnchor::CB, -gridsize * 10.0F);
		this->master->move_to(this->captions[WS::OilTank], this->oil_tank, GraphletAnchor::CB, GraphletAnchor::CT);
		this->master->move_to(this->captions[WS::Heater], this->heater, GraphletAnchor::LB, GraphletAnchor::LT, gridsize);
		this->master->move_to(this->captions[WS::VisorTank], this->visor_tank, GraphletAnchor::CT, GraphletAnchor::CB, gridsize * 0.5F);
	}
	
	void reflow_devices(float width, float height, float gridsize, float vinset) {
		GraphletAnchor lbl_a, cpt_a, bar_a;
		float lbl_dx, lbl_dy, cpt_dx, cpt_dy, bar_dx, bar_dy;
		float valve_adjust_gridsize = gridsize * 0.618F;
		float text_hspace = vinset * 0.125F;
		float x0 = 0.0F;
		float y0 = 0.0F;

		for (auto it = this->pumps.begin(); it != this->pumps.end(); it++) {
			switch (int(it->second->get_direction_degrees())) {
			case -90: { // for Y, L, M, K
				lbl_dx = x0 - gridsize; lbl_dy = y0; lbl_a = GraphletAnchor::RB;
				cpt_dx = x0 + text_hspace; cpt_dy = y0 - gridsize; cpt_a = GraphletAnchor::LB;
				bar_dx = x0; bar_dy = y0; bar_a = GraphletAnchor::CC; // these devices have no scales
			} break;
			case 90: {  // for J, I
				lbl_dx = x0 + gridsize; lbl_dy = y0; lbl_a = GraphletAnchor::LT;
				cpt_dx = x0; cpt_dy = y0 + gridsize * 3.0F; cpt_a = GraphletAnchor::CT;
				bar_dx = x0 + text_hspace; bar_dy = y0 + gridsize; bar_a = GraphletAnchor::LT;
			} break;
			case 180: { // for A, B, G, H
				lbl_dx = x0 - gridsize; lbl_dy = y0; lbl_a = GraphletAnchor::RT;
				cpt_dx = x0 + gridsize; cpt_dy = y0; cpt_a = GraphletAnchor::LT;
				bar_dx = x0 + gridsize; bar_dy = y0; bar_a = GraphletAnchor::LB;
			} break;
			default: {  // for F, C, D, E
				lbl_dx = x0 + gridsize; lbl_dy = y0; lbl_a = GraphletAnchor::LT;
				cpt_dx = x0 - gridsize; cpt_dy = y0; cpt_a = GraphletAnchor::RT;
				bar_dx = x0 - gridsize; bar_dy = y0; bar_a = GraphletAnchor::RB;
			}
			}

			this->stations[0]->map_credit_graphlet(it->second, GraphletAnchor::CC, x0, y0);
			this->stations[0]->map_credit_graphlet(this->plabels[it->first], lbl_a, lbl_dx, lbl_dy);
			this->stations[0]->map_credit_graphlet(this->pcaptions[it->first], cpt_a, cpt_dx, cpt_dy);

			if (this->bars.find(it->first) != this->bars.end()) {
				this->stations[0]->map_credit_graphlet(this->bars[it->first], bar_a, bar_dx, bar_dy);
			}
		}

		for (auto it = this->valves.begin(); it != this->valves.end(); it++) {
			if (it->second->get_direction_degrees() == 0.0) {
				switch (it->first) {
				case WS::SQ2: case WS::SQy: {
					lbl_dx = x0 - valve_adjust_gridsize; lbl_dy = y0; lbl_a = GraphletAnchor::RC;
				} break;
				default: {
					lbl_dx = x0 + valve_adjust_gridsize; lbl_dy = y0; lbl_a = GraphletAnchor::LC;
				}
				}
			} else {
				lbl_dx = x0; lbl_dy = y0 - valve_adjust_gridsize; lbl_a = GraphletAnchor::CB;
			}

			this->stations[0]->map_credit_graphlet(it->second, GraphletAnchor::CC, x0, y0);
			this->stations[0]->map_credit_graphlet(this->vlabels[it->first], lbl_a, lbl_dx, lbl_dy);
		}
	}

	void reflow_state_indicators(float width, float height, float gridsize, float vinset) {
		this->master->move_to(this->captions[WS::MasterTank], this->stations[0], GraphletAnchor::LT, GraphletAnchor::CB, gridsize * 12.0F, gridsize * 4.0F);
		this->master->move_to(this->heater_states[WS::LevelLow], this->captions[WS::MasterTank], GraphletAnchor::LB, GraphletAnchor::LT, -gridsize, gridsize);
		this->master->move_to(this->heater_states[WS::LevelLow2], this->heater_states[WS::LevelLow], GraphletAnchor::LB, GraphletAnchor::LT, 0.0F, gridsize * 0.5F);
		this->master->move_to(this->heater_states[WS::LevelHigh], this->heater_states[WS::LevelLow2], GraphletAnchor::LB, GraphletAnchor::LT, 0.0F, gridsize * 0.5F);
		this->master->move_to(this->temperatures[WS::Heater], this->heater_states[WS::LevelHigh], GraphletAnchor::LB, GraphletAnchor::LT, 0.0F, gridsize);
		this->stations[0]->map_credit_graphlet(this->heater_states[WS::F001Blocked], GraphletAnchor::CC);

		this->master->move_to(this->visor_states[WS::LevelLow], this->visor_tank, GraphletAnchor::RT, GraphletAnchor::LT, gridsize * 2.0F, 0.0F);
		this->master->move_to(this->visor_states[WS::LevelLow2], this->visor_states[WS::LevelLow], GraphletAnchor::LB, GraphletAnchor::LT, 0.0F, gridsize * 0.5F);
		this->master->move_to(this->visor_states[WS::FilterBlocked], this->visor_states[WS::LevelLow2], GraphletAnchor::LB, GraphletAnchor::LT, 0.0F, gridsize * 0.5F);
		this->master->move_to(this->temperatures[WS::VisorTank], this->visor_states[WS::FilterBlocked], GraphletAnchor::LB, GraphletAnchor::LT, 0.0F, gridsize);

		{ // reflow state labels
			float gapsize = vinset * 0.25F;

			for (auto lt = this->heater_states.begin(); lt != this->heater_states.end(); lt++) {
				this->master->move_to(this->hslabels[lt->first], this->heater_states[lt->first],
					GraphletAnchor::RC, GraphletAnchor::LC, gapsize);
			}

			for (auto lt = this->visor_states.begin(); lt != this->visor_states.end(); lt++) {
				this->master->move_to(this->vslabels[lt->first], this->visor_states[lt->first],
					GraphletAnchor::RC, GraphletAnchor::LC, gapsize);
			}
		}
	}

// never deletes these graphlets mannually
private:
	Tracklet<WS>* stations[2];
	LevelGaugelet* heater;
	LevelGaugelet* visor_tank;
	IShapelet* oil_tank;
	std::map<WS, Credit<Labellet, WS>*> captions;
	std::map<WS, Credit<Pumplet, WS>*> pumps;
	std::map<WS, Credit<Labellet, WS>*> plabels;
	std::map<WS, Credit<Labellet, WS>*> pcaptions;
	std::map<WS, Credit<Valvelet, WS>*> valves;
	std::map<WS, Credit<Labellet, WS>*> vlabels;
	std::map<WS, Credit<Dimensionlet, WS>*> bars;
	std::map<WS, Credit<Dimensionlet, WS>*> temperatures;
	std::map<WS, Credit<Booleanlet, WS>*> heater_states;
	std::map<WS, Credit<Labellet, WS>*> hslabels;
	std::map<WS, Credit<Booleanlet, WS>*> visor_states;
	std::map<WS, Credit<Labellet, WS>*> vslabels;
	
private:
	CanvasTextFormat^ caption_font;
};

WaterSystemPage::WaterSystemPage(IMRMaster* plc) : Planet(":ws:"), device(plc) {
	Waters* dashboard = new Waters(this);

	this->dashboard = dashboard; 
	this->gridsize = statusbar_height();

	this->device->append_confirmation_receiver(dashboard);
}

WaterSystemPage::~WaterSystemPage() {
	if (this->dashboard != nullptr) {
		delete this->dashboard;
	}
}

void WaterSystemPage::load(CanvasCreateResourcesReason reason, float width, float height) {
	auto dashboard = dynamic_cast<Waters*>(this->dashboard);
	
	if (dashboard != nullptr) {
		float vinset = statusbar_height();

		{ // load graphlets
			this->change_mode(WSMode::View);
			dashboard->load_pump_station(width, height, this->gridsize);
			dashboard->load_devices(width, height, this->gridsize);
			dashboard->load_state_indicators(width, height, this->gridsize);

			this->change_mode(WSMode::WindowUI);
			this->statusline = new Statuslinelet(default_logging_level);
			this->statusbar = new Statusbarlet(this->name(), this->device);
			this->insert(this->statusbar);
			this->insert(this->statusline);
		}

		{ // delayed initializing
			this->append_decorator(new PageDecorator(Colours::GrayText));

#ifdef _DEBUG
			this->append_decorator(new GridDecorator(this->gridsize, 0.0F, 0.0F, vinset));
#endif

			if (this->device != nullptr) {
				this->device->get_logger()->append_log_receiver(this->statusline);
			}
		}
	}
}

void WaterSystemPage::reflow(float width, float height) {
	auto dashboard = dynamic_cast<Waters*>(this->dashboard);
	
	if (dashboard != nullptr) {
		float vinset = statusbar_height();

		this->change_mode(WSMode::WindowUI);
		this->move_to(this->statusline, 0.0F, height, GraphletAnchor::LB);

		this->change_mode(WSMode::View);
		dashboard->reflow_pump_station(width, height, this->gridsize, vinset);
		dashboard->reflow_devices(width, height, this->gridsize, vinset);
		dashboard->reflow_state_indicators(width, height, this->gridsize, vinset);
	}
}

void WaterSystemPage::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool ctrled) {
	if (dynamic_cast<Tracklet<WS>*>(g) == nullptr) {
		this->set_selected(g);
	}
	// this->set_caret_owner(g);
}
