#include <map>

#include "page/hydraulics.hpp"
#include "configuration.hpp"
#include "menu.hpp"

#include "module.hpp"
#include "text.hpp"
#include "paint.hpp"
#include "brushes.hxx"
#include "turtle.hpp"

#include "graphlet/shapelet.hpp"

#include "graphlet/symbol/pump/hydraulic_pumplet.hpp"
#include "graphlet/symbol/valve/gate_valvelet.hpp"
#include "graphlet/device/tanklet.hpp"
#include "graphlet/dashboard/fueltanklet.hpp"
#include "graphlet/dashboard/thermometerlet.hpp"

#include "decorator/page.hpp"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

private enum HSMode { WindowUI = 0, Dashboard };

private enum class HSPOperation { Start, Stop, Reset, Auto, _ };
private enum class HSVOperation { Start, Stop, Reset, _ };

private enum class HSMTStatus { Empty, UltraLow, Low, Normal, High, Full, _ };
private enum class HSVTStatus { Empty, UltraLow, Low, Normal, Full, _ };

static CanvasSolidColorBrush^ oil_color = Colours::Yellow;

static ICanvasBrush^ block_color = Colours::Red;
static ICanvasBrush^ nonblock_color = Colours::WhiteSmoke;

// WARNING: order matters
private enum class HS : unsigned int {
	// Pumps
	A, B, G, H,
	F, C, D, E,
	J, I,
	Y, L, M, K,

	// Valves
	SQ1, SQ2, SQy, SQl, SQm, SQi, SQj,
	SQa, SQb, SQg, SQh, SQk2, SQk1,
	SQf, SQc, SQd, SQe,
	
	// Key Labels
	Port, Starboard, Master, Visor, Storage, Pressure,
	
	// Filter Indicators
	F01, F02, F10,
	
	_,
	
	// anchors used as last jumping points
	a, b, c, d, e, f, g, h, y, l, m, k, k12,

	// anchors used for unnamed corners
	lt, tl, rt, tr, cl, cr, i, j, f02
};

private class Hydraulics final
	: public PLCConfirmation
	, public IMenuCommand<HSPOperation, Credit<HydraulicPumplet, HS>, IMRMaster*>
	, public IMenuCommand<HSVOperation, Credit<GateValvelet, HS>, IMRMaster*> {
public:
	Hydraulics(HydraulicsPage* master) : master(master) {}

public:
	void pre_read_data(Syslog* logger) override {
		this->master->enter_critical_section();
		this->master->begin_update_sequence();

		this->station->clear_subtacks();
	}

	void on_analog_input_data(const uint8* AI_DB203, size_t count, Syslog* logger) override {
		this->set_temperature(HS::Visor, RealData(AI_DB203, 18U));
		this->set_temperature(HS::Master, RealData(AI_DB203, 19U));
		this->station_pressure->set_value(RealData(AI_DB203, 21U));

		{ // pump pressures
			this->pressures[HS::C]->set_value(RealData(AI_DB203, 8), GraphletAnchor::RB);
			this->pressures[HS::F]->set_value(RealData(AI_DB203, 9), GraphletAnchor::RB);
			this->pressures[HS::D]->set_value(RealData(AI_DB203, 10), GraphletAnchor::RB);
			this->pressures[HS::E]->set_value(RealData(AI_DB203, 11), GraphletAnchor::RB);

			this->pressures[HS::A]->set_value(RealData(AI_DB203, 12), GraphletAnchor::LB);
			this->pressures[HS::B]->set_value(RealData(AI_DB203, 13), GraphletAnchor::LB);
			this->pressures[HS::G]->set_value(RealData(AI_DB203, 14), GraphletAnchor::LB);
			this->pressures[HS::H]->set_value(RealData(AI_DB203, 15), GraphletAnchor::LB);

			this->pressures[HS::I]->set_value(RealData(AI_DB203, 16), GraphletAnchor::RB);
			this->pressures[HS::J]->set_value(RealData(AI_DB203, 17), GraphletAnchor::LB);
		}
	}

	void on_digital_input(const uint8* DI_DB205, size_t count, Syslog* logger) {
		{ // pump states
			HS pump_seq[] = { HS::A, HS::B, HS::C, HS::D, HS::E, HS::F, HS::G, HS::H, HS::Y, HS::K, HS::L, HS::M, HS::I, HS::J };

			for (size_t i = 0; i < sizeof(pump_seq) / sizeof(HS); i++) {
				HydraulicPumplet* target = this->pumps[pump_seq[i]];

				switch (DI_DB205[i + 1]) {
				case 0b00000001: target->set_status(HydraulicPumpStatus::Starting); break;
				case 0b00000010: target->set_status(HydraulicPumpStatus::Stopping); break;
				case 0b00000100: target->set_status(HydraulicPumpStatus::Unstartable); break;
				case 0b00001000: target->set_status(HydraulicPumpStatus::Unstoppable); break;
				case 0b00010000: target->set_status(HydraulicPumpStatus::StartReady); break;
				case 0b00100000: target->set_status(HydraulicPumpStatus::StopReady); break;
				case 0b01000000: target->set_status(HydraulicPumpStatus::Stopped); break;
				case 0b10000000: target->set_status(HydraulicPumpStatus::Ready); break;
				}
			}
		}
	}

	void on_raw_digital_input(const uint8* DI_DB4, size_t count, WarGrey::SCADA::Syslog* logger) override {
		{ // tank status
			if (DBX(DI_DB4, 118U - 1)) {
				this->master_tank->set_status(HSMTStatus::Low);
			} else if (DBX(DI_DB4, 119U - 1)) {
				this->master_tank->set_status(HSMTStatus::UltraLow);
			} else if (DBX(DI_DB4, 120U - 1)) {
				this->master_tank->set_status(HSMTStatus::High);
			} else {
				this->master_tank->set_status(HSMTStatus::Normal);
			}

			if (DBX(DI_DB4, 132U - 1)) {
				this->visor_tank->set_status(HSVTStatus::Low);
			} else if (DBX(DI_DB4, 133U - 1)) {
				this->visor_tank->set_status(HSVTStatus::UltraLow);
			} else {
				this->visor_tank->set_status(HSVTStatus::Normal);
			}
		}

		{ // pump modes or statuses
			HS pump_seq[] = { HS::A, HS::B, HS::C, HS::I, HS::H, HS::G, HS::F, HS::J, HS::D, HS::E, HS::K, HS::L, HS::M, HS::Y };

			for (size_t i = 0; i < sizeof(pump_seq) / sizeof(HS); i++) {
				this->set_pump_status(pump_seq[i], DI_DB4, i * 4 + 49);
			}
		}

		{ // valve statuses
			this->set_valve_status(HS::SQ1, DI_DB4, 113);
			this->set_valve_status(HS::SQ2, DI_DB4, 114);

			this->set_valve_status(HS::SQk1, DI_DB4, 122);
			this->set_valve_status(HS::SQk2, DI_DB4, 123);
			this->set_valve_status(HS::SQl,  DI_DB4, 124);
			this->set_valve_status(HS::SQm,  DI_DB4, 125);
			this->set_valve_status(HS::SQy,  DI_DB4, 126);

			this->set_valve_status(HS::SQi, DI_DB4, 128);
			this->set_valve_status(HS::SQj, DI_DB4, 129);

			this->set_valve_status(HS::SQc, DI_DB4, 135);
			this->set_valve_status(HS::SQd, DI_DB4, 136);
			this->set_valve_status(HS::SQe, DI_DB4, 137);
			this->set_valve_status(HS::SQf, DI_DB4, 138);

			this->set_valve_status(HS::SQa, DI_DB4, 141);
			this->set_valve_status(HS::SQb, DI_DB4, 142);
			this->set_valve_status(HS::SQg, DI_DB4, 143);
			this->set_valve_status(HS::SQh, DI_DB4, 144);
		}

		{ // filter statuses
			this->set_filter_status(HS::F01, DI_DB4, 121);
			this->set_filter_status(HS::F02, DI_DB4, 127);
			this->set_filter_status(HS::F10, DI_DB4, 134);
		}
	}

	void post_read_data(Syslog* logger) override {
		{ // flow oil
			HS ps_path[] = { HS::lt, HS::tl, HS::cl, HS::Master };
			HS sb_path[] = { HS::rt, HS::tr, HS::cr, HS::Master };
			HS i_path[] = { HS::f02, HS::Master };

			this->station->append_subtrack(HS::Master, HS::SQ1, oil_color);
			this->station->append_subtrack(HS::Master, HS::SQ2, oil_color);
			this->station->append_subtrack(HS::Visor, HS::SQi, oil_color);
			this->station->append_subtrack(HS::Visor, HS::SQj, oil_color);
			this->station->append_subtrack(HS::Storage, HS::SQk1, oil_color);

			this->try_flow_oil(HS::SQi, HS::I, HS::i, nullptr, 0, oil_color);
			this->try_flow_oil(HS::SQj, HS::J, HS::j, nullptr, 0, oil_color);

			this->try_flow_oil(HS::SQc, HS::C, HS::c, sb_path, oil_color);
			this->try_flow_oil(HS::SQd, HS::D, HS::d, sb_path, oil_color);
			this->try_flow_oil(HS::SQe, HS::E, HS::e, sb_path, oil_color);
			this->try_flow_oil(HS::SQf, HS::F, HS::f, sb_path, oil_color);

			this->try_flow_oil(HS::SQa, HS::A, HS::a, ps_path, oil_color);
			this->try_flow_oil(HS::SQb, HS::B, HS::b, ps_path, oil_color);
			this->try_flow_oil(HS::SQg, HS::G, HS::g, ps_path, oil_color);
			this->try_flow_oil(HS::SQh, HS::H, HS::h, ps_path, oil_color);

			this->try_flow_oil(HS::SQy, HS::Y, HS::y, i_path, oil_color);
			this->try_flow_oil(HS::SQl, HS::L, HS::l, i_path, oil_color);
			this->try_flow_oil(HS::SQm, HS::M, HS::m, i_path, oil_color);
			this->try_flow_oil(HS::SQk1, HS::K, HS::k, i_path, oil_color);
			this->try_flow_oil(HS::SQk2, HS::K /* , HS::k, i_path */, oil_color);

			this->try_flow_oil(HS::SQ1, HS::SQh, oil_color);
			this->try_flow_oil(HS::SQ2, HS::SQe, oil_color);
			this->try_flow_oil(HS::SQ2, HS::SQk2, oil_color);
			this->try_flow_oil(HS::SQ2, HS::SQm, oil_color);
		}
		
		this->master->end_update_sequence();
		this->master->leave_critical_section();
	}

public:
	void execute(HSPOperation cmd, Credit<HydraulicPumplet, HS>* pump, IMRMaster* plc) {
		plc->get_logger()->log_message(Log::Info, L"%s %s",
			cmd.ToString()->Data(),
			pump->id.ToString()->Data());
	}

	void execute(HSVOperation cmd, Credit<GateValvelet, HS>* valve, IMRMaster* plc) {
		plc->get_logger()->log_message(Log::Info, L"%s %s",
			cmd.ToString()->Data(),
			valve->id.ToString()->Data());
	}

public:
	void construct(float gwidth, float gheight) {
		this->caption_font = make_bold_text_format("Microsoft YaHei", large_font_size);
		this->label_font = make_bold_text_format("Microsoft YaHei", small_font_size);
		this->dimension_style.number_font = make_bold_text_format("Cambria Math", metrics_font_size);
		this->dimension_style.unit_font = make_bold_text_format("Cambria", normal_font_size);
	}
 
public:
	void load_pump_station(float width, float height, float gwidth, float gheight) {
		Turtle<HS>* pTurtle = new Turtle<HS>(gwidth, gheight, true, HS::Master);

		pTurtle->move_right(2)->move_down(5.5F, HS::SQ2);
		pTurtle->move_down()->turn_down_right()->move_right(13, HS::l)->turn_right_down()->move_down(17);
		
		pTurtle->jump_right(20, HS::e)->move_left(5, HS::E)->move_left(10, HS::SQe)->move_left(5)->jump_back();
		pTurtle->move_up(3, HS::d)->move_left(5, HS::D)->move_left(10, HS::SQd)->move_left(5)->jump_back();
		pTurtle->move_up(3, HS::c)->move_left(5, HS::C)->move_left(10, HS::SQc)->move_left(5)->jump_back();
		pTurtle->move_up(3, HS::f)->move_left(5, HS::F)->move_left(10, HS::SQf)->move_left(5)->jump_back();
		
		pTurtle->move_up(3, HS::Port)->move_up(21, HS::rt)->turn_up_left(HS::tr)->move_left(35, HS::cr);
		pTurtle->turn_left_down()->move_down(2, HS::F01)->move_down(2);
		pTurtle->jump_up(4)->turn_up_left(HS::cl)->move_left(35, HS::tl)->turn_left_down(HS::lt)->move_down(21);

		pTurtle->move_down(3, HS::a)->move_right(5, HS::A)->move_right(10, HS::SQa)->move_right(5)->jump_back();
		pTurtle->move_down(3, HS::b)->move_right(5, HS::B)->move_right(10, HS::SQb)->move_right(5)->jump_back();
		pTurtle->move_down(3, HS::g)->move_right(5, HS::G)->move_right(10, HS::SQg)->move_right(5)->jump_back();
		pTurtle->move_down(3, HS::h)->move_right(5, HS::H)->move_right(10, HS::SQh)->move_right(5);

		pTurtle->move_up(12, HS::Starboard)->move_up(5)->turn_up_right()->move_right(13)->turn_right_up();
		pTurtle->move_up(1, HS::SQ1)->move_up(5.5F)->move_to(HS::Master);

		pTurtle->jump_back(HS::Master)->jump_right(4);
		pTurtle->move_up(5.5F, HS::F02)->move_up()->turn_up_right(HS::f02)->move_right(2);
		pTurtle->move_right(5, HS::y)->move_down(6, HS::Y)->move_down(4, HS::SQy)->move_down(4)->turn_down_left()->jump_back();
		pTurtle->move_right(5, HS::l)->move_down(6, HS::L)->move_down(4, HS::SQl)->move_down(4)->turn_down_left()->jump_back();
		pTurtle->move_right(5, HS::m)->move_down(6, HS::M)->move_down(4, HS::SQm)->move_down(4)->turn_down_left()->jump_back();
		pTurtle->move_right(4, HS::k)->turn_right_down()->move_down(5, HS::K)->move_down(9, HS::k12);
		pTurtle->move_left(3, HS::SQk2)->move_left(8)->jump_back()->move_right(3, HS::SQk1)->move_right(2.5F, HS::Storage);
		
		pTurtle->jump_back(HS::Master)->jump_down(14, HS::Visor);
		pTurtle->move_right(2)->move_down(5, HS::SQj)->move_down(3, HS::J)->move_down(3, HS::j);
		pTurtle->jump_left(4, HS::i)->move_up(3, HS::I)->move_up(3, HS::SQi)->move_up(5)->move_right(2 /* HS::Visor */);
		
		this->station = this->master->insert_one(new Tracklet<HS>(pTurtle, default_pipeline_thickness, default_pipeline_color));
		
		this->load_label(this->captions, HS::Port, Colours::DarkKhaki, this->caption_font);
		this->load_label(this->captions, HS::Starboard, Colours::DarkKhaki, this->caption_font);
		this->load_label(this->captions, HS::Storage, Colours::Silver);
	}

	void load_tanks(float width, float height, float gwidth, float gheight) {
		float thickness = 3.0F;

		this->master_tank = this->make_tank(HSMTStatus::Empty, gwidth * 18.0F, gheight * 8.0F, thickness);
		this->visor_tank = this->make_tank(HSVTStatus::Empty, gwidth * 16.0F, gheight * 7.0F, thickness);

		this->load_thermometer(this->thermometers, this->temperatures, HS::Master, gwidth * 2.5F, gheight * 4.5F);
		this->load_thermometer(this->thermometers, this->temperatures, HS::Visor, gwidth * 2.5F, gheight * 4.5F);

		this->storage_tank = this->master->insert_one(new FuelTanklet(gwidth * 2.5F, 0.0F, thickness, Colours::WhiteSmoke));
		
		this->load_filter_indicators(HS::F01, HS::F10, gwidth, this->filters, this->islabels);
	}

	void load_devices(float width, float height, float gwidth, float gheight) {
		float radius = resolve_gridsize(gwidth, gheight);
		float pradius = radius * 1.2F;

		{ // load pumps
			this->load_devices(this->pumps, this->labels, this->captions, HS::A, HS::H, pradius, 180.0, Colours::Salmon);
			this->load_devices(this->pumps, this->labels, this->captions, HS::F, HS::E, pradius, 0.000, Colours::Salmon);
			this->load_devices(this->pumps, this->labels, this->captions, HS::Y, HS::K, pradius, -90.0);
			this->load_devices(this->pumps, this->labels, this->captions, HS::J, HS::I, pradius, 90.00);

			this->load_dimensions(this->pressures, HS::A, HS::I, "bar");

			this->station_pressure = new Dimensionlet(this->dimension_style, "bar", _speak(HS::Pressure));
			this->master->insert(this->station_pressure);
		}

		{ // load valves
			this->load_devices(this->valves, this->labels, HS::SQ1, HS::SQj, radius, 90.000);
			this->load_devices(this->valves, this->labels, HS::SQa, HS::SQk1, radius, 0.0);
			this->load_devices(this->valves, this->labels, HS::SQf, HS::SQe, radius, 180.00);
		}
	}

public:
	void reflow_pump_station(float width, float height, float gwidth, float gheight, float vinset) {
		float cx = width * 0.5F;
		float cy = height * 0.5F;
		float sq1_y;

		this->master->move_to(this->station, cx, cy, GraphletAnchor::CC);
		this->station->map_graphlet_at_anchor(this->storage_tank, HS::Storage, GraphletAnchor::LC);
		this->station->fill_anchor_location(HS::SQ1, nullptr, &sq1_y, true);
		this->station->map_graphlet_at_anchor(this->master_tank, HS::Master, GraphletAnchor::CC);
		this->station->map_graphlet_at_anchor(this->visor_tank, HS::Visor, GraphletAnchor::CC);
		this->master->move_to(this->station_pressure, this->station, GraphletAnchor::CT, GraphletAnchor::CB);
		this->master->move_to(this->thermometers[HS::Master], this->master_tank, 0.25F, 0.5F, GraphletAnchor::CC);
		this->master->move_to(this->thermometers[HS::Visor], this->visor_tank, 0.25F, 0.5F, GraphletAnchor::CC);

		this->station->map_credit_graphlet(this->captions[HS::Port], GraphletAnchor::CB, -gwidth * 10.0F);
		this->station->map_credit_graphlet(this->captions[HS::Starboard], GraphletAnchor::CB, -gwidth * 10.0F);
		this->master->move_to(this->captions[HS::Storage], this->storage_tank, GraphletAnchor::CB, GraphletAnchor::CT);
	}
	
	void reflow_devices(float width, float height, float gwidth, float gheight, float vinset) {
		GraphletAnchor lbl_a, cpt_a, bar_a;
		float lbl_dx, lbl_dy, cpt_dx, cpt_dy, bar_dx, bar_dy, margin;
		float pradius = this->pumps[HS::A]->get_radiusX();
		float vradius = this->valves[HS::SQ1]->get_radiusY();
		float text_hspace = vinset * 0.125F;
		float x0 = 0.0F;
		float y0 = 0.0F;

		for (auto it = this->pumps.begin(); it != this->pumps.end(); it++) {
			switch (it->second->id) {
			case HS::A: case HS::B: case HS::G: case HS::H: {
				lbl_dx = x0 - pradius; lbl_dy = y0; lbl_a = GraphletAnchor::RT;
				cpt_dx = x0 + pradius; cpt_dy = y0; cpt_a = GraphletAnchor::LT;
				bar_dx = x0 + pradius; bar_dy = y0; bar_a = GraphletAnchor::LB;
			} break;
			case HS::F: case HS::C: case HS::D: case HS::E: {
				lbl_dx = x0 + pradius; lbl_dy = y0; lbl_a = GraphletAnchor::LT;
				cpt_dx = x0 - pradius; cpt_dy = y0; cpt_a = GraphletAnchor::RT;
				bar_dx = x0 - pradius; bar_dy = y0; bar_a = GraphletAnchor::RB;
			} break;
			case HS::Y: case HS::L: case HS::M: case HS::K: {
				lbl_dx = x0 - pradius; lbl_dy = y0; lbl_a = GraphletAnchor::RB;
				cpt_dx = x0 + text_hspace; cpt_dy = y0 - pradius; cpt_a = GraphletAnchor::LB;
				bar_dx = x0; bar_dy = y0; bar_a = GraphletAnchor::CC; // these devices have no metrics
			} break;
			default: {
				cpt_dx = x0; cpt_dy = y0 + gheight * 3.0F; cpt_a = GraphletAnchor::CT;
			
				if (it->second->id == HS::J) {
					lbl_dx = x0 + pradius; lbl_dy = y0; lbl_a = GraphletAnchor::LT;
					bar_dx = x0 + text_hspace; bar_dy = y0 + pradius; bar_a = GraphletAnchor::LT;
				} else { // HS::I
					lbl_dx = x0 - pradius; lbl_dy = y0; lbl_a = GraphletAnchor::RT;
					bar_dx = x0 - text_hspace; bar_dy = y0 + pradius; bar_a = GraphletAnchor::RT;
				}
			}
			}

			this->station->map_credit_graphlet(it->second, GraphletAnchor::CC, x0, y0);
			this->station->map_credit_graphlet(this->labels[it->first], lbl_a, lbl_dx, lbl_dy);
			this->station->map_credit_graphlet(this->captions[it->first], cpt_a, cpt_dx, cpt_dy);

			if (this->pressures.find(it->first) != this->pressures.end()) {
				this->station->map_credit_graphlet(this->pressures[it->first], bar_a, bar_dx, bar_dy);
			}
		}

		for (auto it = this->valves.begin(); it != this->valves.end(); it++) {
			if (it->second->get_direction_degrees() == 90.0) {
				switch (it->first) {
				case HS::SQ1: case HS::SQi: case HS::SQy: {
					it->second->fill_margin(x0, y0, nullptr, nullptr, nullptr, &margin);
					lbl_dx = x0 - vradius + margin; lbl_dy = y0; lbl_a = GraphletAnchor::RC;
				}; break;
				default: {
					it->second->fill_margin(x0, y0, nullptr, nullptr, nullptr, &margin);
					lbl_dx = x0 + vradius - margin; lbl_dy = y0; lbl_a = GraphletAnchor::LC;
				}
				}
			} else {
				it->second->fill_margin(x0, y0, &margin, nullptr, nullptr, nullptr);
				lbl_dx = x0; lbl_dy = y0 - vradius + margin; lbl_a = GraphletAnchor::CB;
			}

			this->station->map_credit_graphlet(it->second, GraphletAnchor::CC, x0, y0);
			this->station->map_credit_graphlet(this->labels[it->first], lbl_a, lbl_dx, lbl_dy);
		}
	}

	void reflow_metrics(float width, float height, float gwidth, float gheight, float vinset) {
		this->station->map_credit_graphlet(this->filters[HS::F01], GraphletAnchor::CC);
		this->station->map_credit_graphlet(this->filters[HS::F02], GraphletAnchor::CC);
		
		this->master->move_to(this->temperatures[HS::Master], this->thermometers[HS::Master], GraphletAnchor::RC, GraphletAnchor::LC, gwidth);
		this->master->move_to(this->temperatures[HS::Visor], this->thermometers[HS::Visor], GraphletAnchor::RC, GraphletAnchor::LT, gwidth);
		this->master->move_to(this->filters[HS::F10], this->thermometers[HS::Visor], GraphletAnchor::RC, GraphletAnchor::LB, gwidth);
		
		{ // reflow state labels
			float gapsize = vinset * 0.25F;

			for (auto lt = this->filters.begin(); lt != this->filters.end(); lt++) {
				switch (lt->first) {
				case HS::F01: {
					this->master->move_to(this->islabels[lt->first], this->filters[lt->first],
						GraphletAnchor::LC, GraphletAnchor::RC, -gapsize);
				}; break;
				default: {
					this->master->move_to(this->islabels[lt->first], this->filters[lt->first],
						GraphletAnchor::RC, GraphletAnchor::LC, gapsize);
				}
				}
			}
		}
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
	void load_dimensions(std::map<E, Credit<Dimensionlet, E>*>& ds, E id0, E idn, Platform::String^ unit, Platform::String^ label = nullptr) {
		for (E id = id0; id <= idn; id++) {
			ds[id] = this->master->insert_one(new Credit<Dimensionlet, E>(this->dimension_style, unit, label), id);
		}
	}

	template<typename E>
	void load_filter_indicators(E id0, E idn, float size, std::map<E, Credit<Rectanglet, E>*>& bs, std::map<E, Credit<Labellet, E>*>& ls) {
		for (E id = id0; id <= idn; id++) {
			this->load_label(ls, id, Colours::Silver);
			bs[id] = this->master->insert_one(new Credit<Rectanglet, E>(size, nonblock_color), id);
		}
	}

	template<class T, typename E>
	void load_thermometer(std::map<E, Credit<T, E>*>& ts, std::map<E, Credit<Dimensionlet, E>*>& ds, E id, float width, float height) {
		ts[id] = this->master->insert_one(new Credit<T, E>(100.0, width, height, 2.5F), id);
		ds[id] = this->master->insert_one(new Credit<Dimensionlet, E>(this->dimension_style, "celsius", _speak(id)), id);
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

	template<typename E>
	Tanklet<E>* make_tank(E id, float width, float height, float thickness) {
		Tanklet<E>* tank = new Tanklet<E>(id, width, height, thickness);
		TankStyle ulow, low, normal;

		normal.mark_weight = 0.50F;
		low.mark_weight    = 0.20F;
		ulow.mark_weight   = 0.10F;
		
		tank->set_style(E::Normal,   normal);
		tank->set_style(E::Low,      low);
		tank->set_style(E::UltraLow, ulow);

		// WARNING: set styles before inserting 

		return this->master->insert_one(tank);
	}

private:
	void set_temperature(HS id, float t) {
		this->thermometers[id]->set_value(t);
		this->temperatures[id]->set_value(t, GraphletAnchor::LB);
	}

	void set_pump_status(HS id, const uint8* db4, size_t idx_p1) {
		HydraulicPumplet* target = this->pumps[id];
		
		target->set_remote_control(DBX(db4, idx_p1 - 1));

		if (DBX(db4, idx_p1 + 0)) {
			target->set_status(HydraulicPumpStatus::Running);
		}

		if (DBX(db4, idx_p1 + 1)) {
			target->set_status(HydraulicPumpStatus::Broken);
		}
	}

	void set_valve_status(HS id, const uint8* db4, size_t idx_p1) {
		if (DBX(db4, idx_p1 - 1)) {
			this->valves[id]->set_status(GateValveStatus::Open);
		} else {
			this->valves[id]->set_status(GateValveStatus::Closed);
		}
	}

	void set_filter_status(HS id, const uint8* db4, size_t idx_p1) {
		if (DBX(db4, idx_p1 - 1)) {
			this->filters[id]->set_color(block_color);
		} else {
			this->filters[id]->set_color(nonblock_color);
		}
	}

	void try_flow_oil(HS vid, HS pid, CanvasSolidColorBrush^ color) {
		if (this->valves[vid]->get_status() == GateValveStatus::Open) {
			this->station->append_subtrack(vid, pid, color);
		}
	}

	void try_flow_oil(HS vid, HS pid, HS _id, HS* path, unsigned int count, CanvasSolidColorBrush^ color) {
		this->try_flow_oil(vid, pid, color);

		if (this->pumps[pid]->get_status() == HydraulicPumpStatus::Running) {
			this->station->append_subtrack(pid, _id, oil_color);

			if (path != nullptr) {
				this->station->append_subtrack(_id, path[0], oil_color);
				this->station->append_subtrack(path, count, color);
			}
		}
	}

	template<unsigned int N>
	void try_flow_oil(HS vid, HS pid, HS _id, HS (&path)[N], CanvasSolidColorBrush^ color) {
		this->try_flow_oil(vid, pid, _id, path, N, color);
	}

// never deletes these graphlets mannually
private:
	Tracklet<HS>* station;
	FuelTanklet* storage_tank;
	Tanklet<HSMTStatus>* master_tank;
	Tanklet<HSVTStatus>* visor_tank;
	Dimensionlet* station_pressure;
	std::map<HS, Credit<Thermometerlet, HS>*> thermometers;
	std::map<HS, Credit<Dimensionlet, HS>*> temperatures;
	std::map<HS, Credit<Labellet, HS>*> captions;
	std::map<HS, Credit<Labellet, HS>*> labels;
	std::map<HS, Credit<HydraulicPumplet, HS>*> pumps;
	std::map<HS, Credit<GateValvelet, HS>*> valves;
	std::map<HS, Credit<Dimensionlet, HS>*> pressures;
	std::map<HS, Credit<Rectanglet, HS>*> filters;
	std::map<HS, Credit<Labellet, HS>*> islabels;
	
private:
	CanvasTextFormat^ caption_font;
	CanvasTextFormat^ label_font;
	DimensionStyle dimension_style;

private:
	HydraulicsPage* master;
};

HydraulicsPage::HydraulicsPage(IMRMaster* plc) : Planet(__MODULE__), device(plc) {
	Hydraulics* dashboard = new Hydraulics(this);

	this->dashboard = dashboard;
	this->pump_op = make_menu<HSPOperation, Credit<HydraulicPumplet, HS>, IMRMaster*>(dashboard, plc);
	this->valve_op = make_menu<HSVOperation, Credit<GateValvelet, HS>, IMRMaster*>(dashboard, plc);
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

HydraulicsPage::~HydraulicsPage() {
	if (this->dashboard != nullptr) {
		delete this->dashboard;
	}

#ifndef _DEBUG
	delete this->grid;
#endif
}

void HydraulicsPage::load(CanvasCreateResourcesReason reason, float width, float height) {
	auto dashboard = dynamic_cast<Hydraulics*>(this->dashboard);
	
	if (dashboard != nullptr) {
		float vinset = statusbar_height();
		float gwidth = width / 78.0F;
		float gheight = (height - vinset - vinset) / 38.0F;

		this->grid->set_grid_width(gwidth);
		this->grid->set_grid_height(gheight, vinset);
		
		dashboard->construct(gwidth, gheight);

		{ // load graphlets
			this->change_mode(HSMode::Dashboard);
			dashboard->load_pump_station(width, height, gwidth, gheight);
			dashboard->load_tanks(width, height, gwidth, gheight);
			dashboard->load_devices(width, height, gwidth, gheight);

			this->change_mode(HSMode::WindowUI);
			this->statusline = new Statuslinelet(default_logging_level);
			this->statusbar = new Statusbarlet(this->name(), this->device);
			this->insert(this->statusbar);
			this->insert(this->statusline);
		}

		{ // delayed initializing
			this->get_logger()->append_log_receiver(this->statusline);

			if (this->device != nullptr) {
				this->device->get_logger()->append_log_receiver(this->statusline);
			}
		}
	}
}

void HydraulicsPage::reflow(float width, float height) {
	auto dashboard = dynamic_cast<Hydraulics*>(this->dashboard);
	
	if (dashboard != nullptr) {
		float vinset = statusbar_height();
		float gwidth = this->grid->get_grid_width();
		float gheight = this->grid->get_grid_height();

		this->change_mode(HSMode::WindowUI);
		this->move_to(this->statusline, 0.0F, height, GraphletAnchor::LB);

		this->change_mode(HSMode::Dashboard);
		dashboard->reflow_pump_station(width, height, gwidth, gheight, vinset);
		dashboard->reflow_devices(width, height, gwidth, gheight, vinset);
		dashboard->reflow_metrics(width, height, gwidth, gheight, vinset);
	}
}

bool HydraulicsPage::can_select(IGraphlet* g) {
	return ((dynamic_cast<HydraulicPumplet*>(g) != nullptr)
		|| (dynamic_cast<GateValvelet*>(g) != nullptr));
}

void HydraulicsPage::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool ctrled) {
	auto pump = dynamic_cast<HydraulicPumplet*>(g);
	auto gvalve = dynamic_cast<GateValvelet*>(g);
	
	Planet::on_tap(g, local_x, local_y, shifted, ctrled);

	if (pump != nullptr) {
		menu_popup(this->pump_op, g, local_x, local_y);
	} else if (gvalve != nullptr) {
		menu_popup(this->valve_op, g, local_x, local_y);
	}
}
