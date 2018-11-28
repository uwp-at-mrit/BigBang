#include <map>

#include "page/hydraulics.hpp"
#include "configuration.hpp"
#include "menu.hpp"

#include "module.hpp"
#include "brushes.hxx"
#include "turtle.hpp"

#include "graphlet/shapelet.hpp"

#include "graphlet/symbol/heaterlet.hpp"
#include "graphlet/symbol/pump/hydraulic_pumplet.hpp"
#include "graphlet/symbol/valve/manual_valvelet.hpp"
#include "graphlet/device/tanklet.hpp"
#include "graphlet/dashboard/fueltanklet.hpp"
#include "graphlet/dashboard/thermometerlet.hpp"
#include "graphlet/dashboard/alarmlet.hpp"

#include "iotables/ai_metrics.hpp"
#include "iotables/ai_pumps.hpp"

#include "iotables/di_valves.hpp"
#include "iotables/di_pumps.hpp"
#include "iotables/di_devices.hpp"

#include "iotables/do_pumps.hpp"
#include "iotables/do_devices.hpp"

#include "decorator/page.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI::Xaml::Controls;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

private enum HSMode { WindowUI = 0, Dashboard };

private enum class HSMTStatus { Empty, UltraLow, Low, Normal, High, Full, _ };
private enum class HSVTStatus { Empty, UltraLow, Low, Normal, Full, _ };

static CanvasSolidColorBrush^ oil_color = Colours::Yellow;

// WARNING: order matters
private enum class HS : unsigned int {
	// Pumps
	A, B, G, H,
	C, F, D, E,
	J, I,
	Y, L, M, K,

	// Valves
	SQ1, SQ2, SQy, SQl, SQm, SQi, SQj,
	SQa, SQb, SQg, SQh, SQk2, SQk1,
	SQc, SQf, SQd, SQe,
	
	// Key Labels
	Port, Starboard, Master, Visor, Storage, BackOil,
	
	// Filter Indicators
	F01, F02, F10,
	
	_,
	
	// anchors used as last jumping points
	a, b, c, d, e, f, g, h, y, l, m, k, k12,

	// anchors used for unnamed corners
	lt, tl, rt, tr, cl, cr, i, j, f02, master, sb
};

static uint16 DO_hydraulics_action(HydraulicPumpAction cmd, HydraulicPumplet* pump) {
	auto credit_pump = dynamic_cast<Credit<HydraulicPumplet, HS>*>(pump);
	uint16 index = 0U;

	if (credit_pump != nullptr) {
		index = DO_hydraulic_pump_command(cmd, credit_pump->id);
	}

	return index;
}

/*************************************************************************************************/
private class Hydraulics final : public PLCConfirmation {
public:
	Hydraulics(HydraulicsPage* master) : master(master) {}

public:
	void pre_read_data(Syslog* logger) override {
		this->master->enter_critical_section();
		this->master->begin_update_sequence();

		this->station->clear_subtacks();
	}

	void on_analog_input(const uint8* DB2, size_t count2, const uint8* DB203, size_t count203, Syslog* logger) override {
		this->set_temperature(HS::Visor, RealData(DB203, visor_tank_temperature));
		this->set_temperature(HS::Master, RealData(DB203, master_tank_temperature));
		this->pressures[HS::BackOil]->set_value(RealData(DB203, master_back_oil_pressure));

		{ // pump pressures
			GraphletAnchor psa = GraphletAnchor::LB;
			GraphletAnchor sba = GraphletAnchor::RB;

			this->pressures[HS::C]->set_value(RealData(DB203, pump_C_pressure), psa);
			this->pressures[HS::F]->set_value(RealData(DB203, pump_F_pressure), psa);
			this->pressures[HS::D]->set_value(RealData(DB203, pump_D_pressure), psa);
			this->pressures[HS::E]->set_value(RealData(DB203, pump_E_pressure), psa);

			this->pressures[HS::A]->set_value(RealData(DB203, pump_A_pressure), sba);
			this->pressures[HS::B]->set_value(RealData(DB203, pump_B_pressure), sba);
			this->pressures[HS::G]->set_value(RealData(DB203, pump_G_pressure), sba);
			this->pressures[HS::H]->set_value(RealData(DB203, pump_H_pressure), sba);

			this->pressures[HS::I]->set_value(RealData(DB203, pump_I_pressure), GraphletAnchor::LB);
			this->pressures[HS::J]->set_value(RealData(DB203, pump_J_pressure), GraphletAnchor::RB);
		}
	}

	void on_digital_input(const uint8* DB4, size_t count4, const uint8* DB205, size_t count205, Syslog* logger) {
		{ // tank status
			if (DBX(DB4, 118U - 1)) {
				this->master_tank->set_status(HSMTStatus::Low);
			} else if (DBX(DB4, 119U - 1)) {
				this->master_tank->set_status(HSMTStatus::UltraLow);
			} else if (DBX(DB4, 120U - 1)) {
				this->master_tank->set_status(HSMTStatus::High);
			} else {
				this->master_tank->set_status(HSMTStatus::Normal);
			}

			if (DBX(DB4, 132U - 1)) {
				this->visor_tank->set_status(HSVTStatus::Low);
			} else if (DBX(DB4, 133U - 1)) {
				this->visor_tank->set_status(HSVTStatus::UltraLow);
			} else {
				this->visor_tank->set_status(HSVTStatus::Normal);
			}
		} 
		
		{ // pump states
			DI_hydraulic_pump(this->pumps[HS::A], DB4, pump_A_feedback, DB205, pump_A_status);
			DI_hydraulic_pump(this->pumps[HS::B], DB4, pump_B_feedback, DB205, pump_B_status);
			DI_hydraulic_pump(this->pumps[HS::G], DB4, pump_G_feedback, DB205, pump_G_status);
			DI_hydraulic_pump(this->pumps[HS::H], DB4, pump_H_feedback, DB205, pump_H_status);
			
			DI_hydraulic_pump(this->pumps[HS::C], DB4, pump_C_feedback, DB205, pump_C_status);
			DI_hydraulic_pump(this->pumps[HS::F], DB4, pump_F_feedback, DB205, pump_F_status);
			DI_hydraulic_pump(this->pumps[HS::D], DB4, pump_D_feedback, DB205, pump_D_status);
			DI_hydraulic_pump(this->pumps[HS::E], DB4, pump_E_feedback, DB205, pump_E_status);
			
			DI_hydraulic_pump(this->pumps[HS::Y], DB4, pump_Y_feedback, DB205, pump_Y_status);
			DI_hydraulic_pump(this->pumps[HS::L], DB4, pump_L_feedback, DB205, pump_L_status);
			DI_hydraulic_pump(this->pumps[HS::M], DB4, pump_M_feedback, DB205, pump_M_status);
			DI_hydraulic_pump(this->pumps[HS::K], DB4, pump_K_feedback, DB205, pump_K_status);

			DI_hydraulic_pump(this->pumps[HS::I], DB4, pump_I_feedback, DB205, pump_I_status);
			DI_hydraulic_pump(this->pumps[HS::J], DB4, pump_J_feedback, DB205, pump_J_status);
		}

		{ // valve statuses
			DI_manual_valve(this->valves[HS::SQ1], DB4, gate_valve_SQ1_status);
			DI_manual_valve(this->valves[HS::SQ2], DB4, gate_valve_SQ2_status);

			DI_manual_valve(this->valves[HS::SQk1], DB4, gate_valve_SQk1_status);
			DI_manual_valve(this->valves[HS::SQk2], DB4, gate_valve_SQk2_status);
			DI_manual_valve(this->valves[HS::SQl], DB4, gate_valve_SQl_status);
			DI_manual_valve(this->valves[HS::SQm], DB4, gate_valve_SQm_status);
			DI_manual_valve(this->valves[HS::SQy], DB4, gate_valve_SQy_status);

			DI_manual_valve(this->valves[HS::SQi], DB4, gate_valve_SQi_status);
			DI_manual_valve(this->valves[HS::SQj], DB4, gate_valve_SQj_status);

			DI_manual_valve(this->valves[HS::SQc], DB4, gate_valve_SQc_status);
			DI_manual_valve(this->valves[HS::SQd], DB4, gate_valve_SQd_status);
			DI_manual_valve(this->valves[HS::SQe], DB4, gate_valve_SQe_status);
			DI_manual_valve(this->valves[HS::SQf], DB4, gate_valve_SQf_status);

			DI_manual_valve(this->valves[HS::SQa], DB4, gate_valve_SQa_status);
			DI_manual_valve(this->valves[HS::SQb], DB4, gate_valve_SQb_status);
			DI_manual_valve(this->valves[HS::SQg], DB4, gate_valve_SQc_status);
			DI_manual_valve(this->valves[HS::SQh], DB4, gate_valve_SQd_status);
		}

		{ // filter statuses
			DI_filter_alarm(this->alarms[HS::F01], DB4, filter_01_status);
			DI_filter_alarm(this->alarms[HS::F02], DB4, filter_02_status);
			DI_filter_alarm(this->alarms[HS::F10], DB4, filter_10_status);
		}
	}

	void post_read_data(Syslog* logger) override {
		{ // flow oil
			HS ps_path[] = { HS::lt, HS::tl, HS::cl, HS::Master };
			HS sb_path[] = { HS::rt, HS::tr, HS::cr, HS::Master };
			HS mt_path[] = { HS::f02, HS::master };
			
			this->station->append_subtrack(HS::Master, HS::SQ1, oil_color);
			this->station->append_subtrack(HS::Master, HS::SQ2, oil_color);
			this->station->append_subtrack(HS::Visor, HS::SQi, oil_color);
			this->station->append_subtrack(HS::Visor, HS::SQj, oil_color);
			this->station->append_subtrack(HS::Storage, HS::SQk1, oil_color);

			this->try_flow_oil(HS::SQi, HS::I, HS::i, nullptr, 0, oil_color);
			this->try_flow_oil(HS::SQj, HS::J, HS::j, nullptr, 0, oil_color);

			this->try_flow_oil(HS::SQc, HS::C, HS::c, ps_path, oil_color);
			this->try_flow_oil(HS::SQd, HS::D, HS::d, ps_path, oil_color);
			this->try_flow_oil(HS::SQe, HS::E, HS::e, ps_path, oil_color);
			this->try_flow_oil(HS::SQf, HS::F, HS::f, ps_path, oil_color);

			this->try_flow_oil(HS::SQa, HS::A, HS::a, sb_path, oil_color);
			this->try_flow_oil(HS::SQb, HS::B, HS::b, sb_path, oil_color);
			this->try_flow_oil(HS::SQg, HS::G, HS::g, sb_path, oil_color);
			this->try_flow_oil(HS::SQh, HS::H, HS::h, sb_path, oil_color);

			this->try_flow_oil(HS::SQy, HS::Y, HS::y, mt_path, oil_color);
			this->try_flow_oil(HS::SQl, HS::L, HS::l, mt_path, oil_color);
			this->try_flow_oil(HS::SQm, HS::M, HS::m, mt_path, oil_color);
			this->try_flow_oil(HS::SQk1, HS::K, HS::k, mt_path, oil_color);
			this->try_flow_oil(HS::SQk2, HS::K /* , HS::k, mt_path */, oil_color);

			this->try_flow_oil(HS::SQ1, HS::Port, HS::SQe, oil_color);
			this->try_flow_oil(HS::SQ2, HS::sb, HS::SQh, oil_color);
			this->try_flow_oil(HS::SQ2, HS::SQk2, oil_color);
			this->try_flow_oil(HS::SQ2, HS::SQm, oil_color);
		}
		
		this->master->end_update_sequence();
		this->master->leave_critical_section();
	}

public:
	void construct(float gwidth, float gheight) {
		this->caption_font = make_bold_text_format("Microsoft YaHei", large_font_size);
		this->label_font = make_bold_text_format("Microsoft YaHei", small_font_size);

		this->pressure_style.number_font = make_bold_text_format("Cambria Math", large_metrics_font_size);
		this->pressure_style.unit_font = make_bold_text_format("Cambria", normal_font_size);
		this->temperature_style = this->pressure_style;

		this->pressure_style.precision = 0;
		this->temperature_style.precision = 1;
	}
 
public:
	void load_pump_station(float width, float height, float gwidth, float gheight) {
		Turtle<HS>* pTurtle = new Turtle<HS>(gwidth, gheight, true, HS::Master);

		pTurtle->move_right(2)->move_down(5.5F, HS::SQ2);
		pTurtle->move_down()->turn_down_right()->move_right(13, HS::sb)->turn_right_down()->move_down(17);
		
		pTurtle->jump_right(20, HS::h)->move_left(4, HS::H)->move_left(12, HS::SQh)->move_left(4)->jump_back();
		pTurtle->move_up(3, HS::g)->move_left(4, HS::G)->move_left(12, HS::SQg)->move_left(4)->jump_back();
		pTurtle->move_up(3, HS::b)->move_left(4, HS::B)->move_left(12, HS::SQb)->move_left(4)->jump_back();
		pTurtle->move_up(3, HS::a)->move_left(4, HS::A)->move_left(12, HS::SQa)->move_left(4)->jump_back();
		
		pTurtle->move_up(3, HS::Starboard)->move_up(21, HS::rt)->turn_up_left(HS::tr)->move_left(35, HS::cr);
		pTurtle->turn_left_down()->move_down(4)->jump_up(4);
		pTurtle->turn_up_left(HS::cl)->move_left(35, HS::tl)->turn_left_down(HS::lt)->move_down(21);

		pTurtle->move_down(3, HS::c)->move_right(4, HS::C)->move_right(12, HS::SQc)->move_right(4)->jump_back();
		pTurtle->move_down(3, HS::f)->move_right(4, HS::F)->move_right(12, HS::SQf)->move_right(4)->jump_back();
		pTurtle->move_down(3, HS::d)->move_right(4, HS::D)->move_right(12, HS::SQd)->move_right(4)->jump_back();
		pTurtle->move_down(3, HS::e)->move_right(4, HS::E)->move_right(12, HS::SQe)->move_right(4);

		pTurtle->move_up(12, HS::Port)->move_up(5)->turn_up_right()->move_right(13)->turn_right_up();
		pTurtle->move_up(HS::SQ1)->move_up(5.5F)->move_to(HS::Master);

		pTurtle->jump_back(HS::Master)->jump_right(4, HS::master)->move_up(6.5F)->turn_up_right(HS::f02)->move_right(2);
		pTurtle->move_right(5, HS::y)->move_down(6, HS::Y)->move_down(4, HS::SQy)->move_down(4)->turn_down_left()->jump_back();
		pTurtle->move_right(5, HS::l)->move_down(6, HS::L)->move_down(4, HS::SQl)->move_down(4)->turn_down_left()->jump_back();
		pTurtle->move_right(5, HS::m)->move_down(6, HS::M)->move_down(4, HS::SQm)->move_down(4)->turn_down_left()->jump_back();
		pTurtle->move_right(4, HS::k)->turn_right_down()->move_down(5, HS::K)->move_down(9, HS::k12);
		pTurtle->move_left(3, HS::SQk2)->move_left(8)->jump_back()->move_right(3, HS::SQk1)->move_right(2.5F, HS::Storage);
		
		pTurtle->jump_back(HS::Master)->jump_down(14, HS::Visor);
		pTurtle->move_right(2)->move_down(5, HS::SQi)->move_down(3, HS::I)->move_down(3, HS::i);
		pTurtle->jump_left(4, HS::j)->move_up(3, HS::J)->move_up(3, HS::SQj)->move_up(5)->move_right(2 /* HS::Visor */);
		
		this->station = this->master->insert_one(new Tracklet<HS>(pTurtle, default_pipe_thickness, default_pipe_color));
		
		this->load_label(this->captions, HS::Port, Colours::DarkKhaki, this->caption_font);
		this->load_label(this->captions, HS::Starboard, Colours::DarkKhaki, this->caption_font);
		this->load_label(this->captions, HS::Storage, Colours::Silver);
	}

	void load_tanks(float width, float height, float gwidth, float gheight) {
		float thickness = default_pipe_thickness * 2.0F;
		float alarm_size = gwidth * 1.2F;

		this->master_tank = this->make_tank(HSMTStatus::Empty, gwidth * 18.0F, gheight * 8.0F, thickness);
		this->visor_tank = this->make_tank(HSVTStatus::Empty, gwidth * 16.0F, gheight * 7.0F, thickness);
		this->heater = this->master->insert_one(new Heaterlet(gwidth * 1.618F));

		this->load_thermometer(this->thermometers, this->temperatures, HS::Master, gwidth * 2.5F, gheight * 4.5F);
		this->load_thermometer(this->thermometers, this->temperatures, HS::Visor, gwidth * 2.5F, gheight * 4.5F);

		this->storage_tank = this->master->insert_one(new FuelTanklet(gwidth * 2.5F, 0.0F, thickness, Colours::WhiteSmoke));
		
		this->load_filter_alarms(HS::F01, HS::F10, alarm_size, this->alarms, this->islabels);
	}

	void load_devices(float width, float height, float gwidth, float gheight) {
		float radius = resolve_gridsize(gwidth, gheight);
		float pradius = radius * 1.2F;

		{ // load pumps
			this->load_devices(this->pumps, this->labels, this->captions, HS::A, HS::H, pradius, 0.000, Colours::Salmon);
			this->load_devices(this->pumps, this->labels, this->captions, HS::C, HS::E, pradius, 180.0, Colours::Salmon);
			this->load_devices(this->pumps, this->labels, this->captions, HS::Y, HS::K, pradius, -90.0);
			this->load_devices(this->pumps, this->labels, this->captions, HS::J, HS::I, pradius, 90.00);

			this->load_dimensions(this->pressures, HS::A, HS::I, "bar");
			this->load_dimension(this->pressures, HS::BackOil, "bar");
		}

		{ // load valves
			this->load_devices(this->valves, this->labels, HS::SQ1, HS::SQj, radius, 90.000);
			this->load_devices(this->valves, this->labels, HS::SQa, HS::SQk1, radius, 0.000);
			this->load_devices(this->valves, this->labels, HS::SQc, HS::SQe, radius,  0.000);
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
		this->master->move_to(this->pressures[HS::BackOil], this->station, GraphletAnchor::CT, GraphletAnchor::CB);
		this->master->move_to(this->thermometers[HS::Master], this->master_tank, 0.25F, 0.5F, GraphletAnchor::CC);
		this->master->move_to(this->thermometers[HS::Visor], this->visor_tank, 0.25F, 0.5F, GraphletAnchor::CC);
		
		this->station->map_credit_graphlet(this->captions[HS::Port], GraphletAnchor::CB, -gwidth * 10.0F);
		this->station->map_credit_graphlet(this->captions[HS::Starboard], GraphletAnchor::CB, -gwidth * 10.0F);
		this->master->move_to(this->captions[HS::Storage], this->storage_tank, GraphletAnchor::CB, GraphletAnchor::CT);

		{ // reflow heater
			float hspace, vspace;

			this->heater->fill_margin(0.0F, 0.0F, &vspace, &hspace);
			this->master->move_to(this->heater, this->master_tank, GraphletAnchor::CB,
				GraphletAnchor::CB, 0.0F, vspace - hspace);
		}
	}
	
	void reflow_devices(float width, float height, float gwidth, float gheight, float vinset) {
		GraphletAnchor lbl_a, cpt_a, bar_a;
		float lbl_dx, lbl_dy, cpt_dx, cpt_dy, bar_dx, bar_dy, margin;
		float pradius = this->pumps[HS::A]->get_radiusX();
		float vradius = this->valves[HS::SQ1]->get_radiusY();
		float text_hspace = default_pipe_thickness * 2.0F;
		float x0 = 0.0F;
		float y0 = 0.0F;

		for (auto it = this->pumps.begin(); it != this->pumps.end(); it++) {
			switch (it->second->id) {
			case HS::F: case HS::C: case HS::D: case HS::E: {
				lbl_dx = x0 - pradius; lbl_dy = y0; lbl_a = GraphletAnchor::RT;
				cpt_dx = x0 + pradius; cpt_dy = y0; cpt_a = GraphletAnchor::LT;
				bar_dx = x0 + pradius; bar_dy = y0; bar_a = GraphletAnchor::LB;
			} break;
			case HS::A: case HS::B: case HS::G: case HS::H: {
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
			
				if (it->second->id == HS::I) {
					lbl_dx = x0 + pradius; lbl_dy = y0; lbl_a = GraphletAnchor::LT;
					bar_dx = x0 + text_hspace; bar_dy = y0 + pradius; bar_a = GraphletAnchor::LT;
				} else { // HS::J
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
				case HS::SQ1: case HS::SQj: {
					it->second->fill_margin(x0, y0, nullptr, nullptr, nullptr, &margin);
					lbl_dx = x0 - vradius + margin; lbl_dy = y0; lbl_a = GraphletAnchor::RC;
				}; break;
				default: {
					lbl_dx = x0 + vradius; lbl_dy = y0; lbl_a = GraphletAnchor::LC;
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
		this->master->move_to(this->temperatures[HS::Master], this->thermometers[HS::Master], 1.0F, 0.75F, GraphletAnchor::LC, gwidth);
		this->master->move_to(this->temperatures[HS::Visor], this->thermometers[HS::Visor], 1.0F, 0.75F, GraphletAnchor::LC, gwidth);
		
		{ // reflow alarms
			float lblgap = vinset * 0.25F;
			float vgap = lblgap * 0.5F;

			this->master->move_to(this->alarms[HS::F01], this->thermometers[HS::Master], 1.0F, 0.25F, GraphletAnchor::LB, gwidth, -vgap);
			this->master->move_to(this->alarms[HS::F02], this->thermometers[HS::Master], 1.0F, 0.25F, GraphletAnchor::LT, gwidth, +vgap);
			this->master->move_to(this->alarms[HS::F10], this->thermometers[HS::Visor], 1.0F, 0.25F, GraphletAnchor::LC, gwidth);

			for (auto lt = this->alarms.begin(); lt != this->alarms.end(); lt++) {
				this->master->move_to(this->islabels[lt->first], this->alarms[lt->first],
					GraphletAnchor::RC, GraphletAnchor::LC, lblgap);
			}
		}
	}

public:
	bool pumps_selected(HS id0, HS idn, int tolerance) {
		bool okay = false;
		int ok = 0;

		for (HS id = id0; id <= idn; id++) {
			if (this->master->is_selected(this->pumps[id])) {
				ok += 1;

				if (ok >= tolerance) {
					okay = true;
					break;
				}
			}
		}

		return okay;
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
	void load_dimension(std::map<E, Credit<Dimensionlet, E>*>& ds, E id, Platform::String^ unit) {
		ds[id] = this->master->insert_one(new Credit<Dimensionlet, E>(this->pressure_style, unit, _speak(id.ToString())), id);
	}

	template<typename E>
	void load_dimensions(std::map<E, Credit<Dimensionlet, E>*>& ds, E id0, E idn, Platform::String^ unit) {
		for (E id = id0; id <= idn; id++) {
			ds[id] = this->master->insert_one(new Credit<Dimensionlet, E>(this->pressure_style, unit), id);
		}
	}

	template<class A, typename E>
	void load_filter_alarms(E id0, E idn, float size, std::map<E, Credit<A, E>*>& bs, std::map<E, Credit<Labellet, E>*>& ls) {
		for (E id = id0; id <= idn; id++) {
			this->load_label(ls, id, Colours::Silver);
			bs[id] = this->master->insert_one(new Credit<A, E>(size), id);
		}
	}

	template<class T, typename E>
	void load_thermometer(std::map<E, Credit<T, E>*>& ts, std::map<E, Credit<Dimensionlet, E>*>& ds, E id, float width, float height) {
		ts[id] = this->master->insert_one(new Credit<T, E>(100.0, width, height, 2.5F), id);
		ds[id] = this->master->insert_one(new Credit<Dimensionlet, E>(this->temperature_style, "celsius", _speak(id)), id);
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

private:
	void try_flow_oil(HS vid, HS pid, CanvasSolidColorBrush^ color) {
		switch (this->valves[vid]->get_status()) {
		case ManualValveStatus::Open: {
			this->station->append_subtrack(vid, pid, color);
		}
		}
	}

	void try_flow_oil(HS vid, HS mid, HS eid, CanvasSolidColorBrush^ color) {
		switch (this->valves[vid]->get_status()) {
		case ManualValveStatus::Open: {
			this->station->append_subtrack(vid, mid, color);
			this->station->append_subtrack(mid, eid, color);
		}
		}
	}

	void try_flow_oil(HS vid, HS pid, HS _id, HS* path, unsigned int count, CanvasSolidColorBrush^ color) {
		this->try_flow_oil(vid, pid, color);

		switch (this->pumps[pid]->get_status()) {
		case HydraulicPumpStatus::Running: case HydraulicPumpStatus::StopReady: {
			this->station->append_subtrack(pid, _id, oil_color);

			if (path != nullptr) {
				this->station->append_subtrack(_id, path[0], oil_color);
				this->station->append_subtrack(path, count, color);
			}
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
	Heaterlet* heater;
	Tanklet<HSMTStatus>* master_tank;
	Tanklet<HSVTStatus>* visor_tank;
	std::map<HS, Credit<Thermometerlet, HS>*> thermometers;
	std::map<HS, Credit<Dimensionlet, HS>*> temperatures;
	std::map<HS, Credit<Labellet, HS>*> captions;
	std::map<HS, Credit<Labellet, HS>*> labels;
	std::map<HS, Credit<HydraulicPumplet, HS>*> pumps;
	std::map<HS, Credit<ManualValvelet, HS>*> valves;
	std::map<HS, Credit<Dimensionlet, HS>*> pressures;
	std::map<HS, Credit<Alarmlet, HS>*> alarms;
	std::map<HS, Credit<Labellet, HS>*> islabels;
	
private:
	CanvasTextFormat^ caption_font;
	CanvasTextFormat^ label_font;
	DimensionStyle pressure_style;
	DimensionStyle temperature_style;

private:
	HydraulicsPage* master;
};

HydraulicsPage::HydraulicsPage(PLCMaster* plc) : Planet(__MODULE__), device(plc) {
	Hydraulics* dashboard = new Hydraulics(this);

	this->dashboard = dashboard;
	this->grid = new GridDecorator();
	
	this->gbs_op = make_hydraulics_group_menu(HydraulicsGroup::BothPumps, plc);
	this->gps_op = make_hydraulics_group_menu(HydraulicsGroup::PSPumps, plc);
	this->gsb_op = make_hydraulics_group_menu(HydraulicsGroup::SBPumps, plc);
	this->gvisor_op = make_hydraulics_group_menu(HydraulicsGroup::VisorPumps, plc);
	this->pump_op = make_hydraulic_pump_menu(DO_hydraulics_action, plc);
	this->heater_op = make_tank_heater_menu(plc);
	
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
		|| (dynamic_cast<Heaterlet*>(g) != nullptr));
}

bool HydraulicsPage::can_select_multiple() {
	return true;
}

void HydraulicsPage::on_tap_selected(IGraphlet* g, float local_x, float local_y) {
	auto pump = dynamic_cast<HydraulicPumplet*>(g);
	auto heater = dynamic_cast<Heaterlet*>(g);

	if (pump != nullptr) {
		menu_popup(this->pump_op, g, local_x, local_y);
	} else if (heater != nullptr) {
		menu_popup(this->heater_op, g, local_x, local_y);
	}
}

void HydraulicsPage::on_gesture(std::list<float2>& anchors, float x, float y) {
	auto dashboard = dynamic_cast<Hydraulics*>(this->dashboard);

	if (dashboard != nullptr) {
		if (dashboard->pumps_selected(HS::C, HS::E, 1) && dashboard->pumps_selected(HS::A, HS::H, 1)) {
			group_menu_popup(this->gbs_op, this, x, y);
		} else if (dashboard->pumps_selected(HS::C, HS::E, 2)) {
			group_menu_popup(this->gps_op, this, x, y);
		} else if (dashboard->pumps_selected(HS::A, HS::H, 2)) {
			group_menu_popup(this->gsb_op, this, x, y);
		} else if (dashboard->pumps_selected(HS::J, HS::I, 2)) {
			group_menu_popup(this->gvisor_op, this, x, y);
		}
	}
}
