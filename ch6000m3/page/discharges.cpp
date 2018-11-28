#include <map>

#include "page/discharges.hpp"
#include "configuration.hpp"
#include "menu.hpp"

#include "module.hpp"
#include "text.hpp"
#include "paint.hpp"
#include "brushes.hxx"
#include "turtle.hpp"

#include "graphlet/shapelet.hpp"
#include "graphlet/symbol/door/hatchlet.hpp"
#include "graphlet/symbol/door/hopper_doorlet.hpp"
#include "graphlet/symbol/pump/hopper_pumplet.hpp"
#include "graphlet/symbol/valve/gate_valvelet.hpp"
#include "graphlet/symbol/valve/tagged_valvelet.hpp"

#include "iotables/di_pumps.hpp"
#include "iotables/di_hopper_pumps.hpp"
#include "iotables/di_valves.hpp"
#include "iotables/di_doors.hpp"

#include "iotables/ai_valves.hpp"
#include "iotables/ai_pumps.hpp"
#include "iotables/ai_hopper_pumps.hpp"
#include "iotables/ai_doors.hpp"

#include "iotables/do_doors.hpp"
#include "iotables/do_valves.hpp"
#include "iotables/do_hopper_pumps.hpp"

#include "decorator/page.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Windows::System;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

private enum RSMode { WindowUI = 0, Dashboard };

// WARNING: order matters
private enum class RS : unsigned int {
	// Valves
	D001, D002, D006, D008, D010, D017, D018, D019, D020, D021, D022, D024,
	D003, D007, D023, D025,
	D004, D005, D009,

	// Pump dimensions
	A, C, F, H,

	// Key Labels
	Port, Starboard, Hatch, PSHPump, SBHPump, Gantry,
	
	_,

	// anchors used as last jumping points
	d0225, d0325, d0406,
	d1720, d1819, d1920, d2122,

	// anchors used for unnamed nodes
	ps, sb, d007, deck_lx, deck_rx, deck_ty, deck_by,

	// anchors used for non-interconnected nodes
	n24, n0325, n0405, n0723, n0923,

	// unused, but template requires them
	D011, D012, D013, D014, D015, D016, D026
};

static uint16 DO_gate_valve_action(GateValveAction cmd, GateValvelet* valve) {
	uint16 index = 0U;
	auto credit_valve = dynamic_cast<Credit<GateValvelet, RS>*>(valve);

	if (credit_valve != nullptr) {
		index = DO_gate_valve_command(cmd, credit_valve->id);
	}

	return index;
}

/*************************************************************************************************/
private class Rainbows final : public PLCConfirmation {
public:
	Rainbows(DischargesPage* master) : master(master) {}

public:
	void pre_read_data(Syslog* logger) override {
		this->master->enter_critical_section();
		this->master->begin_update_sequence();
	}

	void on_analog_input(const uint8* DB2, size_t count2, const uint8* DB203, size_t count203, Syslog* logger) override {
		this->pump_pressures[RS::C]->set_value(RealData(DB203, pump_C_pressure), GraphletAnchor::LB);
		this->pump_pressures[RS::F]->set_value(RealData(DB203, pump_F_pressure), GraphletAnchor::LT);

		this->pump_pressures[RS::A]->set_value(RealData(DB203, pump_A_pressure), GraphletAnchor::LB);
		this->pump_pressures[RS::H]->set_value(RealData(DB203, pump_H_pressure), GraphletAnchor::LT);

		this->suctions[RS::D003]->set_value(RealData(DB203, gate_valve_D03_progress), GraphletAnchor::LB);
		this->suctions[RS::D004]->set_value(RealData(DB203, gate_valve_D04_progress), GraphletAnchor::LT);

		this->powers[RS::PSHPump]->set_value(RealData(DB203, ps_hopper_pump_power), GraphletAnchor::RC);
		this->rpms[RS::PSHPump]->set_value(RealData(DB203, ps_hopper_pump_rpm), GraphletAnchor::LC);
		this->dpressures[RS::PSHPump]->set_value(RealData(DB203, ps_hopper_pump_discharge_pressure), GraphletAnchor::LB);
		this->vpressures[RS::PSHPump]->set_value(RealData(DB203, ps_hopper_pump_vacuum_pressure), GraphletAnchor::RB);
		
		this->powers[RS::SBHPump]->set_value(RealData(DB203, sb_hopper_pump_power), GraphletAnchor::RC);
		this->rpms[RS::SBHPump]->set_value(RealData(DB203, sb_hopper_pump_rpm), GraphletAnchor::LC);
		this->dpressures[RS::SBHPump]->set_value(RealData(DB203, sb_hopper_pump_discharge_pressure), GraphletAnchor::LT);
		this->vpressures[RS::SBHPump]->set_value(RealData(DB203, sb_hopper_pump_vacuum_pressure), GraphletAnchor::RT);
		
		{ // door progresses
			this->set_door_progress(Door::PS1, RealData(DB203, upper_door_PS1_progress));
			this->set_door_progress(Door::PS2, RealData(DB203, upper_door_PS2_progress));
			this->set_door_progress(Door::PS3, RealData(DB203, upper_door_PS3_progress));
			this->set_door_progress(Door::PS4, RealData(DB203, upper_door_PS4_progress));
			this->set_door_progress(Door::PS5, RealData(DB203, upper_door_PS5_progress));
			this->set_door_progress(Door::PS6, RealData(DB203, upper_door_PS6_progress));
			this->set_door_progress(Door::PS7, RealData(DB203, upper_door_PS7_progress));

			this->set_door_progress(Door::SB1, RealData(DB203, upper_door_SB1_progress));
			this->set_door_progress(Door::SB2, RealData(DB203, upper_door_SB2_progress));
			this->set_door_progress(Door::SB3, RealData(DB203, upper_door_SB3_progress));
			this->set_door_progress(Door::SB4, RealData(DB203, upper_door_SB4_progress));
			this->set_door_progress(Door::SB5, RealData(DB203, upper_door_SB5_progress));
			this->set_door_progress(Door::SB6, RealData(DB203, upper_door_SB6_progress));
			this->set_door_progress(Door::SB7, RealData(DB203, upper_door_SB7_progress));
		}
	}

	void on_digital_input(const uint8* DB4, size_t count4, const uint8* DB205, size_t count205, WarGrey::SCADA::Syslog* logger) override {
		DI_hopper_pump(this->hoppers[RS::PSHPump], DB4, ps_hopper_pump_feedback, DB205, ps_hopper_pump_details);
		DI_hopper_pump(this->hoppers[RS::SBHPump], DB4, sb_hopper_pump_feedback, DB205, sb_hopper_pump_details);

		DI_hydraulic_pump_dimension(this->pump_pressures[RS::A], DB4, pump_A_feedback);
		DI_hydraulic_pump_dimension(this->pump_pressures[RS::C], DB4, pump_C_feedback);
		DI_hydraulic_pump_dimension(this->pump_pressures[RS::F], DB4, pump_F_feedback);
		DI_hydraulic_pump_dimension(this->pump_pressures[RS::H], DB4, pump_H_feedback);

		this->set_valves_status(RS::D001, DB4, gate_valve_D01_feedback, motor_valve_D01_feedback, DB205, gate_valve_D01_status, motor_valve_D01_status);
		this->set_valves_status(RS::D002, DB4, gate_valve_D02_feedback, motor_valve_D02_feedback, DB205, gate_valve_D02_status, motor_valve_D02_status);
		this->set_valves_status(RS::D005, DB4, gate_valve_D05_feedback, motor_valve_D05_feedback, DB205, gate_valve_D05_status, motor_valve_D05_status);
		this->set_valves_status(RS::D006, DB4, gate_valve_D06_feedback, motor_valve_D06_feedback, DB205, gate_valve_D06_status, motor_valve_D06_status);
		this->set_valves_status(RS::D007, DB4, gate_valve_D07_feedback, motor_valve_D07_feedback, DB205, gate_valve_D07_status, motor_valve_D07_status);
		this->set_valves_status(RS::D008, DB4, gate_valve_D08_feedback, motor_valve_D08_feedback, DB205, gate_valve_D08_status, motor_valve_D08_status);
		this->set_valves_status(RS::D009, DB4, gate_valve_D09_feedback, motor_valve_D09_feedback, DB205, gate_valve_D09_status, motor_valve_D09_status);
		this->set_valves_status(RS::D010, DB4, gate_valve_D10_feedback, motor_valve_D10_feedback, DB205, gate_valve_D10_status, motor_valve_D10_status);
		this->set_valves_status(RS::D017, DB4, gate_valve_D17_feedback, motor_valve_D17_feedback, DB205, gate_valve_D17_status, motor_valve_D17_status);
		this->set_valves_status(RS::D018, DB4, gate_valve_D18_feedback, motor_valve_D18_feedback, DB205, gate_valve_D18_status, motor_valve_D18_status);
		this->set_valves_status(RS::D019, DB4, gate_valve_D19_feedback, motor_valve_D19_feedback, DB205, gate_valve_D19_status, motor_valve_D19_status);
		this->set_valves_status(RS::D020, DB4, gate_valve_D20_feedback, motor_valve_D20_feedback, DB205, gate_valve_D20_status, motor_valve_D20_status);
		this->set_valves_status(RS::D021, DB4, gate_valve_D21_feedback, motor_valve_D21_feedback, DB205, gate_valve_D21_status, motor_valve_D21_status);
		this->set_valves_status(RS::D022, DB4, gate_valve_D22_feedback, motor_valve_D22_feedback, DB205, gate_valve_D22_status, motor_valve_D22_status);
		this->set_valves_status(RS::D023, DB4, gate_valve_D23_feedback, motor_valve_D23_feedback, DB205, gate_valve_D23_status, motor_valve_D23_status);
		this->set_valves_status(RS::D024, DB4, gate_valve_D24_feedback, motor_valve_D24_feedback, DB205, gate_valve_D24_status, motor_valve_D24_status);
		this->set_valves_status(RS::D025, DB4, gate_valve_D25_feedback, motor_valve_D25_feedback, DB205, gate_valve_D25_status, motor_valve_D25_status);
		
		DI_gate_valve(this->gvalves[RS::D003], DB205, gate_valve_D03_feedback, DB205, gate_valve_D03_status);
		DI_motor_valve(this->mvalves[RS::D003], DB4, motor_valve_D03_feedback, DB205, motor_valve_D03_status);

		DI_gate_valve(this->gvalves[RS::D004], DB205, gate_valve_D04_feedback, DB205, gate_valve_D04_status);
		DI_motor_valve(this->mvalves[RS::D004], DB4, motor_valve_D04_feedback, DB205, motor_valve_D04_status);

		DI_hopper_door(this->uhdoors[Door::PS1], DB205, upper_door_PS1_status);
		DI_hopper_door(this->uhdoors[Door::PS2], DB205, upper_door_PS2_status);
		DI_hopper_door(this->uhdoors[Door::PS3], DB205, upper_door_PS3_status);
		DI_hopper_door(this->uhdoors[Door::PS4], DB205, upper_door_PS4_status);
		DI_hopper_door(this->uhdoors[Door::PS5], DB205, upper_door_PS5_status);
		DI_hopper_door(this->uhdoors[Door::PS6], DB205, upper_door_PS6_status);
		DI_hopper_door(this->uhdoors[Door::PS7], DB205, upper_door_PS7_status);

		DI_hopper_door(this->uhdoors[Door::SB1], DB205, upper_door_SB1_status);
		DI_hopper_door(this->uhdoors[Door::SB2], DB205, upper_door_SB2_status);
		DI_hopper_door(this->uhdoors[Door::SB3], DB205, upper_door_SB3_status);
		DI_hopper_door(this->uhdoors[Door::SB4], DB205, upper_door_SB4_status);
		DI_hopper_door(this->uhdoors[Door::SB5], DB205, upper_door_SB5_status);
		DI_hopper_door(this->uhdoors[Door::SB6], DB205, upper_door_SB6_status);
		DI_hopper_door(this->uhdoors[Door::SB7], DB205, upper_door_SB7_status);
	}

	void post_read_data(Syslog* logger) override {
		this->master->end_update_sequence();
		this->master->leave_critical_section();
	}

public:
	void construct(float gwidth, float gheight) {
		this->caption_font = make_bold_text_format("Microsoft YaHei", normal_font_size);
		this->label_font = make_bold_text_format("Microsoft YaHei", small_font_size);
		this->pump_style = make_highlight_dimension_style(large_metrics_font_size, 6U, 0, Colours::Background);
		this->highlight_style = make_highlight_dimension_style(large_metrics_font_size, 6U, 0, Colours::Green);
		this->relationship_style = make_dash_stroke(CanvasDashStyle::DashDot);
		this->relationship_color = Colours::DarkGray;

		this->hopper_style.number_font = make_bold_text_format("Cambria Math", large_metrics_font_size);
		this->hopper_style.unit_font = make_bold_text_format("Cambria", normal_font_size);
	}
 
public:
	void load(float width, float height, float gwidth, float gheight) {
		float radius = resolve_gridsize(gwidth, gheight);
		Turtle<RS>* pTurtle = new Turtle<RS>(gwidth, gheight, false);

		pTurtle->move_left(RS::deck_rx)->move_left(2, RS::D021)->move_left(2, RS::d2122);
		pTurtle->move_down(5)->move_right(2, RS::D022)->move_right(3)->jump_back();
		pTurtle->move_left(2, RS::d1920)->move_left(2, RS::D020)->move_left(7, RS::d1720);

		pTurtle->move_left(3, RS::D017)->move_left(11, RS::n0405)->move_left(4, RS::D010)->jump_back(RS::d1720);
		
		pTurtle->move_down(3.5F, RS::PSHPump)->move_left(6, RS::n0923)->move_left(8)->move_up(1.5F, RS::D005)->move_up(1.5F)->jump_up();
		pTurtle->move_up(3, RS::d0406)->move_right(4, RS::D006)->move_right(4)->move_down(0.5F, RS::deck_ty)->move_down(RS::D009);
		pTurtle->move_down(5)->jump_down()->move_down(2, RS::D023)->jump_back(RS::d0406);

		pTurtle->move_up(1.5F, RS::D004)->move_up(2, RS::ps)->move_up(2, RS::C)->move_up(RS::Port);

		pTurtle->jump_back(RS::D023)->move_down(2)->jump_down()->move_down(5, RS::D007);
		pTurtle->move_down(RS::deck_by)->move_down(0.5F, RS::d007)->jump_left(8, RS::d0325);
		pTurtle->move_up(3)->jump_up()->move_up(1.5F, RS::D025)->move_up(1.5F, RS::d0225);
		pTurtle->move_right(8, RS::n0723)->move_right(6, RS::SBHPump)->move_down(3.5F, RS::d1819)->jump_back(RS::d0225);
		pTurtle->jump_up(2.5F)->move_left(2, RS::D002)->move_left(15, RS::n24)->move_left(10, RS::D001)->move_left(3, RS::Hatch);

		pTurtle->jump_back(RS::d1819)->move_left(3, RS::D018)->move_left(11, RS::n0325)->move_left(4, RS::D008);
		pTurtle->move_left(13)->move_up(5.5F)->jump_up()->move_up(6.5F);
		pTurtle->move_up(2.5F)->turn_up_left()->move_left(3, RS::D024)->move_left(3)->turn_left_up();
		pTurtle->move_up(0.5F, RS::Gantry)->move_left()->jump_back(RS::Gantry)->move_right()->jump_back(RS::d0325);

		pTurtle->move_down(1.5F, RS::D003)->move_down(2, RS::sb)->move_down(2, RS::F)->move_down(RS::Starboard);

		pTurtle->jump_back(RS::d1819)->move_right(5, RS::deck_lx)->move_right(2, RS::D019)->move_right(2)->move_to(RS::d1920);
		
		this->station = this->master->insert_one(new Tracklet<RS>(pTurtle, default_pipe_thickness, default_pipe_color));

		{ // load manual pipe segement
			float d02_y, d05_y;

			this->station->fill_anchor_location(RS::D002, nullptr, &d02_y);
			this->station->fill_anchor_location(RS::D005, nullptr, &d05_y);

			this->manual_pipe = this->master->insert_one(
				new Linelet(0.0F, d02_y, 0.0F, d05_y,
					default_pipe_thickness, default_pipe_color));
		}

		{ // load doors and valves
			this->load_doors(this->uhdoors, this->progresses, Door::PS1, Door::PS7, radius);
			this->load_doors(this->uhdoors, this->progresses, Door::SB1, Door::SB7, radius);
		
			this->load_valve(this->gvalves, this->vlabels, this->captions, RS::D001, radius, 0.0);
			this->load_valves(this->gvalves, this->mvalves, this->vlabels, this->captions, RS::D002, RS::D024, radius, 00.0);
			this->load_valves(this->gvalves, this->mvalves, this->vlabels, this->captions, RS::D003, RS::D025, radius, 90.0);
			this->load_valves(this->gvalves, this->mvalves, this->vlabels, this->captions, RS::D004, RS::D009, radius, -90.0);
		}

		{ // load special nodes
			float sct_radius = radius * 0.5F;
			float nic_radius = radius * 0.25F;
			
			this->load_pump(this->hoppers, this->captions, RS::PSHPump, -radius, +2.0F);
			this->load_pump(this->hoppers, this->captions, RS::SBHPump, -radius, -2.0F);
			this->ps_suction = this->master->insert_one(new Circlelet(sct_radius, default_ps_color, default_pipe_thickness));
			this->sb_suction = this->master->insert_one(new Circlelet(sct_radius, default_sb_color, default_pipe_thickness));
			this->sea_inlet = this->master->insert_one(new Hatchlet(radius * 2.0F));

			for (RS id = RS::n24; id <= RS::n0923; id++) {
				this->nintercs[id] = this->master->insert_one(
					new Omegalet(-90.0, nic_radius, default_pipe_thickness, default_pipe_color));
			}
		}

		{ // load labels and dimensions
			this->load_percentage(this->suctions, RS::D003);
			this->load_percentage(this->suctions, RS::D004);
			this->load_dimensions(this->pump_pressures, RS::A, RS::H, "bar");

			this->load_label(this->captions, RS::Hatch, Colours::SeaGreen, this->caption_font);
			this->load_label(this->captions, RS::Gantry, Colours::Yellow, this->caption_font);

			for (size_t idx = 0; idx < hopper_count; idx++) {
				Platform::String^ id = (idx + 1).ToString();

				this->ps_seqs[idx] = this->master->insert_one(new Labellet(_speak("PS" + id), this->caption_font, Colours::Silver));
				this->sb_seqs[idx] = this->master->insert_one(new Labellet(_speak("SB" + id), this->caption_font, Colours::Silver));
			}
		}
	}

public:
	void reflow(float width, float height, float gwidth, float gheight, float vinset) {
		GraphletAnchor anchor;
		float dx, dy, margin, label_height, ox, oy;
		float gridsize = resolve_gridsize(gwidth, gheight);
		float x0 = 0.0F;
		float y0 = 0.0F;

		this->master->move_to(this->station, width * 0.5F, height * 0.5F, GraphletAnchor::CC);
		this->station->map_graphlet_at_anchor(this->manual_pipe, RS::D025, GraphletAnchor::CB);

		this->station->map_credit_graphlet(this->captions[RS::Gantry], GraphletAnchor::CB);
		this->station->map_graphlet_at_anchor(this->ps_suction, RS::Port, GraphletAnchor::CC);
		this->station->map_graphlet_at_anchor(this->sb_suction, RS::Starboard, GraphletAnchor::CC);
		this->station->map_graphlet_at_anchor(this->sea_inlet, RS::Hatch, GraphletAnchor::CC);
		this->master->move_to(this->captions[RS::Hatch], this->sea_inlet, GraphletAnchor::CB, GraphletAnchor::CT);

		for (auto it = this->nintercs.begin(); it != this->nintercs.end(); it++) {
			/** NOTE
			 * Lines are brush-based shape, they do not have stroke, `Shapelet` does not know how width they are,
			 * thus, we have to do aligning on our own.
			 */
			this->station->map_graphlet_at_anchor(it->second, it->first, GraphletAnchor::LC, -default_pipe_thickness * 0.5F);
		}

		this->reflow_doors(this->uhdoors, this->progresses, Door::PS1, Door::PS7, gheight * -2.5F);
		this->reflow_doors(this->uhdoors, this->progresses, Door::SB1, Door::SB7, gheight * +2.5F);

		for (auto it = this->hoppers.begin(); it != this->hoppers.end(); it++) {
			it->second->fill_pump_origin(&ox);
			this->station->map_credit_graphlet(it->second, GraphletAnchor::CC, -ox);

			ox = std::fabsf(ox);
			switch (it->first) {
			case RS::PSHPump: {
				this->master->move_to(this->captions[it->first], it->second, GraphletAnchor::RC, GraphletAnchor::LC, ox);
				this->master->move_to(this->powers[it->first], it->second, GraphletAnchor::LB, GraphletAnchor::RB, -ox);
				this->master->move_to(this->rpms[it->first], it->second, GraphletAnchor::RB, GraphletAnchor::LB, ox);
				this->master->move_to(this->dpressures[it->first], it->second, GraphletAnchor::CT, GraphletAnchor::LB);
				this->master->move_to(this->vpressures[it->first], it->second, GraphletAnchor::LC, GraphletAnchor::RB, -ox);
			}; break;
			case RS::SBHPump: {
				this->master->move_to(this->captions[it->first], it->second, GraphletAnchor::RC, GraphletAnchor::LC, ox);
				this->master->move_to(this->powers[it->first], it->second, GraphletAnchor::LT, GraphletAnchor::RT, -ox);
				this->master->move_to(this->rpms[it->first], it->second, GraphletAnchor::RT, GraphletAnchor::LT, ox);
				this->master->move_to(this->dpressures[it->first], it->second, GraphletAnchor::CB, GraphletAnchor::LT);
				this->master->move_to(this->vpressures[it->first], it->second, GraphletAnchor::LC, GraphletAnchor::RT, -ox);
			}; break;
			}
		}

		this->vlabels[RS::D001]->fill_extent(0.0F, 0.0F, nullptr, &label_height);
		
		for (auto it = this->gvalves.begin(); it != this->gvalves.end(); it++) {
			switch (it->first) {
			case RS::D006: case RS::D010: case RS::D020: case RS::D021: case RS::D022: case RS::D024: {
				it->second->fill_margin(x0, y0, nullptr, nullptr, &margin, nullptr);
				dx = x0; dy = y0 + gridsize - margin; anchor = GraphletAnchor::CT;
			}; break;
			case RS::D017: {
				dx = x0 + gwidth; dy = y0 - label_height; anchor = GraphletAnchor::LB;
			}; break;
			case RS::D018: {
				dx = x0 + gwidth; dy = y0; anchor = GraphletAnchor::LT;
			}; break;
			case RS::D001: case RS::D002: case RS::D008: case RS::D019: {
				it->second->fill_margin(x0, y0, &margin, nullptr, nullptr, nullptr);
				dx = x0; dy = y0 - gridsize - label_height + margin; anchor = GraphletAnchor::CB;
			}; break;
			default: {
				it->second->fill_margin(x0, y0, nullptr, &margin, nullptr, nullptr);
				dx = x0 + gridsize - margin; dy = y0; anchor = GraphletAnchor::LB;
			}
			}

			this->station->map_credit_graphlet(it->second, GraphletAnchor::CC, x0, y0);
			this->station->map_credit_graphlet(this->captions[it->first], anchor, dx, dy);
			this->master->move_to(this->vlabels[it->first], this->captions[it->first], GraphletAnchor::CB, GraphletAnchor::CT);
		}

		for (auto it = this->mvalves.begin(); it != this->mvalves.end(); it++) {
			switch (it->first) {
			case RS::D003: case RS::D004: case RS::D005: case RS::D007: case RS::D009:
			case RS::D023: case RS::D025: {
				this->gvalves[RS::D003]->fill_margin(x0, y0, nullptr, nullptr, nullptr, &margin);
				dx = x0 - gridsize + margin; dy = y0; anchor = GraphletAnchor::RC;
			}; break;
			case RS::D002: case RS::D008: case RS::D017: case RS::D019: {
				dx = x0; dy = y0 + gridsize; anchor = GraphletAnchor::CC;
			}; break;
			default: {
				dx = x0; dy = y0 - gridsize; anchor = GraphletAnchor::CC;
			}
			}

			it->second->fill_valve_origin(&ox, &oy);
			this->station->map_credit_graphlet(it->second, anchor, dx - ox, dy - oy);
		}

		{ // reflow door sequences
			float ps_x, ps_y, sb_x, sb_y;

			this->station->fill_anchor_location(RS::D010, nullptr, &ps_y);
			this->station->fill_anchor_location(RS::D008, nullptr, &sb_y);

			for (unsigned int idx = 0; idx < hopper_count; idx++) {
				this->master->fill_graphlet_location(this->uhdoors[_E(Door, idx + _I(Door::PS1))], &ps_x, nullptr, GraphletAnchor::CC);
				this->master->fill_graphlet_location(this->uhdoors[_E(Door, idx + _I(Door::SB1))], &sb_x, nullptr, GraphletAnchor::CC);
				
				this->master->move_to(this->ps_seqs[idx], ps_x, ps_y, GraphletAnchor::CT);
				this->master->move_to(this->sb_seqs[idx], sb_x, sb_y, GraphletAnchor::CB);
			}
		}

		{ // reflow settings dimensions
			float offset = default_pipe_thickness * 2.0F;
			
			this->master->move_to(this->suctions[RS::D003], this->gvalves[RS::D003], GraphletAnchor::CB, GraphletAnchor::LT, offset, -offset);
			this->master->move_to(this->suctions[RS::D004], this->gvalves[RS::D004], GraphletAnchor::CT, GraphletAnchor::LB, offset);
			
			this->station->map_credit_graphlet(this->pump_pressures[RS::C], GraphletAnchor::LB, gwidth * 3.0F);
			this->station->map_credit_graphlet(this->pump_pressures[RS::F], GraphletAnchor::LT, gwidth * 3.0F);
			this->master->move_to(this->pump_pressures[RS::A], this->pump_pressures[RS::C], GraphletAnchor::RC, GraphletAnchor::LC, gwidth);
			this->master->move_to(this->pump_pressures[RS::H], this->pump_pressures[RS::F], GraphletAnchor::RC, GraphletAnchor::LC, gwidth);
		}
	}

public:
	void draw_relationships(CanvasDrawingSession^ ds, float Width, float Height) {
		float ox, oy, sx, sy, tx, ty;

		for (auto it = this->mvalves.begin(); it != this->mvalves.end(); it++) {
			this->master->fill_graphlet_location(it->second, &sx, &sy, GraphletAnchor::CC);
			this->master->fill_graphlet_location(this->gvalves[it->first], &tx, &ty, GraphletAnchor::CC);
			it->second->fill_valve_origin(&ox, &oy);

			ds->DrawLine(sx + ox, sy + oy, tx, ty, this->relationship_color, 1.0F, this->relationship_style);
		}

		for (unsigned int idx = 0; idx < hopper_count; idx++) {
			this->master->fill_graphlet_location(this->uhdoors[_E(Door, idx + _I(Door::PS1))], &sx, &sy, GraphletAnchor::CC);
			this->master->fill_graphlet_location(this->uhdoors[_E(Door, idx + _I(Door::SB1))], &tx, &ty, GraphletAnchor::CC);
			
			ds->DrawLine(sx, sy, tx, ty, this->relationship_color, 1.0F, this->relationship_style);
		}
	}

private:
	template<class G, typename E>
	void load_valve(std::map<E, G*>& gs, std::map<E, Credit<Labellet, E>*>& ls, std::map<E, Credit<Labellet, E>*>& cs
		, E id, float radius, double degrees) {
		this->load_label(ls, "(" + id.ToString() + ")", id, Colours::Silver, this->label_font);
		this->load_label(cs, id, Colours::Silver, this->label_font);

		gs[id] = this->master->insert_one(new G(radius, degrees), id);
	}
	
	template<class G, class M, typename E>
	void load_valves(std::map<E, G*>& gs, std::map<E, M*>& ms, std::map<E, Credit<Labellet, E>*>& ls
		, std::map<E, Credit<Labellet, E>*>& cs, E id0, E idn, float radius, double degrees) {
		float mradius = radius * 0.8F;

		for (E id = id0; id <= idn; id++) {
			double mdegrees = 0.0;

			switch (id) {
			case RS::D002: case RS::D008: case RS::D009: case RS::D017: case RS::D019: mdegrees = -180.0; break;
			}

			// moter-driven valves' second, catching events first 
			this->load_valve(gs, ls, cs, id, radius, degrees);
			ms[id] = this->master->insert_one(new M(mradius, mdegrees, false), id);
		}
	}

	template<class D, typename E>
	void load_doors(std::map<E, Credit<D, E>*>& ds, std::map<E, Credit<Percentagelet, E>*>& ps, E id0, E idn, float radius) {
		for (E id = id0; id <= idn; id++) {
			ds[id] = this->master->insert_one(new Credit<D, E>(radius), id);
			this->load_percentage(ps, id);
		}
	}

	template<class G, typename E>
	void load_pump(std::map<E, G*>& gs, std::map<E, Credit<Labellet, E>*>& ls, E id, float rx, float fy) {
		this->load_label(ls, id, Colours::Salmon, this->caption_font);

		gs[id] = this->master->insert_one(new G(rx, std::fabsf(rx) * fy), id);

		this->load_dimension(this->powers, id, "kwatt");
		this->load_dimension(this->rpms, id, "rpm");
		this->load_dimension(this->dpressures, id, "bar");
		this->load_dimension(this->vpressures, id, "bar");
	}

	template<typename E>
	void load_percentage(std::map<E, Credit<Percentagelet, E>*>& ps, E id) {
		ps[id] = this->master->insert_one(new Credit<Percentagelet, E>(this->plain_style), id);
	}

	template<typename E>
	void load_dimension(std::map<E, Credit<Dimensionlet, E>*>& ds, E id, Platform::String^ unit) {
		ds[id] = this->master->insert_one(new Credit<Dimensionlet, E>(this->hopper_style, unit), id);
	}

	template<typename E>
	void load_dimensions(std::map<E, Credit<Dimensionlet, E>*>& ds, E id0, E idn, Platform::String^ unit) {
		for (E id = id0; id <= idn; id++) {
			ds[id] = this->master->insert_one(new Credit<Dimensionlet, E>(unit, id.ToString()), id);

			ds[id]->set_style(DimensionState::Normal, this->pump_style);
			ds[id]->set_style(DimensionState::Highlight, this->highlight_style);
		}
	}

	template<typename E>
	void load_label(std::map<E, Credit<Labellet, E>*>& ls, Platform::String^ caption, E id
		, CanvasSolidColorBrush^ color, CanvasTextFormat^ font = nullptr) {
		ls[id] = this->master->insert_one(new Credit<Labellet, E>(caption, font, color), id);
	}

	template<typename E>
	void load_label(std::map<E, Credit<Labellet, E>*>& ls, E id, CanvasSolidColorBrush^ color, CanvasTextFormat^ font = nullptr) {
		this->load_label(ls, _speak(id), id, color, font);
	}

private:
	template<class D, typename E>
	void reflow_doors(std::map<E, Credit<D, E>*>& ds, std::map<E, Credit<Percentagelet, E>*>& ps, E id0, E idn, float yoff) {
		GraphletAnchor d_anchor = GraphletAnchor::CT;
		GraphletAnchor p_anchor = GraphletAnchor::CB;
		float lx, rx, y, cell_width;
		float label_yoff = default_pipe_thickness * 2.0F;
		
		if (yoff > 0.0F) { // Starboard
			d_anchor = GraphletAnchor::CB;
			p_anchor = GraphletAnchor::CT;
		}

		this->station->fill_anchor_location(RS::D001, &lx, &y);
		this->station->fill_anchor_location(RS::D010, &rx, nullptr);
		cell_width = (rx - lx) / float(hopper_count);

		for (E id = id0; id <= idn; id++) {
			size_t idx = static_cast<size_t>(id) - static_cast<size_t>(id0) + 1;
			float x = lx + cell_width * (0.5F + float(hopper_count - idx));
			
			this->master->move_to(ds[id], x, y + yoff, GraphletAnchor::CC);
			this->master->move_to(ps[id], ds[id], d_anchor, p_anchor);
		}
	}

private:
	void set_door_progress(Door id, float value) {
		this->uhdoors[id]->set_value(value / 100.0F);
		this->progresses[id]->set_value(value, GraphletAnchor::CC);

		AI_hopper_door(this->uhdoors[id], value, bottom_door_open_threshold, upper_door_closed_threshold);
	}

	void set_valves_status(RS id
		, const uint8* db4, unsigned int gidx4_p1, unsigned int midx4_p1
		, const uint8* db205, unsigned int gidx205_p1, unsigned int midx205_p1) {
		DI_gate_valve(this->gvalves[id], db4, gidx4_p1, db205, gidx205_p1);

		if (this->mvalves.find(id) != this->mvalves.end()) {
			DI_motor_valve(this->mvalves[id], db4, midx4_p1, db205, midx205_p1);
		}
	}

// never deletes these graphlets mannually
private:
	Tracklet<RS>* station;
	std::map<RS, Credit<Labellet, RS>*> captions;
	std::map<RS, Credit<HopperPumplet, RS>*> hoppers;
	std::map<RS, Credit<GateValvelet, RS>*> gvalves;
	std::map<RS, Credit<MotorValvelet, RS>*> mvalves;
	std::map<RS, Credit<Labellet, RS>*> vlabels;
	std::map<Door, Credit<UpperHopperDoorlet, Door>*> uhdoors;
	std::map<Door, Credit<Percentagelet, Door>*> progresses;
	std::map<RS, Credit<Percentagelet, RS>*> suctions;
	std::map<RS, Credit<Dimensionlet, RS>*> pump_pressures;
	std::map<RS, Credit<Dimensionlet, RS>*> dpressures;
	std::map<RS, Credit<Dimensionlet, RS>*> vpressures;
	std::map<RS, Credit<Dimensionlet, RS>*> powers;
	std::map<RS, Credit<Dimensionlet, RS>*> rpms;
	Labellet* ps_seqs[hopper_count];
	Labellet* sb_seqs[hopper_count];
	std::map<RS, Omegalet*> nintercs;
	Linelet* manual_pipe;
	Hatchlet* sea_inlet;
	Circlelet* ps_suction;
	Circlelet* sb_suction;
	
private:
	CanvasTextFormat^ caption_font;
	CanvasTextFormat^ label_font;
	ICanvasBrush^ relationship_color;
	CanvasStrokeStyle^ relationship_style;
	DimensionStyle pump_style;
	DimensionStyle highlight_style;
	DimensionStyle plain_style;
	DimensionStyle hopper_style;

private:
	DischargesPage* master;
};

private class RainbowsDecorator : public IPlanetDecorator {
public:
	RainbowsDecorator(Rainbows* master) : master(master) {
		float height = 1.0F;
		float xradius = height * 0.10F;
		float yradius = height * 0.50F;

		this->ship_width = 1.0F - xradius;
		this->ship = geometry_union(rectangle(this->ship_width, height),
			segment(this->ship_width, yradius, -90.0, 90.0, xradius, yradius));

		this->ship_style = make_dash_stroke(CanvasDashStyle::Dash);
	}

public:
	void draw_before(CanvasDrawingSession^ ds, float Width, float Height) override {
		this->master->draw_relationships(ds, Width, Height);
	}

	void draw_before_graphlet(IGraphlet* g, CanvasDrawingSession^ ds, float x, float y, float width, float height, bool is_selected) override {
		auto station = dynamic_cast<Tracklet<RS>*>(g);

		if (station != nullptr) {
			float ps_y, sb_y;
			float deck_lx, deck_ty, deck_rx, deck_by;

			station->fill_anchor_location(RS::ps, nullptr, &ps_y, false);
			station->fill_anchor_location(RS::sb, nullptr, &sb_y, false);

			station->fill_anchor_location(RS::deck_lx, &deck_lx, nullptr, false);
			station->fill_anchor_location(RS::deck_rx, &deck_rx, nullptr, false);
			station->fill_anchor_location(RS::deck_ty, nullptr, &deck_ty, false);
			station->fill_anchor_location(RS::deck_by, nullptr, &deck_by, false);

			{ // draw ship
				float ship_width = this->actual_width();
				float ship_height = std::fabsf(sb_y - ps_y);
				auto real_ship = geometry_scale(this->ship, ship_width, ship_height);
				Rect ship_box = real_ship->ComputeBounds();
				float sx = 0.0F;
				float sy = y + std::fminf(sb_y, ps_y);

				ds->DrawGeometry(real_ship, sx, sy, Colours::SeaGreen, 1.0F, this->ship_style);
			}

			{ // draw deck region
				float dx = x + std::fminf(deck_lx, deck_rx);
				float dy = y + std::fminf(deck_ty, deck_by);
				float dw = std::fabsf((deck_rx - deck_lx));
				float dh = std::fabsf((deck_by - deck_ty));

				ds->DrawGeometry(rectangle(dx, dy, dw, dh), Colours::SeaGreen, 1.0F, this->ship_style);
			}


			{ // draw non-important lines
				float d0525_x, d05_y, d25_y;
				float d0325_y, d03_x, d07_x;
				float d10_x, d10_y;

				station->fill_anchor_location(RS::D005, &d0525_x, &d05_y, false);
				station->fill_anchor_location(RS::D025, nullptr, &d25_y, false);
				station->fill_anchor_location(RS::D010, &d10_x, &d10_y, false);
				station->fill_anchor_location(RS::d0325, &d03_x, &d0325_y, false);
				station->fill_anchor_location(RS::d007, &d07_x, nullptr, false);

				ds->DrawLine(x + d0525_x, y + d05_y, x + d0525_x, y + d25_y,
					Colours::DimGray, default_pipe_thickness, this->ship_style);

				ds->DrawLine(x + d03_x, y + d0325_y, x + d07_x, y + d0325_y,
					Colours::DimGray, default_pipe_thickness, this->ship_style);

				ds->DrawLine(d10_x, y + d10_y, x + d10_x, y + d10_y,
					Colours::DimGray, default_pipe_thickness, this->ship_style);
			}
		}
	}

private:
	CanvasGeometry^ ship;
	CanvasStrokeStyle^ ship_style;

private:
	float ship_width;

private:
	Rainbows* master;
};

DischargesPage::DischargesPage(PLCMaster* plc) : Planet(__MODULE__), device(plc) {
	Rainbows* dashboard = new Rainbows(this);

	this->dashboard = dashboard;
	this->gate_valve_op = make_gate_valve_menu(DO_gate_valve_action, plc);
	this->upper_door_op = make_upper_door_menu(plc);
	this->ps_hopper_op = make_ps_hopper_pump_discharge_menu(plc);
	this->sb_hopper_op = make_sb_hopper_pump_discharge_menu(plc);
	this->grid = new GridDecorator();

	this->device->append_confirmation_receiver(dashboard);

	{ // load decorators
		this->append_decorator(new PageDecorator());
		this->append_decorator(new RainbowsDecorator(dashboard));

#ifdef _DEBUG
		this->append_decorator(this->grid);
#else
		this->grid->set_active_planet(this);
#endif
	}
}

DischargesPage::~DischargesPage() {
	if (this->dashboard != nullptr) {
		delete this->dashboard;
	}

#ifndef _DEBUG
	delete this->grid;
#endif
}

void DischargesPage::load(CanvasCreateResourcesReason reason, float width, float height) {
	auto dashboard = dynamic_cast<Rainbows*>(this->dashboard);
	
	if (dashboard != nullptr) {
		float vinset = statusbar_height();
		float gwidth = width / 64.0F;
		float gheight = (height - vinset - vinset) / 36.0F;

		this->grid->set_grid_width(gwidth);
		this->grid->set_grid_height(gheight, vinset);
		
		dashboard->construct(gwidth, gheight);

		{ // load graphlets
			this->change_mode(RSMode::Dashboard);
			dashboard->load(width, height, gwidth, gheight);
			
			this->change_mode(RSMode::WindowUI);
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

void DischargesPage::reflow(float width, float height) {
	auto dashboard = dynamic_cast<Rainbows*>(this->dashboard);
	
	if (dashboard != nullptr) {
		float vinset = statusbar_height();
		float gwidth = this->grid->get_grid_width();
		float gheight = this->grid->get_grid_height();

		this->change_mode(RSMode::WindowUI);
		this->move_to(this->statusline, 0.0F, height, GraphletAnchor::LB);

		this->change_mode(RSMode::Dashboard);
		dashboard->reflow(width, height, gwidth, gheight, vinset);
	}
}

bool DischargesPage::can_select(IGraphlet* g) {
	return ((dynamic_cast<GateValvelet*>(g) != nullptr)
		|| (dynamic_cast<UpperHopperDoorlet*>(g) != nullptr)
		|| (dynamic_cast<HopperPumplet*>(g) != nullptr));
}

void DischargesPage::on_tap_selected(IGraphlet* g, float local_x, float local_y) {
	auto gvalve = dynamic_cast<GateValvelet*>(g);
	auto uhdoor = dynamic_cast<UpperHopperDoorlet*>(g);
	auto hpump = dynamic_cast<Credit<HopperPumplet, RS>*>(g);

	if (gvalve != nullptr) {
		menu_popup(this->gate_valve_op, g, local_x, local_y);
	} else if (uhdoor != nullptr) {
		menu_popup(this->upper_door_op, g, local_x, local_y);
	} else if (hpump != nullptr) {
		switch (hpump->id) {
		case RS::PSHPump: menu_popup(this->ps_hopper_op, g, local_x, local_y); break;
		case RS::SBHPump: menu_popup(this->sb_hopper_op, g, local_x, local_y); break;
		}
	}
}
