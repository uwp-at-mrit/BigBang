#include <map>

#include "page/loadings.hpp"
#include "configuration.hpp"
#include "menu.hpp"

#include "module.hpp"
#include "text.hpp"
#include "paint.hpp"
#include "brushes.hxx"
#include "turtle.hpp"

#include "graphlet/shapelet.hpp"
#include "graphlet/symbol/pump/hopper_pumplet.hpp"
#include "graphlet/symbol/valve/gate_valvelet.hpp"
#include "graphlet/symbol/valve/tagged_valvelet.hpp"

#include "schema/ai_pumps.hpp"
#include "schema/ai_valves.hpp"

#include "schema/di_pumps.hpp"
#include "schema/di_hopper_pumps.hpp"
#include "schema/di_valves.hpp"

#include "schema/do_valves.hpp"
#include "schema/do_hopper_pumps.hpp"

#include "decorator/page.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

private enum LDMode { WindowUI = 0, Dashboard };

private enum class LDGVOperation { Open, Close, VirtualOpen, VirtualClose, _ };
private enum class LDMVOperation { Open, Close, VirtualOpen, VirtualClose, Heat, _ };

private enum class LDPSHPOperation { Prepare, Start, Stop, Reset, PSHopper, BothHopper, _ };
private enum class LDSBHPOperation { Prepare, Start, Stop, Reset, SBHopper, BothHopper, HPBarge, _ };
private enum class LDPSUWPOperation { Prepare, Start, Stop, Reset, PSUnderWater, BothUnderWater, _ };
private enum class LDSBUWPOperation { Prepare, Start, Stop, Reset, SBUnderWater, BothUnderWater, UWPBarge, _ };

// WARNING: order matters
private enum class LD : unsigned int {
	Port, Starboard,
	
	// Valves
	D001, D002, D006, D008, D010, D017, D018, D019, D020, D021, D022, D024, D026,
	D003, D007, D011, D023, D025,
	D004, D005, D009, D012,
	D014, D016,
	D013, D015,

	// Pump Dimensions
	A, C, F, H,
	
	// Key Labels
	PSUWPump, SBUWPump, PSHPump, SBHPump, Gantry, LMOD,
	
	_,
	// anchors used as last jumping points
	d11, d12, d13, d14, d24,
	d0225, d0326, d0406,
	d1720, d1819, d1920, d2122,

	// anchors used for unnamed nodes
	ps, sb, gantry, deck_lx, deck_rx, deck_ty, deck_by,

	// anchors used for non-interconnected nodes
	n0325, n0405, n0723, n0923
};

private class Vessel final
	: public PLCConfirmation
	, public IMenuCommand<LDGVOperation, Credit<GateValvelet, LD>, PLCMaster*>
	, public IMenuCommand<LDMVOperation, Credit<MotorValvelet, LD>, PLCMaster*>
	, public IMenuCommand<LDPSHPOperation, Credit<HopperPumplet, LD>, PLCMaster*>
	, public IMenuCommand<LDSBHPOperation, Credit<HopperPumplet, LD>, PLCMaster*>
	, public IMenuCommand<LDPSUWPOperation, Credit<HopperPumplet, LD>, PLCMaster*>
	, public IMenuCommand<LDSBUWPOperation, Credit<HopperPumplet, LD>, PLCMaster*> {
public:
	Vessel(LoadingsPage* master) : master(master) {}

public:
	void pre_read_data(Syslog* logger) override {
		this->master->enter_critical_section();
		this->master->begin_update_sequence();
	}

	void on_realtime_data(const uint8* DB2, size_t count, Syslog* logger) override {
		this->powers[LD::PSHPump]->set_value(DBD(DB2, 12U));
		this->powers[LD::SBHPump]->set_value(DBD(DB2, 16U));
		this->powers[LD::PSUWPump]->set_value(DBD(DB2, 200U));
		this->powers[LD::SBUWPump]->set_value(DBD(DB2, 204U));

		this->rpms[LD::PSHPump]->set_value(DBD(DB2, 604U));
		this->rpms[LD::SBHPump]->set_value(DBD(DB2, 608U));
		this->rpms[LD::PSUWPump]->set_value(DBD(DB2, 200U));
		this->rpms[LD::SBUWPump]->set_value(DBD(DB2, 204U));
	}

	void on_analog_input(const uint8* DB203, size_t count, Syslog* logger) override {
		this->pressures[LD::C]->set_value(RealData(DB203, pump_C_pressure), GraphletAnchor::LB);
		this->pressures[LD::F]->set_value(RealData(DB203, pump_F_pressure), GraphletAnchor::LT);

		this->pressures[LD::A]->set_value(RealData(DB203, pump_A_pressure), GraphletAnchor::LB);
		this->pressures[LD::H]->set_value(RealData(DB203, pump_H_pressure), GraphletAnchor::LT);

		this->progresses[LD::D003]->set_value(RealData(DB203, gate_valve_D03_progress), GraphletAnchor::LB);
		this->progresses[LD::D004]->set_value(RealData(DB203, gate_valve_D04_progress), GraphletAnchor::LT);
	}

	void on_digital_input(const uint8* DB4, size_t count4, const uint8* DB205, size_t count205, WarGrey::SCADA::Syslog* logger) override {
		DI_hopper_pumps(this->hoppers[LD::PSHPump], this->hoppers[LD::PSUWPump], DB4, 1U, DB205, 857U, 825U);
		DI_hopper_pumps(this->hoppers[LD::SBHPump], this->hoppers[LD::SBUWPump], DB4, 25U, DB205, 873U, 841U);

		DI_pump_dimension(this->pressures[LD::A], DB4, pump_A_feedback);
		DI_pump_dimension(this->pressures[LD::C], DB4, pump_C_feedback);
		DI_pump_dimension(this->pressures[LD::F], DB4, pump_F_feedback);
		DI_pump_dimension(this->pressures[LD::H], DB4, pump_H_feedback);

		this->set_valves_status(LD::D001, DB4, gate_valve_D01_feedback, motor_valve_D01_feedback, DB205, gate_valve_D01_status, motor_valve_D01_status);
		this->set_valves_status(LD::D002, DB4, gate_valve_D02_feedback, motor_valve_D02_feedback, DB205, gate_valve_D02_status, motor_valve_D02_status);
		this->set_valves_status(LD::D005, DB4, gate_valve_D05_feedback, motor_valve_D05_feedback, DB205, gate_valve_D05_status, motor_valve_D05_status);
		this->set_valves_status(LD::D006, DB4, gate_valve_D06_feedback, motor_valve_D06_feedback, DB205, gate_valve_D06_status, motor_valve_D06_status);
		this->set_valves_status(LD::D007, DB4, gate_valve_D07_feedback, motor_valve_D07_feedback, DB205, gate_valve_D07_status, motor_valve_D07_status);
		this->set_valves_status(LD::D008, DB4, gate_valve_D08_feedback, motor_valve_D08_feedback, DB205, gate_valve_D08_status, motor_valve_D08_status);
		this->set_valves_status(LD::D009, DB4, gate_valve_D09_feedback, motor_valve_D09_feedback, DB205, gate_valve_D09_status, motor_valve_D09_status);
		this->set_valves_status(LD::D010, DB4, gate_valve_D10_feedback, motor_valve_D10_feedback, DB205, gate_valve_D10_status, motor_valve_D10_status);
		this->set_valves_status(LD::D011, DB4, gate_valve_D11_feedback, motor_valve_D11_feedback, DB205, gate_valve_D11_status, motor_valve_D11_status);
		this->set_valves_status(LD::D012, DB4, gate_valve_D12_feedback, motor_valve_D12_feedback, DB205, gate_valve_D12_status, motor_valve_D12_status);
		this->set_valves_status(LD::D013, DB4, gate_valve_D13_feedback, motor_valve_D13_feedback, DB205, gate_valve_D13_status, motor_valve_D13_status);
		this->set_valves_status(LD::D014, DB4, gate_valve_D14_feedback, motor_valve_D14_feedback, DB205, gate_valve_D14_status, motor_valve_D14_status);
		this->set_valves_status(LD::D015, DB4, gate_valve_D15_feedback, motor_valve_D15_feedback, DB205, gate_valve_D15_status, motor_valve_D15_status);
		this->set_valves_status(LD::D016, DB4, gate_valve_D16_feedback, motor_valve_D16_feedback, DB205, gate_valve_D16_status, motor_valve_D16_status);
		this->set_valves_status(LD::D017, DB4, gate_valve_D17_feedback, motor_valve_D17_feedback, DB205, gate_valve_D17_status, motor_valve_D17_status);
		this->set_valves_status(LD::D018, DB4, gate_valve_D18_feedback, motor_valve_D18_feedback, DB205, gate_valve_D18_status, motor_valve_D18_status);
		this->set_valves_status(LD::D019, DB4, gate_valve_D19_feedback, motor_valve_D19_feedback, DB205, gate_valve_D19_status, motor_valve_D19_status);
		this->set_valves_status(LD::D020, DB4, gate_valve_D20_feedback, motor_valve_D20_feedback, DB205, gate_valve_D20_status, motor_valve_D20_status);
		this->set_valves_status(LD::D021, DB4, gate_valve_D21_feedback, motor_valve_D21_feedback, DB205, gate_valve_D21_status, motor_valve_D21_status);
		this->set_valves_status(LD::D022, DB4, gate_valve_D22_feedback, motor_valve_D22_feedback, DB205, gate_valve_D22_status, motor_valve_D22_status);
		this->set_valves_status(LD::D023, DB4, gate_valve_D23_feedback, motor_valve_D23_feedback, DB205, gate_valve_D23_status, motor_valve_D23_status);
		this->set_valves_status(LD::D024, DB4, gate_valve_D24_feedback, motor_valve_D24_feedback, DB205, gate_valve_D24_status, motor_valve_D24_status);
		this->set_valves_status(LD::D025, DB4, gate_valve_D25_feedback, motor_valve_D25_feedback, DB205, gate_valve_D25_status, motor_valve_D25_status);
		this->set_valves_status(LD::D026, DB4, gate_valve_D26_feedback, motor_valve_D26_feedback, DB205, gate_valve_D26_status, motor_valve_D26_status);
	
		DI_gate_valve(this->gvalves[LD::D003], DB205, gate_valve_D03_feedback, DB205, gate_valve_D03_status);
		DI_motor_valve(this->mvalves[LD::D003], DB4, motor_valve_D03_feedback, DB205, motor_valve_D03_status);

		DI_gate_valve(this->gvalves[LD::D004], DB205, gate_valve_D04_feedback, DB205, gate_valve_D04_status);
		DI_motor_valve(this->mvalves[LD::D004], DB4, motor_valve_D04_feedback, DB205, motor_valve_D04_status);
	}

	void post_read_data(Syslog* logger) override {
		this->master->end_update_sequence();
		this->master->leave_critical_section();
	}

public:
	bool can_execute(LDGVOperation cmd, Credit<GateValvelet, LD>* valve, PLCMaster* plc, bool acc_executable) override {
		return gate_valve_command_executable(valve, cmd, true) && plc->connected();
	}
	
	void execute(LDGVOperation cmd, Credit<GateValvelet, LD>* valve, PLCMaster* plc) override {
		plc->send_command(DO_gate_valve_command(cmd, valve->id));
	}

	bool can_execute(LDMVOperation cmd, Credit<MotorValvelet, LD>* valve, PLCMaster* plc, bool acc_executable) override {
		return motor_valve_command_executable(valve, cmd, true) && plc->connected();
	}

	void execute(LDMVOperation cmd, Credit<MotorValvelet, LD>* valve, PLCMaster* plc) override {
		plc->send_command(DO_motor_valve_command(cmd, valve->id));
	}

public:
	bool can_execute(LDPSHPOperation cmd, Credit<HopperPumplet, LD>* valve, PLCMaster* plc, bool acc_executable) override {
		return plc->connected();
	}

	void execute(LDPSHPOperation cmd, Credit<HopperPumplet, LD>* hopper, PLCMaster* plc) override {
		plc->send_command(DO_ps_hopper_pump_charge_command(cmd));
	}

	bool can_execute(LDSBHPOperation cmd, Credit<HopperPumplet, LD>* hopper, PLCMaster* plc, bool acc_executable) override {
		return plc->connected();
	}

	void execute(LDSBHPOperation cmd, Credit<HopperPumplet, LD>* hopper, PLCMaster* plc) override {
		plc->send_command(DO_sb_hopper_pump_charge_command(cmd));
	}

public:
	bool can_execute(LDPSUWPOperation cmd, Credit<HopperPumplet, LD>* hopper, PLCMaster* plc, bool acc_executable) override {
		return plc->connected();
	}

	void execute(LDPSUWPOperation cmd, Credit<HopperPumplet, LD>* hopper, PLCMaster* plc) override {
		plc->send_command(DO_ps_underwater_pump_command(cmd));
	}

	bool can_execute(LDSBUWPOperation cmd, Credit<HopperPumplet, LD>* hopper, PLCMaster* plc, bool acc_executable) override {
		return plc->connected();
	}

	void execute(LDSBUWPOperation cmd, Credit<HopperPumplet, LD>* hopper, PLCMaster* plc) override {
		plc->send_command(DO_sb_underwater_pump_command(cmd));
	}

public:
	void construct(float gwidth, float gheight) {
		this->caption_font = make_bold_text_format("Microsoft YaHei", normal_font_size);
		this->label_font = make_bold_text_format("Microsoft YaHei", small_font_size);
		this->special_font = make_text_format(tiny_font_size);
		this->pump_style = make_highlight_dimension_style(large_metrics_font_size, 6U, Colours::Background);
		this->highlight_style = make_highlight_dimension_style(large_metrics_font_size, 6U, Colours::Green);
		this->relationship_style = make_dash_stroke(CanvasDashStyle::DashDot);
		this->relationship_color = Colours::DarkGray;

		this->hopper_style.number_font = make_bold_text_format("Cambria Math", large_metrics_font_size);
		this->hopper_style.unit_font = make_bold_text_format("Cambria", normal_font_size);
	}
 
public:
	void load(float width, float height, float gwidth, float gheight) {
		float radius = resolve_gridsize(gwidth, gheight);
		Turtle<LD>* pTurtle = new Turtle<LD>(gwidth, gheight, false);

		pTurtle->move_left(LD::deck_rx)->move_left(2, LD::D021)->move_left(2, LD::d2122);
		pTurtle->move_down(5)->move_right(2, LD::D022)->move_right(3)->jump_back();
		pTurtle->move_left(2, LD::d1920)->move_left(2, LD::D020)->move_left(7, LD::d1720);

		pTurtle->move_left(3, LD::D017)->move_left(11, LD::n0405)->move_left(4, LD::D010)->move_left(6, LD::d12);
		pTurtle->move_down(2, LD::D012)->move_down(3)->jump_down(LD::LMOD)->jump_back(LD::d12);
		pTurtle->move_left(4, LD::d14)->move_left_down(2, LD::D014)->move_left_down(1.5F)->jump_back();
		pTurtle->move_left(5, LD::d24)->move_left(4)->move_left_down(2, LD::D016)->move_left_down(1.5F)->jump_back();
		pTurtle->jump_up(2.5F, LD::gantry)->turn_up_left()->move_left(3, LD::D024)->move_left(3)->turn_left_up();
		pTurtle->move_up(0.5F, LD::Gantry)->move_left()->jump_back(LD::Gantry)->move_right()->jump_back(LD::d1720);
		
		pTurtle->move_down(3.5F, LD::PSHPump)->move_left(6, LD::n0923)->move_left(8)->move_up(1.5F, LD::D005)->move_up(1.5F)->jump_up();
		pTurtle->move_up(3, LD::d0406)->move_right(4, LD::D006)->move_right(4)->move_down(0.5F, LD::deck_ty)->move_down(LD::D009);
		pTurtle->move_down(5)->jump_down()->move_down(2, LD::D023)->jump_back(LD::d0406);

		pTurtle->move_up(1.5F, LD::D004)->move_up(2, LD::ps)->move_up(2, LD::C)->turn_up_left();
		pTurtle->move_left(10, LD::PSUWPump)->move_left(8, LD::A)->move_left(LD::Port);

		pTurtle->jump_back(LD::D023)->move_down(2)->jump_down()->move_down(5, LD::D007);
		pTurtle->move_down(LD::deck_by)->move_down(0.5F)->move_left(4, LD::D026)->move_left(4, LD::d0326);
		pTurtle->move_up(3)->jump_up()->move_up(1.5F, LD::D025)->move_up(1.5F, LD::d0225);
		pTurtle->move_right(8, LD::n0723)->move_right(6, LD::SBHPump)->move_down(3.5F, LD::d1819)->jump_back(LD::d0225);
		pTurtle->move_up(2.5F)->move_left(2, LD::D002)->move_left(28, LD::D001)->move_left(2)->jump_back(LD::d1819);

		pTurtle->move_left(3, LD::D018)->move_left(11, LD::n0325)->move_left(4, LD::D008);
		pTurtle->move_left(6, LD::d11)->move_up(2, LD::D011)->move_up(3 /* LD::LMOD */)->jump_back();
		pTurtle->move_left(4, LD::d13)->move_left_up(2, LD::D013)->move_left_up(1.5F)->jump_back();
		pTurtle->move_left(9)->move_left_up(2, LD::D015)->move_left_up(1.5F);

		pTurtle->jump_back(LD::d0326)->move_down(1.5F, LD::D003)->move_down(2, LD::sb)->move_down(2, LD::F)->turn_down_left();
		pTurtle->move_left(10, LD::SBUWPump)->move_left(8, LD::H)->move_left(LD::Starboard);

		pTurtle->jump_back(LD::d1819)->move_right(5, LD::deck_lx)->move_right(2, LD::D019)->move_right(2)->move_to(LD::d1920);
		
		this->station = this->master->insert_one(new Tracklet<LD>(pTurtle, default_pipe_thickness, default_pipe_color));

		{ // load valves
			this->load_valve(this->gvalves, this->vlabels, this->captions, LD::D001, radius, 0.0);
			this->load_valves(this->gvalves, this->mvalves, this->vlabels, this->captions, LD::D002, LD::D026, radius, 00.0);
			this->load_valves(this->gvalves, this->mvalves, this->vlabels, this->captions, LD::D003, LD::D025, radius, 90.0);
			this->load_valves(this->gvalves, this->mvalves, this->vlabels, this->captions, LD::D004, LD::D012, radius, -90.0);
			this->load_valves(this->gvalves, this->mvalves, this->vlabels, this->captions, LD::D014, LD::D016, radius, -45.0);
			this->load_valves(this->gvalves, this->mvalves, this->vlabels, this->captions, LD::D013, LD::D015, radius, 45.0);
		}

		{ // load special nodes
			float nic_radius = gheight * 0.25F;

			this->load_pump(this->hoppers, this->captions, LD::PSUWPump, -radius, -2.0F);
			this->load_pump(this->hoppers, this->captions, LD::SBUWPump, -radius, +2.0F);
			this->load_pump(this->hoppers, this->captions, LD::PSHPump, -radius, +2.0F);
			this->load_pump(this->hoppers, this->captions, LD::SBHPump, -radius, -2.0F);

			this->LMOD = this->master->insert_one(new Arclet(0.0, 360.0, gheight, gheight, 0.5F, default_pipe_color));

			this->ps_draghead = this->master->insert_one(
				new Segmentlet(-90.0, 90.0, gwidth * 2.0F, gheight,
					default_ps_color, default_pipe_thickness));

			this->sb_draghead = this->master->insert_one(
				new Segmentlet(-90.0, 90.0, gwidth * 2.0F, gheight,
					default_sb_color, default_pipe_thickness));

			for (LD id = LD::n0325; id <= LD::n0923; id++) {
				this->nintercs[id] = this->master->insert_one(
					new Omegalet(-90.0, nic_radius, default_pipe_thickness, default_pipe_color));
			}
		}

		{ // load labels and dimensions
			this->load_percentage(this->progresses, LD::D003);
			this->load_percentage(this->progresses, LD::D004);
			this->load_dimensions(this->pressures, LD::A, LD::H, "bar");

			this->load_label(this->captions, LD::Gantry, Colours::Yellow, this->caption_font);
			this->load_label(this->captions, LD::LMOD, Colours::Yellow, this->special_font);
		}
	}

public:
	void reflow(float width, float height, float gwidth, float gheight, float vinset) {
		GraphletAnchor anchor;
		float dx, dy, margin, label_height, ox, oy;
		float gridsize = std::fminf(gwidth, gheight);
		float x0 = 0.0F;
		float y0 = 0.0F;

		this->master->move_to(this->station, width * 0.5F, height * 0.5F, GraphletAnchor::CC);

		this->station->map_credit_graphlet(this->captions[LD::Gantry], GraphletAnchor::CB);
		this->station->map_credit_graphlet(this->captions[LD::LMOD], GraphletAnchor::CB);

		this->station->map_graphlet_at_anchor(this->ps_draghead, LD::Port, GraphletAnchor::RC);
		this->station->map_graphlet_at_anchor(this->sb_draghead, LD::Starboard, GraphletAnchor::RC);
		this->station->map_graphlet_at_anchor(this->LMOD, LD::LMOD, GraphletAnchor::CC);

		for (auto it = this->nintercs.begin(); it != this->nintercs.end(); it++) {
			/** NOTE
			 * Lines are brush-based shape, they do not have stroke, `Shapelet` does not know how width they are,
			 * thus, we have to do aligning on our own.
			 */
			this->station->map_graphlet_at_anchor(it->second, it->first, GraphletAnchor::LC, -default_pipe_thickness * 0.5F);
		}

		for (auto it = this->hoppers.begin(); it != this->hoppers.end(); it++) {
			it->second->fill_pump_origin(&ox);
			this->station->map_credit_graphlet(it->second, GraphletAnchor::CC, -ox);

			if (it->first == LD::SBUWPump) {
				this->master->move_to(this->captions[it->first], it->second, GraphletAnchor::RC, GraphletAnchor::LT, std::fabsf(ox));
				this->master->move_to(this->rpms[it->first], this->captions[it->first], GraphletAnchor::LT, GraphletAnchor::LB);
				this->master->move_to(this->powers[it->first], this->rpms[it->first], GraphletAnchor::LT, GraphletAnchor::LB);
			} else {
				this->master->move_to(this->captions[it->first], it->second, GraphletAnchor::RC, GraphletAnchor::LB, std::fabsf(ox));
				this->master->move_to(this->powers[it->first], this->captions[it->first], GraphletAnchor::LB, GraphletAnchor::LT);
				this->master->move_to(this->rpms[it->first], this->powers[it->first], GraphletAnchor::LB, GraphletAnchor::LT);
			}
		}

		this->vlabels[LD::D001]->fill_extent(0.0F, 0.0F, nullptr, &label_height);
		
		for (auto it = this->gvalves.begin(); it != this->gvalves.end(); it++) {
			switch (it->first) {
			case LD::D014: case LD::D016: {
				dx = x0 + gwidth; dy = y0; anchor = GraphletAnchor::LB;
			}; break;
			case LD::D013: case LD::D015: {
				dx = x0 + gwidth; dy = y0; anchor = GraphletAnchor::LB;
			}; break;
			case LD::D006: case LD::D010: case LD::D020: case LD::D021: case LD::D022: case LD::D024: {
				it->second->fill_margin(x0, y0, nullptr, nullptr, &margin, nullptr);
				dx = x0; dy = y0 + gridsize - margin; anchor = GraphletAnchor::CT;
			}; break;
			case LD::D017: {
				dx = x0 + gwidth; dy = y0 - label_height; anchor = GraphletAnchor::LB;
			}; break;
			case LD::D018: {
				dx = x0 + gwidth; dy = y0; anchor = GraphletAnchor::LT;
			}; break;
			case LD::D001: case LD::D002: case LD::D008: case LD::D019: case LD::D026: {
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

		{ // reflow motor-driven valves
			float polar45 = gridsize * std::sqrtf(2.0F) * 0.618F;

			for (auto it = this->mvalves.begin(); it != this->mvalves.end(); it++) {
				switch (it->first) {
				case LD::D014: case LD::D016: {
					dx = x0 - polar45; dy = y0 - polar45; anchor = GraphletAnchor::CC;
				}; break;
				case LD::D013: case LD::D015: {
					dx = x0 - polar45; dy = y0 + polar45; anchor = GraphletAnchor::CC;
				}; break;
				case LD::D003: case LD::D004: case LD::D005: case LD::D007: case LD::D009:
				case LD::D011: case LD::D012: case LD::D023: case LD::D025: {
					this->gvalves[LD::D003]->fill_margin(x0, y0, nullptr, nullptr, nullptr, &margin);
					dx = x0 - gridsize + margin; dy = y0; anchor = GraphletAnchor::RC;
				}; break;
				case LD::D002: case LD::D008: case LD::D017: case LD::D019: case LD::D026: {
					dx = x0; dy = y0 + gridsize; anchor = GraphletAnchor::CC;
				}; break;
				default: {
					dx = x0; dy = y0 - gridsize; anchor = GraphletAnchor::CC;
				}
				}

				it->second->fill_valve_origin(&ox, &oy);
				this->station->map_credit_graphlet(it->second, anchor, dx - ox, dy - oy);
			}
		}

		{ // reflow dimensions
			float offset = default_pipe_thickness * 2.0F;

			this->master->move_to(this->progresses[LD::D003], this->gvalves[LD::D003], GraphletAnchor::CB, GraphletAnchor::LT, offset, -offset);
			this->master->move_to(this->progresses[LD::D004], this->gvalves[LD::D004], GraphletAnchor::CT, GraphletAnchor::LB, offset);

			this->station->map_credit_graphlet(this->pressures[LD::A], GraphletAnchor::LB, 0.0F, -offset);
			this->station->map_credit_graphlet(this->pressures[LD::C], GraphletAnchor::LB, gwidth);
			this->station->map_credit_graphlet(this->pressures[LD::F], GraphletAnchor::LT, gwidth);
			this->station->map_credit_graphlet(this->pressures[LD::H], GraphletAnchor::LT, 0.0F, +offset);
		}
	}

public:
	void draw_valves_relationships(CanvasDrawingSession^ ds, float Width, float Height) {
		float ox, oy, gx, gy, mx, my;

		for (auto it = this->mvalves.begin(); it != this->mvalves.end(); it++) {
			this->master->fill_graphlet_location(it->second, &mx, &my, GraphletAnchor::CC);
			this->master->fill_graphlet_location(this->gvalves[it->first], &gx, &gy, GraphletAnchor::CC);
			it->second->fill_valve_origin(&ox, &oy);

			ds->DrawLine(mx + ox, my + oy, gx, gy, this->relationship_color, 1.0F, this->relationship_style);
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
			case LD::D014: case LD::D016: mdegrees = 45.0; break;
			case LD::D013: case LD::D015: mdegrees = -45.0; break;
			case LD::D002: case LD::D008: case LD::D009: case LD::D017: case LD::D019: case LD::D026: mdegrees = -180.0; break;
			}

			// moter-driven valves' second, catching events first 
			this->load_valve(gs, ls, cs, id, radius, degrees);
			ms[id] = this->master->insert_one(new M(mradius, mdegrees, false), id);
		}
	}

	template<class G, typename E>
	void load_pump(std::map<E, G*>& gs, std::map<E, Credit<Labellet, E>*>& ls, E id, float rx, float fy) {
		this->load_label(ls, id, Colours::Salmon, this->caption_font);

		gs[id] = this->master->insert_one(new G(rx, std::fabsf(rx) * fy), id);
		this->powers[id] = this->master->insert_one(new Credit<Dimensionlet, E>(this->hopper_style, "kwatt"), id);
		this->rpms[id] = this->master->insert_one(new Credit<Dimensionlet, E>(this->hopper_style, "rpm"), id);
	}

	template<typename E>
	void load_percentage(std::map<E, Credit<Percentagelet, E>*>& ps, E id) {
		ps[id] = this->master->insert_one(new Credit<Percentagelet, E>(this->plain_style), id);
	}

	template<typename E>
	void load_dimensions(std::map<E, Credit<Dimensionlet, E>*>& ds, E id0, E idn, Platform::String^ unit) {
		for (E id = id0; id <= idn; id++) {
			ds[id] = this->master->insert_one(new Credit<Dimensionlet, E>(unit, id.ToString()), id);

			ds[id]->set_style(DimensionStatus::Normal, this->pump_style);
			ds[id]->set_style(DimensionStatus::Highlight, this->highlight_style);
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
	void set_valves_status(LD id
		, const uint8* db4, unsigned int gidx4_p1, unsigned int midx4_p1
		, const uint8* db205, unsigned int gidx205_p1, unsigned int midx205_p1) {
		DI_gate_valve(this->gvalves[id], db4, gidx4_p1, db205, gidx205_p1);

		if (this->mvalves.find(id) != this->mvalves.end()) {
			DI_motor_valve(this->mvalves[id], db4, midx4_p1, db205, midx205_p1);
		}
	}

// never deletes these graphlets mannually
private:
	Tracklet<LD>* station;
	std::map<LD, Credit<Labellet, LD>*> captions;
	std::map<LD, Credit<HopperPumplet, LD>*> hoppers;
	std::map<LD, Credit<GateValvelet, LD>*> gvalves;
	std::map<LD, Credit<MotorValvelet, LD>*> mvalves;
	std::map<LD, Credit<Labellet, LD>*> vlabels;
	std::map<LD, Credit<Percentagelet, LD>*> progresses;
	std::map<LD, Credit<Dimensionlet, LD>*> pressures;
	std::map<LD, Credit<Dimensionlet, LD>*> powers;
	std::map<LD, Credit<Dimensionlet, LD>*> rpms;
	std::map<LD, Omegalet*> nintercs;
	Segmentlet* ps_draghead;
	Segmentlet* sb_draghead;
	Arclet* LMOD;
	
private:
	CanvasTextFormat^ caption_font;
	CanvasTextFormat^ label_font;
	CanvasTextFormat^ special_font;
	ICanvasBrush^ relationship_color;
	CanvasStrokeStyle^ relationship_style;
	DimensionStyle pump_style;
	DimensionStyle highlight_style;
	DimensionStyle plain_style;
	DimensionStyle hopper_style;

private:
	LoadingsPage* master;
};

private class ShipDecorator : public IPlanetDecorator {
public:
	ShipDecorator(Vessel* master) : master(master) {
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
		this->master->draw_valves_relationships(ds, Width, Height);
	}

	void draw_before_graphlet(IGraphlet* g, CanvasDrawingSession^ ds, float x, float y, float width, float height, bool is_selected) override {
		auto station = dynamic_cast<Tracklet<LD>*>(g);

		if (station != nullptr) {
			float ps_y, sb_y;
			float deck_lx, deck_ty, deck_rx, deck_by;
			
			station->fill_anchor_location(LD::ps, nullptr, &ps_y, false);
			station->fill_anchor_location(LD::sb, nullptr, &sb_y, false);

			station->fill_anchor_location(LD::deck_lx, &deck_lx, nullptr, false);
			station->fill_anchor_location(LD::deck_rx, &deck_rx, nullptr, false);
			station->fill_anchor_location(LD::deck_ty, nullptr, &deck_ty, false);
			station->fill_anchor_location(LD::deck_by, nullptr, &deck_by, false);

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
				float gantry_x, gantry_y, barge_y;
				float d0525_x, d05_y, d25_y;

				station->fill_anchor_location(LD::gantry, &gantry_x, &gantry_y, false);
				station->fill_anchor_location(LD::D008, nullptr, &barge_y, false);
				station->fill_anchor_location(LD::D005, &d0525_x, &d05_y, false);
				station->fill_anchor_location(LD::D025, nullptr, &d25_y, false);

				ds->DrawLine(x + gantry_x, y + gantry_y, x + gantry_x, y + barge_y,
					Colours::DimGray, default_pipe_thickness, this->ship_style);

				ds->DrawLine(x + d0525_x, y + d05_y, x + d0525_x, y + d25_y,
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
	Vessel* master;
};

LoadingsPage::LoadingsPage(PLCMaster* plc) : Planet(__MODULE__), device(plc) {
	Vessel* dashboard = new Vessel(this);

	this->dashboard = dashboard;
	this->gate_valve_op = make_menu<LDGVOperation, Credit<GateValvelet, LD>, PLCMaster*>(dashboard, plc);
	this->motor_valve_op = make_menu<LDMVOperation, Credit<MotorValvelet, LD>, PLCMaster*>(dashboard, plc);
	this->ps_hopper_op = make_menu<LDPSHPOperation, Credit<HopperPumplet, LD>, PLCMaster*>(dashboard, plc);
	this->sb_hopper_op = make_menu<LDSBHPOperation, Credit<HopperPumplet, LD>, PLCMaster*>(dashboard, plc);
	this->ps_underwater_op = make_menu<LDPSUWPOperation, Credit<HopperPumplet, LD>, PLCMaster*>(dashboard, plc);
	this->sb_underwater_op = make_menu<LDSBUWPOperation, Credit<HopperPumplet, LD>, PLCMaster*>(dashboard, plc);
	this->grid = new GridDecorator();

	this->device->append_confirmation_receiver(dashboard);

	{ // load decorators
		this->append_decorator(new PageDecorator());
		this->append_decorator(new ShipDecorator(dashboard));

#ifdef _DEBUG
		this->append_decorator(this->grid);
#else
		this->grid->set_active_planet(this);
#endif
	}
}

LoadingsPage::~LoadingsPage() {
	if (this->dashboard != nullptr) {
		delete this->dashboard;
	}

#ifndef _DEBUG
	delete this->grid;
#endif
}

void LoadingsPage::load(CanvasCreateResourcesReason reason, float width, float height) {
	auto dashboard = dynamic_cast<Vessel*>(this->dashboard);
	
	if (dashboard != nullptr) {
		float vinset = statusbar_height();
		float gwidth = width / 64.0F;
		float gheight = (height - vinset - vinset) / 36.0F;

		this->grid->set_grid_width(gwidth);
		this->grid->set_grid_height(gheight, vinset);
		
		dashboard->construct(gwidth, gheight);

		{ // load graphlets
			this->change_mode(LDMode::Dashboard);
			dashboard->load(width, height, gwidth, gheight);
			
			this->change_mode(LDMode::WindowUI);
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

void LoadingsPage::reflow(float width, float height) {
	auto dashboard = dynamic_cast<Vessel*>(this->dashboard);
	
	if (dashboard != nullptr) {
		float vinset = statusbar_height();
		float gwidth = this->grid->get_grid_width();
		float gheight = this->grid->get_grid_height();

		this->change_mode(LDMode::WindowUI);
		this->move_to(this->statusline, 0.0F, height, GraphletAnchor::LB);

		this->change_mode(LDMode::Dashboard);
		dashboard->reflow(width, height, gwidth, gheight, vinset);
	}
}

bool LoadingsPage::can_select(IGraphlet* g) {
	return ((dynamic_cast<GateValvelet*>(g) != nullptr)
		|| (dynamic_cast<MotorValvelet*>(g) != nullptr)
		|| (dynamic_cast<HopperPumplet*>(g) != nullptr));
}

void LoadingsPage::on_tap_selected(IGraphlet* g, float local_x, float local_y) {
	auto gvalve = dynamic_cast<GateValvelet*>(g);
	auto mvalve = dynamic_cast<MotorValvelet*>(g);
	auto hpump = dynamic_cast<Credit<HopperPumplet, LD>*>(g);

	if (gvalve != nullptr) {
		menu_popup(this->gate_valve_op, g, local_x, local_y);
	} else if (mvalve != nullptr) {
		menu_popup(this->motor_valve_op, g, local_x, local_y);
	} else if (hpump != nullptr) {
		switch (hpump->id) {
		case LD::PSHPump: menu_popup(this->ps_hopper_op, g, local_x, local_y); break;
		case LD::SBHPump: menu_popup(this->sb_hopper_op, g, local_x, local_y); break;
		case LD::PSUWPump: menu_popup(this->ps_underwater_op, g, local_x, local_y); break;
		case LD::SBUWPump: menu_popup(this->sb_underwater_op, g, local_x, local_y); break;
		}
	}
}
