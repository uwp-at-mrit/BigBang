#include <map>

#include "page/dredges.hpp"
#include "configuration.hpp"
#include "menu.hpp"

#include "satellite.hpp"

#include "graphlet/shapelet.hpp"

#include "graphlet/dashboard/cylinderlet.hpp"
#include "graphlet/dashboard/densityflowmeterlet.hpp"
#include "graphlet/symbol/valve/gate_valvelet.hpp"
#include "graphlet/symbol/pump/hopper_pumplet.hpp"
#include "graphlet/device/compensatorlet.hpp"
#include "graphlet/device/gantrylet.hpp"
#include "graphlet/device/overflowlet.hpp"
#include "graphlet/device/winchlet.hpp"
#include "graphlet/device/draglet.hpp"

#include "schema/ai_metrics.hpp"
#include "schema/ai_dredges.hpp"
#include "schema/ai_pumps.hpp"
#include "schema/ai_valves.hpp"

#include "schema/di_valves.hpp"
#include "schema/di_pumps.hpp"
#include "schema/di_hopper_pumps.hpp"
#include "schema/di_dredges.hpp"

#include "schema/do_dredges.hpp"

#include "decorator/page.hpp"

#include "module.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::System;
using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Windows::UI::Xaml::Controls;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

private enum DSMode { WindowUI = 0, Dashboard };

private enum class DSGroup { PSGantries, SBGantries };

private enum class DSWOperation { Up, Down, Stop, HighSpeed, _ };
private enum class DSGOperation { WindOut, WindUp, Stop, _ };
private enum class DSWCOperation { Charge, Discharge, Stop, _ };
private enum class DSDVOperation { Up, Down, Stop, _ };

// WARNING: order matters
private enum class DS : unsigned int {
	// pipeline
	D003, D004, D011, D012, D013, D014, D015, D016,
	LMOD, PS, SB, PSHP, SBHP,
	
	_,

	// unnamed nodes
	ps, sb, d13, d14,

	// Pump Dimensions
	C, B, A, F, G, H,

	// labels
	Overlook, Sidelook, pump, hopper, underwater,

	// dimensions
	Overflow, PSWC, SBWC, PSDP, SBDP, PSPF1, PSPF2, SBPF1, SBPF2, TideMark, Speed,

	// hopper pump metrics
	PSHPDP, SBHPDP, PSHPVP, SBHPVP,

	// winches and gantries
	psTrunnion, psIntermediate, psDragHead,
	sbTrunnion, sbIntermediate, sbDragHead,

	// winch statuses
	SuctionLimited, SaddleLimited, Slack, Upper, SoftUpper,

	// instructions
	ps_gantry_settings, sb_gantry_settings, on,
	tVirtualUp, tVirtualOut,
	iVirtualUp, iVirtualOut,
	hVirtualUp, hVirtualOut
};

static ICanvasBrush^ checked_color = Colours::Green;
static ICanvasBrush^ unchecked_color = Colours::WhiteSmoke;

static ICanvasBrush^ winch_status_color = Colours::DimGray;
static ICanvasBrush^ winch_status_highlight_color = Colours::Green;

private class IDredgingSystem : public PLCConfirmation {
public:
	IDredgingSystem(DredgesPage* master) : master(master) {
		this->label_font = make_bold_text_format("Microsoft YaHei", normal_font_size);
		this->station_font = make_bold_text_format("Microsoft YaHei", tiny_font_size);
		this->caption_font = make_bold_text_format("Microsoft YaHei", large_font_size);
		this->caption_color = Colours::Salmon;

		//this->plain_style.number_font = make_bold_text_format("Cambria Math", large_metrics_font_size);
		//this->plain_style.unit_font = make_bold_text_format("Cambria", normal_font_size);

		this->drag_configs[0].trunnion_gapsize = ps_drag_trunnion_gapsize;
		this->drag_configs[0].trunnion_length = ps_drag_trunnion_length;
		this->drag_configs[0].pipe_lengths[0] = ps_drag_pipe1_length;
		this->drag_configs[0].pipe_lengths[1] = ps_drag_pipe2_length;
		this->drag_configs[0].pipe_radius = ps_drag_radius;
		this->drag_configs[0].head_width = ps_drag_head_width;
		this->drag_configs[0].head_height = ps_drag_head_length;

		this->drag_configs[1].trunnion_gapsize = sb_drag_trunnion_gapsize;
		this->drag_configs[1].trunnion_length = sb_drag_trunnion_length;
		this->drag_configs[1].pipe_lengths[0] = sb_drag_pipe1_length;
		this->drag_configs[1].pipe_lengths[1] = sb_drag_pipe2_length;
		this->drag_configs[1].pipe_radius = sb_drag_radius;
		this->drag_configs[1].head_width = sb_drag_head_width;
		this->drag_configs[1].head_height = sb_drag_head_length;
	}

public:
	virtual void load(float width, float height, float vinset) = 0;
	virtual void reflow(float width, float height, float vinset) = 0;

public:
	virtual bool can_select(IGraphlet* g) { return false; }
	virtual bool can_select_multiple() { return false; }
	virtual void on_tap_selected(IGraphlet* g, float x, float y) {}
	virtual void on_gesture(std::list<float2>& anchors, float x, float y) {}

public:
	virtual void draw_cables(CanvasDrawingSession^ ds, float Width, float Height) {}

public:
	void pre_read_data(Syslog* logger) override {
		this->master->enter_critical_section();
		this->master->begin_update_sequence();
	}

	void post_read_data(Syslog* logger) override {
		this->master->end_update_sequence();
		this->master->leave_critical_section();
	}

protected:
	template<typename E>
	void load_dimension(std::map<E, Credit<Dimensionlet, E>*>& ds, E id, Platform::String^ unit) {
		ds[id] = this->master->insert_one(new Credit<Dimensionlet, E>(this->plain_style, unit, _speak(id)), id);
	}

	template<typename E>
	void load_dimension(std::map<E, Credit<Dimensionlet, E>*>& ds, std::map<E, Credit<Labellet, E>*>& ls, E id, Platform::String^ unit) {
		this->load_label(ls, id, Colours::Silver);
		ds[id] = this->master->insert_one(new Credit<Dimensionlet, E>(this->plain_style, unit), id);
	}

	template<typename E>
	void load_percentage(std::map<E, Credit<Percentagelet, E>*>& ps, E id) {
		ps[id] = this->master->insert_one(new Credit<Percentagelet, E>(), id);
	}

	template<class C, typename E>
	void load_cylinders(std::map<E, Credit<C, E>*>& cs, E id0, E idn, float height, double vmin, double vmax, Platform::String^ unit) {
		for (E id = id0; id <= idn; id++) {
			auto cylinder = new Credit<C, E>(LiquidSurface::_, vmin, vmax, height * 0.2718F, height);

			cs[id] = this->master->insert_one(cylinder, id);

			this->load_dimension(this->pressures, this->labels, id, unit);
		}
	}

	template<class C, typename E>
	void load_densityflowmeters(std::map<E, Credit<C, E>*>& dfs, E id0, E idn, float height) {
		for (E id = id0; id <= idn; id++) {
			dfs[id] = this->master->insert_one(new Credit<C, E>(height), id);
		}
	}

	template<class C, typename E>
	void load_winches(std::map<E, Credit<C, E>*>& ws, E id0, E idn, float radius) {
		for (E id = id0; id <= idn; id++) {
			ws[id] = this->master->insert_one(new Credit<C, E>(radius), id);

			this->load_label(this->labels, id, this->caption_color);
			this->lengths[id] = this->master->insert_one(new Credit<Dimensionlet, E>("meter"), id);
			this->speeds[id] = this->master->insert_one(new Credit<Dimensionlet, E>("mpm"), id);

			this->load_label(this->winch_saddles, _speak(DS::SaddleLimited.ToString()), id, winch_status_color);
			this->load_label(this->winch_uppers, _speak(DS::Upper.ToString()), id, winch_status_color);
			this->load_label(this->winch_soft_uppers, _speak(DS::SoftUpper.ToString()), id, winch_status_color);
		}

		this->load_label(this->winch_suctions, _speak(DS::SuctionLimited.ToString()), id0, winch_status_color);
		this->load_label(this->winch_slacks, _speak(DS::Slack.ToString()), id0, winch_status_color);
	}

	template<class C, typename E>
	void load_compensator(std::map<E, Credit<C, E>*>& cs, E id, float height, double range) {
		cs[id] = this->master->insert_one(new Credit<C, E>(range, height / 2.718F, height), id);

		this->load_label(this->labels, id, this->caption_color);
		this->lengths[id] = this->master->insert_one(new Credit<Dimensionlet, E>("meter"), id);
		this->pressures[id] = this->master->insert_one(new Credit<Dimensionlet, E>("bar"), id);
	}

	template<class C, typename E>
	void load_compensators(std::map<E, Credit<C, E>*>& cs, E id0, E idn, float height, double range) {
		for (E id = id0; id <= idn; id++) {
			this->load_compensator(cs, id, height, range);
		}
	}

	template<class C, typename E>
	void load_draghead(std::map<E, Credit<C, E>*>& ds, E id, E dpid, float radius, DragInfo& info, unsigned int visor_color) {
		ds[id] = this->master->insert_one(new Credit<C, E>(radius, visor_color, drag_depth(info)), id);

		this->pressures[id] = this->master->insert_one(new Credit<Dimensionlet, E>("bar", _speak(dpid)), id);
		this->lengths[id] = this->master->insert_one(new Credit<Dimensionlet, E>("meter"), id);
		this->degrees[id] = this->master->insert_one(new Credit<Dimensionlet, E>("degrees"), id);
	}

	template<class C, typename E>
	void load_drag(std::map<E, Credit<C, E>*>& ds, E id, float width, float height, DragInfo& info, unsigned int visor_color) {
		ds[id] = this->master->insert_one(new Credit<C, E>(info, width, height, visor_color), id);
	}

	template<typename E>
	void load_label(std::map<E, Credit<Labellet, E>*>& ls, E id, ICanvasBrush^ color, CanvasTextFormat^ font = nullptr) {
		this->load_label(ls, _speak(id), id, color, font);
	}

	template<typename E>
	void load_label(std::map<E, Credit<Labellet, E>*>& ls, Platform::String^ label, E id, ICanvasBrush^ color, CanvasTextFormat^ font = nullptr) {
		ls[id] = this->master->insert_one(new Credit<Labellet, E>(label, font, color), id);
	}

protected:
	void reflow_winch_limits(DS id) {
		this->master->move_to(this->winch_saddles[id], this->winches[id], GraphletAnchor::CT, GraphletAnchor::CB);
		this->master->move_to(this->winch_uppers[id], this->winch_saddles[id], GraphletAnchor::CT, GraphletAnchor::CB);
		this->master->move_to(this->winch_soft_uppers[id], this->winch_uppers[id], GraphletAnchor::CT, GraphletAnchor::CB);

		if (this->winch_suctions.find(id) != this->winch_suctions.end()) {
			this->master->move_to(this->winch_slacks[id], this->winch_soft_uppers[id], GraphletAnchor::CT, GraphletAnchor::CB);
			this->master->move_to(this->winch_suctions[id], this->winch_slacks[id], GraphletAnchor::CT, GraphletAnchor::CB);
		}
	}

protected:
	void set_compensator(DS id, const uint8* db203, unsigned int rd_idx, GraphletAnchor a) {
		float progress = RealData(db203, rd_idx + 2U);

		this->compensators[id]->set_value(progress);
		this->lengths[id]->set_value(progress, a);
		this->pressures[id]->set_value(RealData(db203, rd_idx + 0U), a);
	}

	void set_winch_metrics(DS id, const uint8* db2, unsigned int speed_idx, unsigned int length_idx, GraphletAnchor a) {
		this->speeds[id]->set_value(DBD(db2, speed_idx), a);
		this->lengths[id]->set_value(DBD(db2, length_idx), a);
	}

	void set_winch_limits(DS id0, DS idn) {
		for (DS id = id0; id <= idn; id++) {
			bool slack = false;
			bool upper = false;
			bool saddle = false;
			bool suction = false;
			bool soft_upper = false;

			switch (this->winches[id]->get_status()) {
			case WinchStatus::SaddleLimited: saddle = true; break;
			case WinchStatus::SuctionLimited: suction = true; break;
			case WinchStatus::UpperLimited: upper = true; break;
			case WinchStatus::SensorUpperLimited: soft_upper = true; break;
			case WinchStatus::SaddleSlack: saddle = true; slack = true; break;
			case WinchStatus::SuctionSlack: suction = true; slack = true; break;
			}

			this->winch_saddles[id]->set_color(saddle ? winch_status_highlight_color : winch_status_color);
			this->winch_uppers[id]->set_color(upper ? winch_status_highlight_color : winch_status_color);
			this->winch_soft_uppers[id]->set_color(soft_upper ? winch_status_highlight_color : winch_status_color);

			if (this->winch_slacks.find(id) != this->winch_slacks.end()) {
				this->winch_slacks[id]->set_color(slack ? winch_status_highlight_color : winch_status_color);
			}

			if (this->winch_suctions.find(id) != this->winch_suctions.end()) {
				this->winch_suctions[id]->set_color(suction ? winch_status_highlight_color : winch_status_color);
			}
		}
	}

	void set_draghead_angles(DS id, const uint8* db2, unsigned int idx) {
		float visor_angle = DBD(db2, idx);

		this->dragheads[id]->set_angles(visor_angle - 90.0, 0.0);
		this->degrees[id]->set_value(visor_angle, GraphletAnchor::CC);
	}

	void set_drag_positions(DS id, const uint8* db2, DredgeAddress* address) {
		unsigned int pidx = address->drag_position;
		unsigned int aidx = address->draghead_angle;
		float3 ujoints[2];
		float3 draghead = DBD_3(db2, pidx + 36U);
		float suction_depth = DBD(db2, pidx + 0U);
		float draghead_angle = DBD(db2, aidx) - 90.0F;

		ujoints[0] = DBD_3(db2, pidx + 12U);
		ujoints[1] = DBD_3(db2, pidx + 24U);

		ujoints[1].y = DBD(db2, pidx + 48U);
		draghead.y = DBD(db2, pidx + 52U);
		draghead.z = DBD(db2, pidx + 56U);

		this->dragxys[id]->set_position(suction_depth, ujoints, draghead, draghead_angle);
		this->dragxzes[id]->set_position(suction_depth, ujoints, draghead, draghead_angle);
		this->dragheads[id]->set_depths(suction_depth, draghead.z);

		this->lengths[id]->set_value(draghead.z, GraphletAnchor::CC);
	}

protected: // never delete these graphlets manually.
	std::map<DS, Credit<Labellet, DS>*> labels;
	std::map<DS, Credit<Winchlet, DS>*> winches;
	std::map<DS, Credit<Compensatorlet, DS>*> compensators;
	std::map<DS, Credit<DragHeadlet, DS>*> dragheads;
	std::map<DS, Credit<DragXYlet, DS>*> dragxys;
	std::map<DS, Credit<DragXZlet, DS>*> dragxzes;
	std::map<DS, Credit<Dimensionlet, DS>*> forces;
	std::map<DS, Credit<Dimensionlet, DS>*> pressures;
	std::map<DS, Credit<Dimensionlet, DS>*> degrees;
	std::map<DS, Credit<Dimensionlet, DS>*> lengths;
	std::map<DS, Credit<Dimensionlet, DS>*> speeds;
	std::map<DS, Credit<Labellet, DS>*> winch_suctions;
	std::map<DS, Credit<Labellet, DS>*> winch_saddles;
	std::map<DS, Credit<Labellet, DS>*> winch_slacks;
	std::map<DS, Credit<Labellet, DS>*> winch_uppers;
	std::map<DS, Credit<Labellet, DS>*> winch_soft_uppers;
	
protected:
	CanvasTextFormat^ caption_font;
	CanvasTextFormat^ label_font;
	CanvasTextFormat^ station_font;
	ICanvasBrush^ caption_color;
	DimensionStyle percentage_style;
	DimensionStyle plain_style;

protected:
	DragInfo drag_configs[2];

protected:
	DredgesPage* master;
};

private class Dredges final : public IDredgingSystem {
public:
	Dredges(DredgesPage* master) : IDredgingSystem(master) {
		this->ps_address = make_ps_dredging_system_schema();
		this->sb_address = make_sb_dredging_system_schema();
	}

public:
	void on_analog_input(const uint8* DB203, size_t count, Syslog* logger) override {
		this->overflowpipe->set_value(RealData(DB203, overflow_pipe_progress));
		this->lengths[DS::Overflow]->set_value(this->overflowpipe->get_value());

		this->progresses[DS::D003]->set_value(RealData(DB203, gate_valve_D03_progress), GraphletAnchor::LB);
		this->progresses[DS::D004]->set_value(RealData(DB203, gate_valve_D04_progress), GraphletAnchor::LT);

		this->set_cylinder(DS::PSHPDP, RealData(DB203, this->ps_address->discharge_pressure));
		this->set_cylinder(DS::PSHPVP, RealData(DB203, this->ps_address->vacuum_pressure));
		this->set_cylinder(DS::SBHPDP, RealData(DB203, this->sb_address->discharge_pressure));
		this->set_cylinder(DS::SBHPVP, RealData(DB203, this->sb_address->vacuum_pressure));

		this->set_compensator(DS::PSWC, DB203, this->ps_address->compensator, GraphletAnchor::LC);
		this->set_compensator(DS::SBWC, DB203, this->sb_address->compensator, GraphletAnchor::RC);

		this->pressures[DS::PS]->set_value(RealData(DB203, this->ps_address->differential_pressure), GraphletAnchor::CC);
		this->pressures[DS::SB]->set_value(RealData(DB203, this->sb_address->differential_pressure), GraphletAnchor::CC);

		this->set_density_speed(DS::PS, DB203, this->ps_address->density_speed);
		this->set_density_speed(DS::SB, DB203, this->sb_address->density_speed);

		this->forces[DS::PSPF1]->set_value(RealData(DB203, this->ps_address->pulling_force + 0U), GraphletAnchor::LC);
		this->forces[DS::PSPF2]->set_value(RealData(DB203, this->ps_address->pulling_force + 1U), GraphletAnchor::LC);
		this->forces[DS::SBPF1]->set_value(RealData(DB203, this->sb_address->pulling_force + 0U), GraphletAnchor::LC);
		this->forces[DS::SBPF2]->set_value(RealData(DB203, this->sb_address->pulling_force + 1U), GraphletAnchor::LC);
	}

	void on_realtime_data(const uint8* DB2, size_t count, Syslog* logger) override {
		unsigned int psws_idx = this->ps_address->winch_speed;
		unsigned int pswl_idx = this->ps_address->winch_length;
		unsigned int sbws_idx = this->sb_address->winch_speed;
		unsigned int sbwl_idx = this->sb_address->winch_length;

		this->set_drag_positions(DS::PS, DB2, this->ps_address);
		this->set_drag_positions(DS::SB, DB2, this->sb_address);

		this->set_draghead_angles(DS::PS, DB2, this->ps_address->draghead_angle);
		this->set_draghead_angles(DS::SB, DB2, this->sb_address->draghead_angle);

		this->set_winch_metrics(DS::psTrunnion,     DB2, psws_idx + 0U, pswl_idx + 0U, GraphletAnchor::LC);
		this->set_winch_metrics(DS::psIntermediate, DB2, psws_idx + 4U, pswl_idx + 4U, GraphletAnchor::LC);
		this->set_winch_metrics(DS::psDragHead,     DB2, psws_idx + 8U, pswl_idx + 8U, GraphletAnchor::LC);

		this->set_winch_metrics(DS::sbTrunnion,     DB2, sbws_idx + 0U, sbwl_idx + 0U, GraphletAnchor::RC);
		this->set_winch_metrics(DS::sbIntermediate, DB2, sbws_idx + 4U, sbwl_idx + 4U, GraphletAnchor::RC);
		this->set_winch_metrics(DS::sbDragHead,     DB2, sbws_idx + 8U, sbwl_idx + 8U, GraphletAnchor::RC);

		this->overflowpipe->set_liquid_height(DBD(DB2, 224U));
	}

	void on_digital_input(const uint8* DB4, size_t count4, const uint8* DB205, size_t count205, Syslog* logger) override {
		DI_hopper_pumps(this->hpumps[DS::PSHP], this->hpumps[DS::PSHP], DB4, ps_hopper_pump_feedback, DB205, ps_hopper_pump_details, ps_underwater_pump_details);
		DI_hopper_pumps(this->hpumps[DS::SBHP], this->hpumps[DS::SBHP], DB4, sb_hopper_pump_feedback, DB205, sb_hopper_pump_details, sb_underwater_pump_details);

		DI_gate_valve(this->valves[DS::D003], DB205, gate_valve_D03_feedback, DB205, gate_valve_D03_status);
		DI_gate_valve(this->valves[DS::D004], DB205, gate_valve_D04_feedback, DB205, gate_valve_D04_status);
		DI_gate_valve(this->valves[DS::D011], DB4, gate_valve_D11_feedback, DB205, gate_valve_D11_status);
		DI_gate_valve(this->valves[DS::D012], DB4, gate_valve_D12_feedback, DB205, gate_valve_D12_status);
		DI_gate_valve(this->valves[DS::D013], DB4, gate_valve_D13_feedback, DB205, gate_valve_D13_status);
		DI_gate_valve(this->valves[DS::D014], DB4, gate_valve_D14_feedback, DB205, gate_valve_D14_status);
		DI_gate_valve(this->valves[DS::D015], DB4, gate_valve_D15_feedback, DB205, gate_valve_D15_status);
		DI_gate_valve(this->valves[DS::D016], DB4, gate_valve_D16_feedback, DB205, gate_valve_D16_status);
		
		DI_winch(this->winches[DS::psTrunnion], DB4, winch_ps_trunnion_limits, DB205, winch_ps_trunnion_details);
		DI_winch(this->winches[DS::psIntermediate], DB4, winch_ps_intermediate_limits, DB205, winch_ps_intermediate_details);
		DI_winch(this->winches[DS::psDragHead], DB4, winch_ps_draghead_limits, DB205, winch_ps_draghead_details);

		DI_winch(this->winches[DS::sbTrunnion], DB4, winch_sb_trunnion_limits, DB205, winch_sb_trunnion_details);
		DI_winch(this->winches[DS::sbIntermediate], DB4, winch_sb_intermediate_limits, DB205, winch_sb_intermediate_details);
		DI_winch(this->winches[DS::sbDragHead], DB4, winch_sb_draghead_limits, DB205, winch_sb_draghead_details);

		this->set_winch_limits(DS::psTrunnion, DS::psDragHead);
		this->set_winch_limits(DS::sbTrunnion, DS::sbDragHead);

		this->set_hopper_type(DS::PS, DB4, ps_hopper_pump_feedback);
		this->set_hopper_type(DS::SB, DB4, sb_hopper_pump_feedback);
	}

public:
	void load(float width, float height, float vinset) override {
		this->load_station(width, height, vinset);
		this->load_dashboard(width, height, vinset);
	}

	void reflow(float width, float height, float vinset) {
		this->reflow_station(width, height, vinset);
		this->reflow_dashboard(width, height, vinset);
	}

public:
	void load_station(float width, float height, float vinset) {
		float gridsize = vinset * 0.618F;
		float rx = gridsize;
		float ry = rx * 2.0F;
		float rsct = rx * 0.5F;
		float rlmod = gridsize * 1.5F;
		Turtle<DS>* pTurtle = new Turtle<DS>(gridsize, gridsize, DS::LMOD);

		pTurtle->jump_right(1.5F)->move_right(2.5F, DS::D011)->move_right(3, DS::sb)->move_down(4, DS::SBHP);
		pTurtle->move_right(3, DS::D003)->move_right(3, DS::SB)->jump_back();
		pTurtle->move_right(3)->move_up(4, DS::d13)->move_left(1.5F)->move_up(2, DS::D013)->jump_back();
		pTurtle->move_up(4)->move_left(1.5F)->move_up(2, DS::D015)->jump_back(DS::LMOD);

		pTurtle->jump_left(1.5F)->move_left(2.5F, DS::D012)->move_left(3, DS::ps)->move_down(4, DS::PSHP);
		pTurtle->move_left(3, DS::D004)->move_left(3, DS::PS)->jump_back();
		pTurtle->move_left(3)->move_up(4, DS::d14)->move_right(1.5F)->move_up(2, DS::D014)->jump_back();
		pTurtle->move_up(4)->move_right(1.5F)->move_up(2, DS::D016);

		this->station = this->master->insert_one(new Tracklet<DS>(pTurtle, default_pipe_thickness * 1.618F, default_pipe_color));

		this->load_percentage(this->progresses, DS::D003);
		this->load_percentage(this->progresses, DS::D004);
		this->load_valves(this->valves, this->labels, DS::D003, DS::D012, vinset, 0.0);
		this->load_valves(this->valves, this->labels, DS::D013, DS::D016, vinset, -90.0);
		this->load_label(this->labels, DS::LMOD.ToString(), DS::LMOD, Colours::Cyan, this->station_font);
		this->load_label(this->hopper_types, _speak(DS::pump), DS::PS, Colours::Salmon, this->caption_font);
		this->load_label(this->hopper_types, _speak(DS::pump), DS::SB, Colours::Salmon, this->caption_font);

		this->lmod = this->master->insert_one(new Arclet(0.0, 360.0, rlmod, rlmod, default_pipe_thickness, Colours::Green));
		this->hpumps[DS::PSHP] = this->master->insert_one(new Credit<HopperPumplet, DS>(+rx, -ry), DS::PSHP);
		this->hpumps[DS::SBHP] = this->master->insert_one(new Credit<HopperPumplet, DS>(-rx, -ry), DS::SBHP);
		this->suctions[DS::PS] = this->master->insert_one(new Credit<Circlelet, DS>(rsct, default_ps_color, default_pipe_thickness), DS::PS);
		this->suctions[DS::SB] = this->master->insert_one(new Credit<Circlelet, DS>(rsct, default_sb_color, default_pipe_thickness), DS::SB);
	}

	void load_dashboard(float width, float height, float vinset) {
		float shwidth, shheight, shhmargin, shvmargin, xstep, ystep;
		
		this->station->fill_extent(0.0F, 0.0F, &shwidth, &shheight);
		this->station->fill_stepsize(&xstep, &ystep);

		shwidth += (xstep * 2.0F);
		shheight += ystep;
		shvmargin = (width - shwidth) * 0.5F;
		shhmargin = (height - vinset * 2.0F - shheight) * 0.5F;

		{ // load dimensions
			float overflow_height = shheight * 0.618F;
			float dfmeter_height = shhmargin * 0.72F;
			float cylinder_height = shheight * 0.5F;
			float winch_radius = shvmargin * 0.12F;
		
			this->overflowpipe = this->master->insert_one(new OverflowPipelet(hopper_height_range, overflow_height));
			this->load_dimension(this->lengths, DS::Overflow, "meter");
			this->load_dimension(this->forces, DS::PSPF1, "knewton");
			this->load_dimension(this->forces, DS::PSPF2, "knewton");
			this->load_dimension(this->forces, DS::SBPF1, "knewton");
			this->load_dimension(this->forces, DS::SBPF2, "knewton");

			this->load_densityflowmeters(this->dfmeters, DS::PS, DS::SB, dfmeter_height);
			this->load_compensators(this->compensators, DS::PSWC, DS::SBWC, cylinder_height, compensator_range);
			this->load_cylinders(this->cylinders, DS::PSHPDP, DS::SBHPDP, cylinder_height, 0.0, 20.0, "bar");
			this->load_cylinders(this->cylinders, DS::PSHPVP, DS::SBHPVP, cylinder_height, -2.0, 2.0, "bar");

			this->load_winches(this->winches, DS::psTrunnion, DS::psDragHead, winch_radius);
			this->load_winches(this->winches, DS::sbTrunnion, DS::sbDragHead, winch_radius);
		}

		{ // load drags
			float draghead_radius = shhmargin * 0.32F;
			float over_drag_height = height * 0.5F - vinset;
			float over_drag_width = over_drag_height * 0.382F;
			float side_drag_width = width * 0.5F - (draghead_radius + vinset) * 2.0F;
			float side_drag_height = height * 0.382F - vinset;
			
			this->load_draghead(this->dragheads, DS::PS, DS::PSDP, -draghead_radius, this->drag_configs[0], default_ps_color);
			this->load_draghead(this->dragheads, DS::SB, DS::SBDP, +draghead_radius, this->drag_configs[1], default_sb_color);
		
			this->load_drag(this->dragxys, DS::PS, -over_drag_width, over_drag_height, this->drag_configs[0], default_ps_color);
			this->load_drag(this->dragxys, DS::SB, +over_drag_width, over_drag_height, this->drag_configs[1], default_sb_color);

			this->load_drag(this->dragxzes, DS::PS, -side_drag_width, side_drag_height, this->drag_configs[0], default_ps_color);
			this->load_drag(this->dragxzes, DS::SB, +side_drag_width, side_drag_height, this->drag_configs[1], default_sb_color);
		}
	}

public:
	void reflow_station(float width, float height, float vinset) {
		GraphletAnchor anchor;
		float dx, dy, xstep, ystep;
		float x0 = 0.0F;
		float y0 = 0.0F;
		
		this->station->fill_stepsize(&xstep, &ystep);

		this->master->move_to(this->station, width * 0.5F, height * 0.5F, GraphletAnchor::CC);

		for (auto it = this->valves.begin(); it != this->valves.end(); it++) {
			switch (it->first) {
			case DS::D014: case DS::D016: {
				dx = x0 + xstep; dy = y0; anchor = GraphletAnchor::LC;
			}; break;
			case DS::D013: case DS::D015: {
				dx = x0 - xstep; dy = y0; anchor = GraphletAnchor::RC;
			}; break;
			default: {
				dx = x0; dy = y0 - ystep; anchor = GraphletAnchor::CB;
			}
			}

			this->station->map_credit_graphlet(it->second, GraphletAnchor::CC, x0, y0);
			this->station->map_credit_graphlet(this->labels[it->first], anchor, dx, dy);
		}

		this->station->map_credit_graphlet(this->progresses[DS::D003], GraphletAnchor::CT, 0.0F, ystep);
		this->station->map_credit_graphlet(this->progresses[DS::D004], GraphletAnchor::CT, 0.0F, ystep);

		this->station->map_graphlet_at_anchor(this->lmod, DS::LMOD, GraphletAnchor::CC);
		this->station->map_credit_graphlet(this->labels[DS::LMOD], GraphletAnchor::CC);
		
		this->hpumps[DS::SBHP]->fill_pump_origin(&dx, nullptr);
		this->station->map_credit_graphlet(this->hpumps[DS::SBHP], GraphletAnchor::CC, +std::fabsf(dx));
		this->station->map_credit_graphlet(this->hpumps[DS::PSHP], GraphletAnchor::CC, -std::fabsf(dx));
		this->station->map_credit_graphlet(this->suctions[DS::PS], GraphletAnchor::CC);
		this->station->map_credit_graphlet(this->suctions[DS::SB], GraphletAnchor::CC);

		this->master->move_to(this->cylinders[DS::PSHPDP], this->station, GraphletAnchor::LC, GraphletAnchor::RB, 0.0F, vinset);
		this->master->move_to(this->cylinders[DS::PSHPVP], this->cylinders[DS::PSHPDP], GraphletAnchor::LB, GraphletAnchor::RB, -vinset);
		this->master->move_to(this->cylinders[DS::SBHPDP], this->station, GraphletAnchor::RC, GraphletAnchor::LB, 0.0F, vinset);
		this->master->move_to(this->cylinders[DS::SBHPVP], this->cylinders[DS::SBHPDP], GraphletAnchor::RB, GraphletAnchor::LB, +vinset);
	}

	void reflow_dashboard(float width, float height, float vinset) {
		float txt_gapsize = vinset * 0.5F;
		float cx = width * 0.5F;
		float cy = height * 0.5F;
		float xstep, ystep;
		float trunnion_y, intermediate_y, draghead_y;
		
		this->station->fill_stepsize(&xstep, &ystep);

		{ // reflow centeral components
			this->master->move_to(this->lengths[DS::Overflow], this->station, GraphletAnchor::CC, GraphletAnchor::CB);
			this->master->move_to(this->overflowpipe, this->lengths[DS::Overflow], GraphletAnchor::CT, GraphletAnchor::CB, 0.0F, -txt_gapsize);

			this->master->move_to(this->dfmeters[DS::PS], this->overflowpipe, GraphletAnchor::LT, GraphletAnchor::RB);
			this->master->move_to(this->dfmeters[DS::SB], this->overflowpipe, GraphletAnchor::RT, GraphletAnchor::LB);
			this->master->move_to(this->compensators[DS::PSWC], this->cylinders[DS::PSHPVP], GraphletAnchor::LB, GraphletAnchor::RB, -vinset);

			this->master->move_to(this->dragheads[DS::PS], cx, height * 0.80F, GraphletAnchor::RC, -xstep);
			this->master->move_to(this->dragheads[DS::SB], cx, height * 0.80F, GraphletAnchor::LC, +xstep);
			this->master->move_to(this->compensators[DS::SBWC], this->cylinders[DS::SBHPVP], GraphletAnchor::RB, GraphletAnchor::LB, +vinset);
		}

		{ // reflow left dredging system
			float dflx, wclx;

			this->master->fill_graphlet_location(this->dfmeters[DS::PS], &dflx, nullptr, GraphletAnchor::LC);
			this->master->fill_graphlet_location(this->compensators[DS::PSWC], &wclx, nullptr, GraphletAnchor::LC);

			this->master->move_to(this->dragxys[DS::PS], std::fminf(dflx, wclx), cy, GraphletAnchor::RB, -vinset, vinset);
			this->master->move_to(this->dragxzes[DS::PS], xstep, height - vinset - ystep, GraphletAnchor::LB);

			{ // reflow winches
				float lx = vinset;

				this->master->fill_graphlet_location(this->dragxys[DS::PS], nullptr, &trunnion_y, GraphletAnchor::CT);
				this->master->fill_graphlet_location(this->dragxys[DS::PS], nullptr, &intermediate_y, GraphletAnchor::CC);
				this->master->fill_graphlet_location(this->dragxys[DS::PS], nullptr, &draghead_y, GraphletAnchor::CB);

				this->master->move_to(this->winches[DS::psTrunnion], lx, trunnion_y, GraphletAnchor::LT, 0.0F, vinset * 6.0F);
				this->master->move_to(this->winches[DS::psIntermediate], lx, intermediate_y, GraphletAnchor::LC, 0.0F, vinset * 3.0F);
				this->master->move_to(this->winches[DS::psDragHead], lx, draghead_y, GraphletAnchor::LB, 0.0F, -vinset * 0.0F);
			}
		}

		{ // reflow right dredging system
			float dfrx, wcrx;

			this->master->fill_graphlet_location(this->dfmeters[DS::SB], &dfrx, nullptr, GraphletAnchor::RC);
			this->master->fill_graphlet_location(this->compensators[DS::SBWC], &wcrx, nullptr, GraphletAnchor::RC);

			this->master->move_to(this->dragxys[DS::SB], std::fmaxf(dfrx, wcrx), cy, GraphletAnchor::LB, vinset, vinset);
			this->master->move_to(this->dragxzes[DS::SB], width - xstep, height - vinset - ystep, GraphletAnchor::RB);

			{ // reflow winches
				float rx = width - vinset;

				this->master->fill_graphlet_location(this->dragxys[DS::SB], nullptr, &trunnion_y, GraphletAnchor::CT);
				this->master->fill_graphlet_location(this->dragxys[DS::SB], nullptr, &intermediate_y, GraphletAnchor::CC);
				this->master->fill_graphlet_location(this->dragxys[DS::SB], nullptr, &draghead_y, GraphletAnchor::CB);

				this->master->move_to(this->winches[DS::sbTrunnion], rx, trunnion_y, GraphletAnchor::RT, 0.0F, vinset * 6.0F);
				this->master->move_to(this->winches[DS::sbIntermediate], rx, intermediate_y, GraphletAnchor::RC, 0.0F, vinset * 3.0F);
				this->master->move_to(this->winches[DS::sbDragHead], rx, draghead_y, GraphletAnchor::RB, 0.0F, -vinset * 0.0F);
			}
		}

		{ // reflow dimensions and labels
			for (DS id = DS::PSWC; id <= DS::SBWC; id++) {
				this->master->move_to(this->labels[id], this->compensators[id], GraphletAnchor::CT, GraphletAnchor::CB, 0.0F, -txt_gapsize);
				this->master->move_to(this->lengths[id], this->compensators[id], GraphletAnchor::CB, GraphletAnchor::CT);
				this->master->move_to(this->pressures[id], this->lengths[id], GraphletAnchor::CB, GraphletAnchor::CT);
			}

			for (DS id = DS::PS; id <= DS::SB; id++) {
				this->master->move_to(this->pressures[id], this->dragheads[id], GraphletAnchor::CT, GraphletAnchor::CB);
				this->master->move_to(this->degrees[id], this->dragheads[id], GraphletAnchor::CB, GraphletAnchor::CT);
				this->master->move_to(this->lengths[id], this->degrees[id], GraphletAnchor::CB, GraphletAnchor::CT);
			}

			for (DS id = DS::PSHPDP; id <= DS::SBHPVP; id++) {
				this->master->move_to(this->labels[id], this->cylinders[id], GraphletAnchor::CT, GraphletAnchor::CB, 0.0F, -txt_gapsize);
				this->master->move_to(this->pressures[id], this->cylinders[id], GraphletAnchor::CB, GraphletAnchor::CT, 0.0F, txt_gapsize);
			}

			for (DS id = DS::psTrunnion; id <= DS::psDragHead; id++) {
				this->master->move_to(this->lengths[id], this->winches[id], GraphletAnchor::RC, GraphletAnchor::LC, txt_gapsize);
				this->master->move_to(this->labels[id], this->lengths[id], GraphletAnchor::LT, GraphletAnchor::LB);
				this->master->move_to(this->speeds[id], this->lengths[id], GraphletAnchor::LB, GraphletAnchor::LT);

				this->reflow_winch_limits(id);
			}

			for (DS id = DS::sbTrunnion; id <= DS::sbDragHead; id++) {
				this->master->move_to(this->lengths[id], this->winches[id], GraphletAnchor::LC, GraphletAnchor::RC, -txt_gapsize);
				this->master->move_to(this->labels[id], this->lengths[id], GraphletAnchor::RT, GraphletAnchor::RB);
				this->master->move_to(this->speeds[id], this->lengths[id], GraphletAnchor::RB, GraphletAnchor::RT);

				this->reflow_winch_limits(id);
			}

			this->master->move_to(this->forces[DS::PSPF1], this->dragxzes[DS::PS], GraphletAnchor::LT, GraphletAnchor::LB);
			this->master->move_to(this->forces[DS::PSPF2], this->dragxzes[DS::PS], GraphletAnchor::CT, GraphletAnchor::LB);
			this->master->move_to(this->forces[DS::SBPF1], this->dragxzes[DS::SB], GraphletAnchor::RT, GraphletAnchor::RB);
			this->master->move_to(this->forces[DS::SBPF2], this->dragxzes[DS::SB], GraphletAnchor::CT, GraphletAnchor::RB);

			this->master->move_to(this->hopper_types[DS::PS], this->labels[DS::PSHPDP], GraphletAnchor::LT, GraphletAnchor::CB, -vinset, -txt_gapsize);
			this->master->move_to(this->hopper_types[DS::SB], this->labels[DS::SBHPDP], GraphletAnchor::RT, GraphletAnchor::CB, +vinset, -txt_gapsize);
		}
	}

private:
	template<class G, typename E>
	void load_valves(std::map<E, G*>& gs, std::map<E, Credit<Labellet, E>*>& ls, E id0, E idn, float radius, double degrees) {
		for (E id = id0; id <= idn; id++) {
			this->load_label(ls, id.ToString(), id, Colours::Silver, this->station_font);
			gs[id] = this->master->insert_one(new G(radius, degrees), id);
		}
	}

private:
	void set_cylinder(DS id, float value) {
		this->cylinders[id]->set_value(value);
		this->pressures[id]->set_value(value, GraphletAnchor::CC);
	}

	void set_density_speed(DS id, const uint8* db203, unsigned int idx) {
		this->dfmeters[id]->set_values(RealData(db203, idx + 0U), RealData(db203, idx + 1U));
	}

	void set_hopper_type(DS id, const uint8* db4, unsigned int idx_p1) {
		DS type = DS::pump;

		if (DI_hopper_type(db4, idx_p1)) {
			type = DS::hopper;
		} else if (DI_underwater_type(db4, idx_p1)) {
			type = DS::underwater;
		}

		this->hopper_types[id]->set_text(_speak(type), GraphletAnchor::CC);
	}

private:
	void try_flow_water(DS vid, DS start, DS end, CanvasSolidColorBrush^ color) {
		switch (this->valves[vid]->get_status()) {
		case GateValveStatus::Open: {
			this->station->append_subtrack(start, end, color);
		}
		}
	}

	void try_flow_water(DS vid, DS* path, unsigned int count, CanvasSolidColorBrush^ color) {
		switch (this->valves[vid]->get_status()) {
		case GateValveStatus::Open: {
			this->station->append_subtrack(path, count, color);
		}
		}
	}

	template<unsigned int N>
	void try_flow_water(DS vid, DS (&path)[N], CanvasSolidColorBrush^ color) {
		this->try_flow_water(vid, path, N, color);
	}

private: // never delete these graphlets manually.
	Tracklet<DS>* station;
	std::map<DS, Credit<GateValvelet, DS>*> valves;
	std::map<DS, Credit<HopperPumplet, DS>*> hpumps;
	std::map<DS, Credit<Circlelet, DS>*> suctions;
	std::map<DS, Credit<Cylinderlet, DS>*> cylinders;
	std::map<DS, Credit<DensitySpeedmeterlet, DS>*> dfmeters;
	std::map<DS, Credit<Percentagelet, DS>*> progresses;
	std::map<DS, Credit<Labellet, DS>*> hopper_types;
	OverflowPipelet* overflowpipe;
	Arclet* lmod;

private: // never delete these global objects
	DredgeAddress* ps_address;
	DredgeAddress* sb_address;
};

private class Drags final
	: public IDredgingSystem
	, public IMenuCommand<DSWOperation, Credit<Winchlet, DS>, PLCMaster*>
	, public IMenuCommand<DSGOperation, Credit<Gantrylet, DS>, PLCMaster*>
	, public IMenuCommand<DSWCOperation, Credit<Compensatorlet, DS>, PLCMaster*>
	, public IMenuCommand<DSDVOperation, Credit<DragHeadlet, DS>, PLCMaster*>
	, public IGroupMenuCommand<DSGOperation, DSGroup, PLCMaster*> {
public:
	Drags(DredgesPage* master, DS side, unsigned int drag_color, unsigned int config_idx)
		: IDredgingSystem(master), DS_side(side), drag_color(drag_color), drag_idx(config_idx) {
		this->pump_style = make_highlight_dimension_style(small_metrics_font_size, 6U, Colours::Background);
		this->highlight_style = make_highlight_dimension_style(small_metrics_font_size, 6U, Colours::Green);
		this->setting_style = make_setting_dimension_style(normal_metrics_font_size, 6U);

		this->winch_op = make_menu<DSWOperation, Credit<Winchlet, DS>, PLCMaster*>(this, master->get_plc_device());
		this->gantry_op = make_menu<DSGOperation, Credit<Gantrylet, DS>, PLCMaster*>(this, master->get_plc_device());
		this->compensator_op = make_menu<DSWCOperation, Credit<Compensatorlet, DS>, PLCMaster*>(this, master->get_plc_device());
		this->visor_op = make_menu<DSDVOperation, Credit<DragHeadlet, DS>, PLCMaster*>(this, master->get_plc_device());

		if (this->DS_side == DS::PS) {
			this->address = make_ps_dredging_system_schema();
			this->ggantries_op = make_group_menu<DSGOperation, DSGroup, PLCMaster*>(this, DSGroup::PSGantries, master->get_plc_device());
			this->sign = -1.0F;
		} else {
			this->address = make_sb_dredging_system_schema();
			this->ggantries_op = make_group_menu<DSGOperation, DSGroup, PLCMaster*>(this, DSGroup::SBGantries, master->get_plc_device());
			this->sign = +1.0F;
		}
	}

public:
	void on_analog_input(const uint8* DB203, size_t count, Syslog* logger) override {
		if (DS_side == DS::PS) {
			this->pressures[DS::psTrunnion]->set_value(RealData(DB203, pump_C_pressure), GraphletAnchor::CC);
			this->pressures[DS::psIntermediate]->set_value(RealData(DB203, pump_B_pressure), GraphletAnchor::CC);
			this->pressures[DS::psDragHead]->set_value(RealData(DB203, pump_A_pressure), GraphletAnchor::CC);

			this->rc_speeds[DS::psIntermediate]->set_value(RealData(DB203, winch_ps_intermediate_remote_speed), GraphletAnchor::CC);
			this->rc_speeds[DS::psDragHead]->set_value(RealData(DB203, winch_ps_draghead_remote_speed), GraphletAnchor::CC);
			
			this->forces[DS::PSPF1]->set_value(RealData(DB203, this->address->pulling_force + 0U), GraphletAnchor::LC);
			this->forces[DS::PSPF2]->set_value(RealData(DB203, this->address->pulling_force + 1U), GraphletAnchor::LC);
			
			this->set_compensator(DS::PSWC, DB203, this->address->compensator, GraphletAnchor::LC);

			this->Alets[DS::psTrunnion]->set_value(RealData(DB203, winch_ps_trunnion_A_pressure), GraphletAnchor::LB);
			this->Blets[DS::psTrunnion]->set_value(RealData(DB203, winch_ps_trunnion_B_pressure), GraphletAnchor::LT);
			this->Alets[DS::psIntermediate]->set_value(RealData(DB203, winch_ps_intermediate_A_pressure), GraphletAnchor::LB);
			this->Blets[DS::psIntermediate]->set_value(RealData(DB203, winch_ps_intermediate_B_pressure), GraphletAnchor::LT);
			this->Alets[DS::psDragHead]->set_value(RealData(DB203, winch_ps_draghead_A_pressure), GraphletAnchor::LB);
			this->Blets[DS::psDragHead]->set_value(RealData(DB203, winch_ps_draghead_B_pressure), GraphletAnchor::LT);
		} else {
			this->pressures[DS::sbTrunnion]->set_value(RealData(DB203, pump_F_pressure), GraphletAnchor::CC);
			this->pressures[DS::sbIntermediate]->set_value(RealData(DB203, pump_G_pressure), GraphletAnchor::CC);
			this->pressures[DS::sbDragHead]->set_value(RealData(DB203, pump_H_pressure), GraphletAnchor::CC);

			this->rc_speeds[DS::sbIntermediate]->set_value(RealData(DB203, winch_sb_intermediate_remote_speed), GraphletAnchor::CC);
			this->rc_speeds[DS::sbDragHead]->set_value(RealData(DB203, winch_sb_draghead_remote_speed), GraphletAnchor::CC);

			this->forces[DS::SBPF1]->set_value(RealData(DB203, this->address->pulling_force + 0U), GraphletAnchor::LC);
			this->forces[DS::SBPF2]->set_value(RealData(DB203, this->address->pulling_force + 1U), GraphletAnchor::LC);

			this->set_compensator(DS::SBWC, DB203, this->address->compensator, GraphletAnchor::RC);

			this->Alets[DS::sbTrunnion]->set_value(RealData(DB203, winch_sb_trunnion_A_pressure), GraphletAnchor::RB);
			this->Blets[DS::sbTrunnion]->set_value(RealData(DB203, winch_sb_trunnion_B_pressure), GraphletAnchor::RT);
			this->Alets[DS::sbIntermediate]->set_value(RealData(DB203, winch_sb_intermediate_A_pressure), GraphletAnchor::RB);
			this->Blets[DS::sbIntermediate]->set_value(RealData(DB203, winch_sb_intermediate_B_pressure), GraphletAnchor::RT);
			this->Alets[DS::sbDragHead]->set_value(RealData(DB203, winch_sb_draghead_A_pressure), GraphletAnchor::RB);
			this->Blets[DS::sbDragHead]->set_value(RealData(DB203, winch_sb_draghead_B_pressure), GraphletAnchor::RT);
		}
	}

	void on_realtime_data(const uint8* DB2, size_t count, Syslog* logger) override {
		unsigned int psws_idx = this->address->winch_speed;
		unsigned int pswl_idx = this->address->winch_length;
		unsigned int sbws_idx = this->address->winch_speed;
		unsigned int sbwl_idx = this->address->winch_length;

		this->lengths[DS::TideMark]->set_value(DBD(DB2, 0U));
		this->speeds[DS::Speed]->set_value(DBD(DB2, 584U));

		this->set_drag_positions(this->DS_side, DB2, this->address);
		this->set_draghead_angles(this->DS_side, DB2, this->address->draghead_angle);

		if (this->DS_side == DS::PS) {
			this->set_winch_metrics(DS::psTrunnion, DB2, psws_idx + 0U, pswl_idx + 0U, GraphletAnchor::CC);
			this->set_winch_metrics(DS::psIntermediate, DB2, psws_idx + 4U, pswl_idx + 4U, GraphletAnchor::CC);
			this->set_winch_metrics(DS::psDragHead, DB2, psws_idx + 8U, pswl_idx + 8U, GraphletAnchor::CC);
		} else {
			this->set_winch_metrics(DS::sbTrunnion, DB2, sbws_idx + 0U, sbwl_idx + 0U, GraphletAnchor::CC);
			this->set_winch_metrics(DS::sbIntermediate, DB2, sbws_idx + 4U, sbwl_idx + 4U, GraphletAnchor::CC);
			this->set_winch_metrics(DS::sbDragHead, DB2, sbws_idx + 8U, sbwl_idx + 8U, GraphletAnchor::CC);
		}
	}

	void on_digital_input(const uint8* DB4, size_t count4, const uint8* DB205, size_t count205, Syslog* logger) override {
		if (DS_side == DS::PS) {
			DI_pump_dimension(this->pressures[DS::psTrunnion], DB4, pump_C_feedback);
			DI_pump_dimension(this->pressures[DS::psIntermediate], DB4, pump_B_feedback);
			DI_pump_dimension(this->pressures[DS::psDragHead], DB4, pump_A_feedback);

			DI_winch(this->winches[DS::psTrunnion], DB4, winch_ps_trunnion_limits, DB205, winch_ps_trunnion_details);
			DI_winch(this->winches[DS::psIntermediate], DB4, winch_ps_intermediate_limits, DB205, winch_ps_intermediate_details);
			DI_winch(this->winches[DS::psDragHead], DB4, winch_ps_draghead_limits, DB205, winch_ps_draghead_details);
			this->set_winch_limits(DS::psTrunnion, DS::psDragHead);

			DI_gantry(this->gantries[DS::psTrunnion], DB4, gantry_ps_trunnion_limited, DB205, gantry_ps_trunnion_details);
			DI_gantry(this->gantries[DS::psIntermediate], DB4, gantry_ps_intermediate_limited, DB205, gantry_ps_intermediate_details);
			DI_gantry(this->gantries[DS::psDragHead], DB4, gantry_ps_draghead_limited, DB205, gantry_ps_draghead_details);

			this->set_gantry_virtual_action_status(DS::tVirtualUp, DB205, gantry_ps_trunnion_virtual_up_limited);
			this->set_gantry_virtual_action_status(DS::tVirtualOut, DB205, gantry_ps_trunnion_virtual_out_limited);
			this->set_gantry_virtual_action_status(DS::iVirtualUp, DB205, gantry_ps_intermediate_virtual_up_limited);
			this->set_gantry_virtual_action_status(DS::iVirtualOut, DB205, gantry_ps_intermediate_virtual_out_limited);
			this->set_gantry_virtual_action_status(DS::hVirtualUp, DB205, gantry_ps_draghead_virtual_up_limited);
			this->set_gantry_virtual_action_status(DS::hVirtualOut, DB205, gantry_ps_draghead_virtual_out_limited);
		} else {
			DI_pump_dimension(this->pressures[DS::sbTrunnion], DB4, pump_F_feedback);
			DI_pump_dimension(this->pressures[DS::sbIntermediate], DB4, pump_G_feedback);
			DI_pump_dimension(this->pressures[DS::sbDragHead], DB4, pump_H_feedback);

			DI_winch(this->winches[DS::sbTrunnion], DB4, winch_sb_trunnion_limits, DB205, winch_sb_trunnion_details);
			DI_winch(this->winches[DS::sbIntermediate], DB4, winch_sb_intermediate_limits, DB205, winch_sb_intermediate_details);
			DI_winch(this->winches[DS::sbDragHead], DB4, winch_sb_draghead_limits, DB205, winch_sb_draghead_details);
			this->set_winch_limits(DS::sbTrunnion, DS::sbDragHead);

			DI_gantry(this->gantries[DS::sbTrunnion], DB4, gantry_sb_trunnion_limited, DB205, gantry_sb_trunnion_details);
			DI_gantry(this->gantries[DS::sbIntermediate], DB4, gantry_sb_intermediate_limited, DB205, gantry_sb_intermediate_details);

			if (DI_long_sb_drag(DB205)) {
				DI_gantry(this->gantries[DS::sbDragHead], DB4, gantry_sb_long_draghead_limited, DB205, gantry_sb_draghead_details);
			} else {
				DI_gantry(this->gantries[DS::sbDragHead], DB4, gantry_sb_short_draghead_limited, DB205, gantry_sb_draghead_details);
			}
			
			this->set_gantry_virtual_action_status(DS::tVirtualUp, DB205, gantry_sb_trunnion_virtual_up_limited);
			this->set_gantry_virtual_action_status(DS::tVirtualOut, DB205, gantry_sb_trunnion_virtual_out_limited);
			this->set_gantry_virtual_action_status(DS::iVirtualUp, DB205, gantry_sb_intermediate_virtual_up_limited);
			this->set_gantry_virtual_action_status(DS::iVirtualOut, DB205, gantry_sb_intermediate_virtual_out_limited);
			this->set_gantry_virtual_action_status(DS::hVirtualUp, DB205, gantry_sb_draghead_virtual_up_limited);
			this->set_gantry_virtual_action_status(DS::hVirtualOut, DB205, gantry_sb_draghead_virtual_out_limited);
		}
	}

public:
	bool can_execute(DSWOperation cmd, Credit<Winchlet, DS>* winch, PLCMaster* plc, bool acc_executable) override {
		DS id = winch->id;
		bool draghead = ((id == DS::psDragHead) || (id == DS::sbDragHead));

		return winch_command_executable(winch, cmd, draghead, true) && plc->connected();
	}

	void execute(DSWOperation cmd, Credit<Winchlet, DS>* winch, PLCMaster* plc) override {
		plc->send_command(DO_winch_command(cmd, winch->id));
	}

public:
	bool can_execute(DSGOperation cmd, Credit<Gantrylet, DS>* gantry, PLCMaster* plc, bool acc_executable) override {
		return gantry_command_executable(gantry, cmd, true) && plc->connected();
	}

	void execute(DSGOperation cmd, Credit<Gantrylet, DS>* gantry, PLCMaster* plc) override {
		plc->send_command(DO_gantry_command(cmd, gantry->id));
	}

public:
	bool can_execute(DSGOperation cmd, DSGroup group, PLCMaster* plc) override {
		return plc->connected();
	}

	void execute(DSGOperation cmd, DSGroup group, PLCMaster* plc) override {
		plc->send_command(DO_gantry_group_command(cmd, group));
	}

public:
	bool can_execute(DSWCOperation cmd, Credit<Compensatorlet, DS>* wc, PLCMaster* plc, bool acc_executable) override {
		return plc->connected();
	}

	void execute(DSWCOperation cmd, Credit<Compensatorlet, DS>* wc, PLCMaster* plc) override {
		plc->send_command(DO_wave_compensator_command(cmd, (wc->id == DS::PSWC)));
	}

public:
	bool can_execute(DSDVOperation cmd, Credit<DragHeadlet, DS>* visor, PLCMaster* plc, bool acc_executable) override {
		return plc->connected();
	}

	void execute(DSDVOperation cmd, Credit<DragHeadlet, DS>* visor, PLCMaster* plc) override {
		plc->send_command(DO_visor_command(cmd, (visor->id == DS::PS)));
	}

public:
	void load(float width, float height, float vinset) override {
		float drag_height = height * 0.618F;
		float side_drag_width = width * 0.32F;
		float over_drag_width = drag_height * 0.382F;
		float draghead_radius = side_drag_width * 0.18F;
		float winch_radius = over_drag_width * 0.382F;
		float gantry_radius = drag_height * 0.20F;
		float table_header_width = width * 0.14F;
		DragInfo config = this->drag_configs[this->drag_idx];

		this->load_drag(this->dragxzes, this->DS_side, side_drag_width * this->sign, drag_height, config, this->drag_color);
		this->load_drag(this->dragxys, this->DS_side, over_drag_width * this->sign, drag_height, config, this->drag_color);

		this->load_label(this->labels, DS_side, this->caption_color, this->caption_font);
		this->load_label(this->labels, DS::Overlook, this->caption_color, this->label_font);
		this->load_label(this->labels, DS::Sidelook, this->caption_color, this->label_font);

		this->load_dimension(this->lengths, DS::TideMark, "meter");
		this->load_dimension(this->speeds, DS::Speed, "kmeter");
		
		this->load_table_header(this->table_headers, DS::ps_gantry_settings, table_header_width, normal_font_size, Colours::SeaGreen);
		this->load_gantry_indicators(DS::tVirtualUp, DS::hVirtualOut, large_font_size, this->indicators, this->labels);

		if (this->DS_side == DS::PS) {
			this->load_draghead(this->dragheads, DS::PS, DS::PSDP, -draghead_radius, this->drag_configs[0], default_ps_color);
			this->load_gantries(this->gantries, DS::psTrunnion, DS::psDragHead, -gantry_radius);
			this->load_detailed_winches(this->winches, DS::psTrunnion, DS::psDragHead, winch_radius);
			this->load_compensator(this->compensators, DS::PSWC, gantry_radius, compensator_range);
			this->load_dimensions(this->pressures, DS::psTrunnion, DS::psDragHead, DS::C, "bar");
			this->load_dimension(this->forces, DS::PSPF1, "knewton");
			this->load_dimension(this->forces, DS::PSPF2, "knewton");

			this->load_label(this->labels, DS::ps_gantry_settings, Colours::Black);
		} else {
			this->load_draghead(this->dragheads, DS::SB, DS::SBDP, +draghead_radius, this->drag_configs[1], default_sb_color);
			this->load_gantries(this->gantries, DS::sbTrunnion, DS::sbDragHead, +gantry_radius);
			this->load_detailed_winches(this->winches, DS::sbTrunnion, DS::sbDragHead, winch_radius);
			this->load_compensator(this->compensators, DS::SBWC, gantry_radius, compensator_range);
			this->load_dimensions(this->pressures, DS::sbTrunnion, DS::sbDragHead, DS::F, "bar");
			this->load_dimension(this->forces, DS::SBPF1, "knewton");
			this->load_dimension(this->forces, DS::SBPF2, "knewton");

			this->load_label(this->labels, DS::sb_gantry_settings, Colours::Black);
		}

		this->load_label(this->labels, DS::on, checked_color);
	}

	void reflow(float width, float height, float vinset) override {
		float txt_gapsize = vinset * 0.5F;
		float cx = width * 0.5F;
		float cy = height * 0.5F;
		float vgapsize;

		if (this->DS_side == DS::PS) {
			this->master->move_to(this->dragxzes[DS::PS], vinset, cy, GraphletAnchor::LC);
			this->master->move_to(this->dragxys[DS::PS], this->dragxzes[DS::PS], GraphletAnchor::RC, GraphletAnchor::LC, vinset);
			this->master->move_to(this->dragheads[DS::PS], this->dragxys[DS::PS], GraphletAnchor::LB, GraphletAnchor::RC, -vinset);

			{ // flow gantries and winches
				auto gantry = this->gantries[DS::psIntermediate];
				
				this->pressures[DS::psTrunnion]->fill_extent(0.0F, 0.0F, nullptr, &vgapsize);

				vgapsize = vgapsize * 2.0F + vinset;
				this->master->move_to(gantry, this->dragxys[DS::PS], GraphletAnchor::RC, GraphletAnchor::LC);
				this->master->move_to(this->gantries[DS::psTrunnion], gantry, GraphletAnchor::LT, GraphletAnchor::LB, 0.0F, -vgapsize);
				this->master->move_to(this->gantries[DS::psDragHead], gantry, GraphletAnchor::LB, GraphletAnchor::LT, 0.0F, +vgapsize);
				this->reflow_winches(DS::PSWC, DS::psTrunnion, DS::psDragHead, GraphletAnchor::RC, GraphletAnchor::LC, vinset * 2.0F);
			}

			for (DS id = DS::psTrunnion; id <= DS::psDragHead; id++) {
				this->master->move_to(this->pressures[id], this->gantries[id], GraphletAnchor::RB, GraphletAnchor::CT, 0.0F, txt_gapsize);
				this->master->move_to(this->labels[id], this->winches[id], GraphletAnchor::LC, GraphletAnchor::RB);
				this->master->move_to(this->Alets[id], this->winches[id], GraphletAnchor::RC, GraphletAnchor::LB, txt_gapsize);
				this->master->move_to(this->Blets[id], this->winches[id], GraphletAnchor::RC, GraphletAnchor::LT, txt_gapsize);

				this->reflow_winch_limits(id);

				if (this->rc_speeds.find(id) != this->rc_speeds.end()) {
					this->master->move_to(this->rc_speeds[id], this->winches[id], GraphletAnchor::CB, GraphletAnchor::CT);
					this->master->move_to(this->lengths[id], this->rc_speeds[id], GraphletAnchor::CB, GraphletAnchor::CT);
				} else {
					this->master->move_to(this->lengths[id], this->winches[id], GraphletAnchor::CB, GraphletAnchor::CT);
				}

				this->master->move_to(this->speeds[id], this->lengths[id], GraphletAnchor::CB, GraphletAnchor::CT);
			}
		} else {
			this->master->move_to(this->dragxzes[DS::SB], width - vinset, cy, GraphletAnchor::RC);
			this->master->move_to(this->dragxys[DS::SB], this->dragxzes[DS::SB], GraphletAnchor::LC, GraphletAnchor::RC, -vinset);
			this->master->move_to(this->dragheads[DS::SB], this->dragxys[DS::SB], GraphletAnchor::RB, GraphletAnchor::LC, vinset);

			{ // flow gantries and winches
				auto gantry = this->gantries[DS::sbIntermediate];
				
				this->pressures[DS::sbTrunnion]->fill_extent(0.0F, 0.0F, nullptr, &vgapsize);

				vgapsize = vgapsize * 2.0F + vinset;
				this->master->move_to(gantry, this->dragxys[DS::SB], GraphletAnchor::LC, GraphletAnchor::RC);
				this->master->move_to(this->gantries[DS::sbTrunnion], gantry, GraphletAnchor::RT, GraphletAnchor::RB, 0.0F, -vgapsize);
				this->master->move_to(this->gantries[DS::sbDragHead], gantry, GraphletAnchor::RB, GraphletAnchor::RT, 0.0F, +vgapsize);
				this->reflow_winches(DS::SBWC, DS::sbTrunnion, DS::sbDragHead, GraphletAnchor::LC, GraphletAnchor::RC, vinset * -2.0F);
			}

			for (DS id = DS::sbTrunnion; id <= DS::sbDragHead; id++) {
				this->master->move_to(this->pressures[id], this->gantries[id], GraphletAnchor::LB, GraphletAnchor::CT, 0.0F, txt_gapsize);
				this->master->move_to(this->labels[id], this->winches[id], GraphletAnchor::RC, GraphletAnchor::LB);
				this->master->move_to(this->Alets[id], this->winches[id], GraphletAnchor::LC, GraphletAnchor::RB, -txt_gapsize);
				this->master->move_to(this->Blets[id], this->winches[id], GraphletAnchor::LC, GraphletAnchor::RT, -txt_gapsize);

				this->reflow_winch_limits(id);

				if (this->rc_speeds.find(id) != this->rc_speeds.end()) {
					this->master->move_to(this->rc_speeds[id], this->winches[id], GraphletAnchor::CB, GraphletAnchor::CT);
					this->master->move_to(this->lengths[id], this->rc_speeds[id], GraphletAnchor::CB, GraphletAnchor::CT);
				} else {
					this->master->move_to(this->lengths[id], this->winches[id], GraphletAnchor::CB, GraphletAnchor::CT);
				}

				this->master->move_to(this->speeds[id], this->lengths[id], GraphletAnchor::CB, GraphletAnchor::CT);
			}
		}

		{ // reflow labels and dimensions
			this->master->move_to(this->labels[DS_side], cx, vinset * 2.0F, GraphletAnchor::CC);
			this->master->move_to(this->labels[DS::Sidelook], this->dragxzes[DS_side], GraphletAnchor::CT, GraphletAnchor::CB, 0.0, -vinset);
			this->master->move_to(this->labels[DS::Overlook], this->dragxys[DS_side], GraphletAnchor::CT, GraphletAnchor::CB, 0.0, -vinset);

			this->master->move_to(this->lengths[DS::TideMark], this->labels[DS::Sidelook], GraphletAnchor::CT, GraphletAnchor::RB, -vinset, -vinset);
			this->master->move_to(this->speeds[DS::Speed], this->labels[DS::Sidelook], GraphletAnchor::CT, GraphletAnchor::LB, +vinset, -vinset);

			if (DS_side == DS::PS) {
				this->master->move_to(this->forces[DS::PSPF2], this->dragxzes[DS_side], GraphletAnchor::RT, GraphletAnchor::CB);
				this->master->move_to(this->forces[DS::PSPF1], this->forces[DS::PSPF2], GraphletAnchor::CT, GraphletAnchor::CB);
			} else {
				this->master->move_to(this->forces[DS::SBPF2], this->dragxzes[DS_side], GraphletAnchor::LT, GraphletAnchor::CB);
				this->master->move_to(this->forces[DS::SBPF1], this->forces[DS::SBPF2], GraphletAnchor::CT, GraphletAnchor::CB);
			}
		}

		{ // reflow gantry indicators
			GraphletAnchor table_anchor = GraphletAnchor::LC;
			Credit<Rectanglet, DS>* table_header = this->table_headers[DS::ps_gantry_settings];
			float xoff = vinset * 0.5F;
			float ygapsize = vinset * 0.5F;
			float yoff = ygapsize;
			float icy = cy * 0.382F;

			if (DS_side == DS::PS) {
				this->master->move_to(table_header, width - vinset, icy, GraphletAnchor::RC);
			} else {
				this->master->move_to(table_header, vinset, icy, GraphletAnchor::LC);
			}

			this->master->move_to(this->labels[table_header->id], table_header, GraphletAnchor::LC, GraphletAnchor::LC, xoff);
			this->master->move_to(this->labels[DS::on], table_header, GraphletAnchor::RC, GraphletAnchor::RC, -xoff);

			for (DS id = DS::tVirtualUp; id <= DS::hVirtualOut; id++) {
				float iheight;

				this->indicators[id]->fill_extent(0.0F, 0.0F, nullptr, &iheight);

				this->master->move_to(this->labels[id], table_header, GraphletAnchor::LB, GraphletAnchor::LT, xoff, yoff);
				this->master->move_to(this->indicators[id], table_header, GraphletAnchor::RB, GraphletAnchor::RT, -xoff, yoff);

				yoff += (iheight + ygapsize);
			}
		}
	}

public:
	void draw_cables(CanvasDrawingSession^ ds, float Width, float Height) {
		ICanvasBrush^ color = Colours::DarkGray;
		float thickness = 2.0F;

		if (this->DS_side == DS::PS) {
			this->draw_cable(ds, DS::psTrunnion, GraphletAnchor::RT, GraphletAnchor::LC, color, thickness);
			this->draw_cable(ds, DS::psIntermediate, GraphletAnchor::RT, GraphletAnchor::LC, color, thickness);
			this->draw_cable(ds, DS::PSWC, DS::psDragHead, GraphletAnchor::RT, GraphletAnchor::LC, color, thickness);
		} else {
			this->draw_cable(ds, DS::sbTrunnion, GraphletAnchor::LT, GraphletAnchor::RC, color, thickness);
			this->draw_cable(ds, DS::sbIntermediate, GraphletAnchor::LT, GraphletAnchor::RC, color, thickness);
			this->draw_cable(ds, DS::SBWC, DS::sbDragHead, GraphletAnchor::LT, GraphletAnchor::RC, color, thickness);
		}
	}

public:
	bool can_select(IGraphlet* g) override {
		bool select_settings = false;
		auto settings = dynamic_cast<Credit<Rectanglet, DS>*>(g);

		if (settings != nullptr) {
			select_settings = (this->indicators.find(settings->id) != this->indicators.end());
		}

		return (select_settings
			|| (dynamic_cast<Winchlet*>(g) != nullptr) 
			|| (dynamic_cast<Gantrylet*>(g) != nullptr)
			|| (dynamic_cast<Compensatorlet*>(g) != nullptr)
			|| (dynamic_cast<DragHeadlet*>(g) != nullptr));
	}

	bool can_select_multiple() override {
		return true;
	}

	void on_tap_selected(IGraphlet* g, float local_x, float local_y) override {
		auto winch = dynamic_cast<Winchlet*>(g);
		auto gantry = dynamic_cast<Gantrylet*>(g);
		auto compensator = dynamic_cast<Compensatorlet*>(g);
		auto visor = dynamic_cast<Credit<DragHeadlet, DS>*>(g);
		auto indicator = dynamic_cast<Credit<Rectanglet, DS>*>(g);

		if (winch != nullptr) {
			menu_popup(this->winch_op, g, local_x, local_y);
		} else if (gantry != nullptr) {
			menu_popup(this->gantry_op, g, local_x, local_y);
		} else if (compensator != nullptr) {
			menu_popup(this->compensator_op, g, local_x, local_y);
		} else if (visor != nullptr) {
			menu_popup(this->visor_op, g, local_x, local_y);
		} else if (indicator != nullptr) {
			this->master->get_plc_device()->send_command(DO_gantry_virtual_action_command(indicator->id, (DS_side == DS::PS)));
		}
	}

	void on_gesture(std::list<float2>& anchors, float x, float y) {
		if (this->DS_side == DS::PS) {
			if (this->gantries_selected(DS::psTrunnion, DS::psDragHead, 2)) {
				group_menu_popup(this->ggantries_op, this->master, x, y);
			}
		} else {
			if (this->gantries_selected(DS::sbTrunnion, DS::sbDragHead, 2)) {
				group_menu_popup(this->ggantries_op, this->master, x, y);
			}
		}
	}

private:
	template<class C, typename E>
	void load_gantries(std::map<E, Credit<C, E>*>& cs, E id0, E idn, float radius) {
		for (E id = id0; id <= idn; id++) {
			cs[id] = this->master->insert_one(new Credit<C, E>(radius), id);
		}
	}

	template<class C, typename E>
	void load_detailed_winches(std::map<E, Credit<C, E>*>& ws, E id0, E idn, float radius) {
		this->load_winches(ws, id0, idn, radius);

		for (E id = id0; id <= idn; id++) {
			if (id != id0) {
				this->load_percentage(this->rc_speeds, id);
			}
		}
	}

	template<typename E>
	void load_dimensions(std::map<E, Credit<Dimensionlet, E>*>& ds, E id0, E idn, E lbl0, Platform::String^ unit) {
		Platform::String^ A = _speak("Alet");
		Platform::String^ B = _speak("Blet");
		E label = lbl0;

		for (E id = id0; id <= idn; id++, label++) {
			this->Alets[id] = this->master->insert_one(new Credit<Dimensionlet, E>(this->pump_style, unit, A), id);
			this->Blets[id] = this->master->insert_one(new Credit<Dimensionlet, E>(this->pump_style, unit, B), id);

			ds[id] = this->master->insert_one(new Credit<Dimensionlet, E>(unit, label.ToString()), id);

			ds[id]->set_style(DimensionStatus::Normal, this->pump_style);
			ds[id]->set_style(DimensionStatus::Highlight, this->highlight_style);
		}
	}

	template<typename E>
	void load_gantry_indicators(E id0, E idn, float size, std::map<E, Credit<Rectanglet, E>*>& bs, std::map<E, Credit<Labellet, E>*>& ls) {
		for (E id = id0; id <= idn; id++) {
			this->load_label(ls, id, Colours::Silver);
			bs[id] = this->master->insert_one(new Credit<Rectanglet, E>(size, unchecked_color), id);
		}
	}
	
	template<class S, typename E>
	void load_table_header(std::map<E, Credit<S, E>*>& ths, E id, float width, float height, ICanvasBrush^ color) {
		ths[id] = this->master->insert_one(new Credit<S, E>(width, height, color), id);
		this->load_label(this->labels, id, Colours::Background);
	}

private:
	template<typename E>
	void reflow_winches(E wc, E trunnion, E draghead, GraphletAnchor ga, GraphletAnchor wa, float gapsize) {
		float compensator_width, compensator_height;
		
		this->compensators[wc]->fill_extent(0.0F, 0.0F, &compensator_width, &compensator_height);
		
		{ // align gantries and winches
			float gw_gap = gapsize * 2.0F + compensator_width * ((gapsize > 0.0F) ? 1.0F : -1.0F);
			float gantry_joint_dy = 0.0F;

			for (E id = trunnion; id <= draghead; id++) {
				float gantry_height = 0.0F;
				
				this->gantries[id]->fill_extent(0.0F, 0.0F, nullptr, &gantry_height);
				gantry_joint_dy = this->gantries[id]->get_winch_joint_y() - gantry_height * 0.5F;
				this->master->move_to(this->winches[id], this->gantries[id], ga, wa, gw_gap, gantry_joint_dy);
			}

			{ // align the draghead gantry and the wave compensator
				float wc_joint_dy = this->compensators[wc]->get_cable_joint_y() - compensator_height * 0.5F;
				
				this->master->move_to(this->compensators[wc], this->gantries[draghead], ga, wa,gapsize, gantry_joint_dy - wc_joint_dy);
				this->master->move_to(this->lengths[wc], this->compensators[wc], GraphletAnchor::CB, GraphletAnchor::CT);
				this->master->move_to(this->pressures[wc], this->lengths[wc], GraphletAnchor::CB, GraphletAnchor::CT);
			}
		}
	}

	template<typename E>
	void draw_cable(CanvasDrawingSession^ ds, E id, GraphletAnchor ga, GraphletAnchor wa, ICanvasBrush^ color, float thickness) {
		float gantry_x, gantry_y, winch_x, winch_y;
		float gantry_joint = this->gantries[id]->get_winch_joint_y();

		this->master->fill_graphlet_location(this->gantries[id], &gantry_x, &gantry_y, ga);
		this->master->fill_graphlet_location(this->winches[id], &winch_x, &winch_y, wa);

		ds->DrawLine(gantry_x, gantry_y + gantry_joint, winch_x, winch_y, color, thickness);
	}

	template<typename E>
	void draw_cable(CanvasDrawingSession^ ds, E wc, E id, GraphletAnchor ga, GraphletAnchor wa, ICanvasBrush^ color, float thickness) {
		float gantry_x, gantry_y, winch_x, winch_y, wc_gx, wc_wx, wc_y;
		float gantry_joint = this->gantries[id]->get_winch_joint_y();
		float wc_joint = this->compensators[wc]->get_cable_joint_y();
		float wc_adjust = this->compensators[wc]->get_cable_joint_size();
		float sign = ((wc == DS::PSWC) ? 1.0F : -1.0F);

		this->master->fill_graphlet_location(this->gantries[id], &gantry_x, &gantry_y, ga);
		this->master->fill_graphlet_location(this->winches[id], &winch_x, &winch_y, wa);
		this->master->fill_graphlet_location(this->compensators[wc], &wc_wx, &wc_y, ga);
		this->master->fill_graphlet_location(this->compensators[wc], &wc_gx, nullptr, wa);

		ds->DrawLine(gantry_x, gantry_y + gantry_joint, wc_gx + wc_adjust * sign, wc_y + wc_joint, color, thickness);
		ds->DrawLine(wc_wx - wc_adjust * sign, wc_y + wc_joint, winch_x, winch_y, color, thickness);
	}

private:
	template<typename E>
	void set_gantry_virtual_action_status(E id, const uint8* db205, unsigned int idx_p1) {
		if (DBX(db205, idx_p1 - 1U)) {
			this->indicators[id]->set_color(checked_color);
		} else {
			this->indicators[id]->set_color(unchecked_color);
		}
	}

private:
	bool gantries_selected(DS id0, DS idn, int tolerance) {
		bool okay = false;
		int ok = 0;

		for (DS id = id0; id <= idn; id++) {
			if (this->master->is_selected(this->gantries[id])) {
				ok += 1;

				if (ok >= tolerance) {
					okay = true;
					break;
				}
			}
		}

		return okay;
	}

private: // never delete these graphlets manually.
	std::map<DS, Credit<Gantrylet, DS>*> gantries;
	std::map<DS, Credit<Gantrylet, DS>*> lines;
	std::map<DS, Credit<Dimensionlet, DS>*> Alets;
	std::map<DS, Credit<Dimensionlet, DS>*> Blets;
	std::map<DS, Credit<Percentagelet, DS>*> rc_speeds;
	std::map<DS, Credit<Rectanglet, DS>*> indicators;
	std::map<DS, Credit<Rectanglet, DS>*> table_headers;

private:
	DimensionStyle pump_style;
	DimensionStyle highlight_style;
	DimensionStyle setting_style;

private:
	MenuFlyout^ ggantries_op;
	MenuFlyout^ gantry_op;
	MenuFlyout^ winch_op;
	MenuFlyout^ compensator_op;
	MenuFlyout^ visor_op;

private:
	DS DS_side;
	DredgeAddress* address; // global object
	unsigned int drag_color;
	unsigned int drag_idx;
	float sign;
};

private class DragCableDecorator : public IPlanetDecorator {
public:
	DragCableDecorator(IDredgingSystem* master) : master(master) {}

public:
	void draw_before(CanvasDrawingSession^ ds, float Width, float Height) override {
		this->master->draw_cables(ds, Width, Height);
	}

private:
	IDredgingSystem* master;
};

/*************************************************************************************************/
DredgesPage::DredgesPage(PLCMaster* plc, DragView type)
	: Planet(__MODULE__ + ((type == DragView::_) ? "" : "_" + type.ToString()))
	, device(plc) {
	IDredgingSystem* dashboard = nullptr;
	
	switch (type) {
	case DragView::Left: dashboard = new Drags(this, DS::PS, default_ps_color, 0); break;
	case DragView::Right: dashboard = new Drags(this, DS::SB, default_sb_color, 1); break;
	default: dashboard = new Dredges(this); break;
	}

	this->dashboard = dashboard;
	this->device->append_confirmation_receiver(dashboard);

	this->append_decorator(new PageDecorator());
	this->append_decorator(new DragCableDecorator(dashboard));
}

DredgesPage::~DredgesPage() {
	if (this->dashboard != nullptr) {
		delete this->dashboard;
	}
}

void DredgesPage::load(CanvasCreateResourcesReason reason, float width, float height) {
	auto db = dynamic_cast<IDredgingSystem*>(this->dashboard);
	
	if (db != nullptr) {
		{ // load graphlets
			this->change_mode(DSMode::Dashboard);
			db->load(width, height, statusbar_height());

			this->change_mode(DSMode::WindowUI);
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

void DredgesPage::reflow(float width, float height) {
	auto db = dynamic_cast<IDredgingSystem*>(this->dashboard);
	
	if (db != nullptr) {
		this->change_mode(DSMode::WindowUI);
		this->move_to(this->statusline, 0.0F, height, GraphletAnchor::LB);
		
		this->change_mode(DSMode::Dashboard);
		db->reflow(width, height, statusbar_height());
	}
}

PLCMaster* DredgesPage::get_plc_device() {
	return this->device;
}

bool DredgesPage::can_select(IGraphlet* g) {
	auto db = dynamic_cast<IDredgingSystem*>(this->dashboard);
	
	return ((db != nullptr) && (db->can_select(g)));
}

bool DredgesPage::can_select_multiple() {
	auto db = dynamic_cast<IDredgingSystem*>(this->dashboard);

	return ((db != nullptr) && (db->can_select_multiple()));
}

void DredgesPage::on_tap_selected(IGraphlet* g, float local_x, float local_y) {
	auto db = dynamic_cast<IDredgingSystem*>(this->dashboard);

	if (db != nullptr) {
		db->on_tap_selected(g, local_x, local_y);
	}
}

void DredgesPage::on_gesture(std::list<float2>& anchors, float x, float y) {
	auto db = dynamic_cast<IDredgingSystem*>(this->dashboard);

	if (db != nullptr) {
		db->on_gesture(anchors, x, y);
	}
}
