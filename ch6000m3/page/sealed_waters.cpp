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
#include "graphlet/symbol/valve/manual_valvelet.hpp"

#include "schema/ai_pumps.hpp"
#include "schema/ai_hopper_pumps.hpp"

#include "schema/di_pumps.hpp"
#include "schema/di_hopper_pumps.hpp"

#include "schema/do_pumps.hpp"

#include "decorator/page.hpp"

using namespace WarGrey::SCADA;
using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::System;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

private enum SWMode { WindowUI = 0, Dashboard };

private enum class SWPOperation { Start, Stop, Reset, Auto, _ };
private enum class SWVOperation { Open, Close, VirtualOpen, VirtualClose, _ };

static CanvasSolidColorBrush^ water_color = Colours::Green;

// WARNING: order matters
private enum class SW : unsigned int {
	// Pumps
	PSHP, SBHP, PSUWP, SBUWP, PSFP, SBFP,
	PSHPa, PSHPb, SBHPa, SBHPb, PSUWP1, PSUWP2, SBUWP1, SBUWP2,

	// Manual Valves
	DGV3, DGV4, DGV5, DGV6, DGV1, DGV2, DGV9, DGV10,
	DGV12, DGV11, DGV13, DGV14, DGV15, DGV16, DGV17, DGV18, DGV19, DGV20,
	DGV7, DGV44, DGV45, DGV8,
	
	// Labels
	ToFlushs, SS1, SS2, Hatch, Sea,
	
	_,
	
	// anchors used as last jumping points
	d3, d4, d5, d6,
	d12, d11, d13, d14, d15, d16, d17, d18, d19, d20,
	d44, d45, d46, d47,

	// anchors used for unnamed corners
	flushs, pshp, sbhp, psuwp, sbuwp
};

private class SealedWaters final
	: public PLCConfirmation
	, public IMenuCommand<SWPOperation, Credit<HydraulicPumplet, SW>, PLCMaster*> {
public:
	SealedWaters(SealedWaterPage* master) : master(master), sea_oscillation(1.0F) {
		this->label_font = make_bold_text_format("Microsoft YaHei", small_font_size);
		this->dimension_style = make_highlight_dimension_style(large_metrics_font_size, 6U);
		this->setting_style = make_highlight_dimension_style(large_metrics_font_size, 6U, Colours::GhostWhite, Colours::RoyalBlue);
	}

public:
	void pre_read_data(Syslog* logger) override {
		this->master->enter_critical_section();
		this->master->begin_update_sequence();

		this->station->clear_subtacks();
	}

	void on_analog_input(const uint8* DB203, size_t count, Syslog* logger) override {
		this->pressures[SW::PSFP]->set_value(RealData(DB203, ps_flushing_pump_pressure), GraphletAnchor::LB);

		this->pressures[SW::PSHP]->set_value(RealData(DB203, ps_hopper_pump_sealed_water_pressure), GraphletAnchor::LB);
		this->flows[SW::PSHP]->set_value(RealData(DB203, ps_hopper_pump_sealed_water_flow), GraphletAnchor::LT);
		this->powers[SW::PSHP]->set_value(RealData(DB203, ps_hopper_pump_power), GraphletAnchor::LB);
		this->rpms[SW::PSHP]->set_value(RealData(DB203, ps_hopper_pump_rpm), GraphletAnchor::LB);

		this->pressures[SW::SBHP]->set_value(RealData(DB203, sb_hopper_pump_sealed_water_pressure), GraphletAnchor::LB);
		this->flows[SW::SBHP]->set_value(RealData(DB203, sb_hopper_pump_sealed_water_flow), GraphletAnchor::LB);
		this->powers[SW::SBHP]->set_value(RealData(DB203, sb_hopper_pump_power), GraphletAnchor::LB);
		this->rpms[SW::SBHP]->set_value(RealData(DB203, sb_hopper_pump_rpm), GraphletAnchor::LB);

		this->pressures[SW::PSUWP1]->set_value(RealData(DB203, ps_underwater_pump1_sealed_water_pressure), GraphletAnchor::LB);
		this->pressures[SW::PSUWP2]->set_value(RealData(DB203, ps_underwater_pump2_sealed_water_pressure), GraphletAnchor::LB);
		
		this->pressures[SW::SBUWP1]->set_value(RealData(DB203, sb_underwater_pump1_sealed_water_pressure), GraphletAnchor::LB);
		this->pressures[SW::SBUWP2]->set_value(RealData(DB203, sb_underwater_pump2_sealed_water_pressure), GraphletAnchor::LB);
	}

	void on_digital_input(const uint8* DB4, size_t count4, const uint8* DB205, size_t count25, WarGrey::SCADA::Syslog* logger) override {
		DI_hopper_pumps(this->hoppers[SW::PSHP], this->hoppers[SW::PSUWP], DB4, ps_hopper_pump_feedback, DB205, ps_hopper_pump_details, ps_underwater_pump_details);
		DI_hopper_pumps(this->hoppers[SW::SBHP], this->hoppers[SW::SBUWP], DB4, sb_hopper_pump_feedback, DB205, sb_hopper_pump_details, sb_underwater_pump_details);

		DI_gate_flushing_pump(this->pumps[SW::PSFP], DB4, pump_ps_gate_flushing_feedback, DB205, pump_ps_gate_flushing_status);
		DI_gate_flushing_pump(this->pumps[SW::SBFP], DB4, pump_sb_gate_flushing_feedback, DB205, pump_sb_gate_flushing_status);

		DI_sealed_water_pump(this->pumps[SW::PSHPa], false, DB4, pump_ps_hopper_A_feedback, DB205, pump_ps_hopper_A_status);
		DI_sealed_water_pump(this->pumps[SW::PSHPb], false, DB4, pump_ps_hopper_B_feedback, DB205, pump_ps_hopper_B_status);
		DI_sealed_water_pump(this->pumps[SW::SBHPa], false, DB4, pump_sb_hopper_A_feedback, DB205, pump_sb_hopper_A_status);
		DI_sealed_water_pump(this->pumps[SW::SBHPb], false, DB4, pump_sb_hopper_B_feedback, DB205, pump_sb_hopper_B_status);

		DI_sealed_water_pump(this->pumps[SW::PSUWP1], true, DB4, pump_ps_underwater_1_feedback, DB205, pump_ps_underwater_1_feedback);
		DI_sealed_water_pump(this->pumps[SW::PSUWP2], true, DB4, pump_ps_underwater_2_feedback, DB205, pump_ps_underwater_2_feedback);
		DI_sealed_water_pump(this->pumps[SW::SBUWP1], true, DB4, pump_sb_underwater_1_feedback, DB205, pump_sb_underwater_1_feedback);
		DI_sealed_water_pump(this->pumps[SW::SBUWP2], true, DB4, pump_sb_underwater_2_feedback, DB205, pump_sb_underwater_2_feedback);
	}

	void post_read_data(Syslog* logger) override {
		SW ps_hopper_long_path[] = { SW::DGV13, SW::pshp, SW::d44, SW::DGV8, SW::PSHP };
		SW ps_hopper_short_path[] = { SW::DGV14, SW::d44, SW::DGV8, SW::PSHP };
		SW sb_hopper_short_path[] = { SW::DGV15, SW::d45, SW::DGV7, SW::SBHP };
		SW sb_hopper_long_path[] = { SW::DGV16, SW::sbhp, SW::d45, SW::DGV7, SW::SBHP };
		SW ps_underwater_path[] = { SW::DGV17, SW::psuwp, SW::d46, SW::PSUWP };
		SW sb_underwater_path[] = { SW::DGV20, SW::sbuwp, SW::d47, SW::SBUWP };

		this->station->append_subtrack(SW::Hatch, SW::DGV20, water_color);

		this->try_flow_water(SW::PSFP, SW::DGV12, SW::flushs, water_color);
		this->try_flow_water(SW::SBFP, SW::DGV11, SW::flushs, water_color);
		this->try_flow_water(SW::PSHPa, ps_hopper_long_path, water_color);
		this->try_flow_water(SW::PSHPb, ps_hopper_short_path, water_color);
		this->try_flow_water(SW::SBHPa, sb_hopper_short_path, water_color);
		this->try_flow_water(SW::SBHPb, sb_hopper_long_path, water_color);

		this->try_flow_water(SW::PSUWP1, ps_underwater_path, water_color);
		this->try_flow_water(SW::PSUWP2, SW::DGV18, SW::PSUWP, water_color);
		this->try_flow_water(SW::SBUWP1, SW::DGV19, SW::SBUWP, water_color);
		this->try_flow_water(SW::SBUWP2, sb_underwater_path, water_color);

		this->master->end_update_sequence();
		this->master->leave_critical_section();
	}

public:
	bool can_execute(SWPOperation cmd, Credit<HydraulicPumplet, SW>* pump, PLCMaster* plc, bool acc_executable) override {
		bool executable = plc->connected();

		switch (pump->id) {
		case SW::PSFP: case SW::SBFP: executable = executable && gate_flushing_pump_command_executable(pump, cmd, true); break;
		default: executable = executable && sealed_water_pump_command_executable(pump, cmd, true); break;
		}

		return executable;
	}

	void execute(SWPOperation cmd, Credit<HydraulicPumplet, SW>* pump, PLCMaster* plc) {
		switch (pump->id) {
		case SW::PSFP: case SW::SBFP: plc->send_command(DO_gate_flushing_pump_command(cmd, pump->id)); break;
		default: plc->send_command(DO_sealed_water_pump_command(cmd, pump->id)); break;
		}
	}

public:
	bool on_char(VirtualKey key, PLCMaster* plc) {
		bool handled = false;

		if (key == VirtualKey::Enter) {
			auto editor = dynamic_cast<Credit<Dimensionlet, SW>*>(this->master->get_focus_graphlet());

			if (editor != nullptr) {
				plc->get_logger()->log_message(Log::Info, L"%s: %lf",
					editor->id.ToString()->Data(),
					editor->get_input_number());

				editor->set_value(editor->get_input_number());
			}

			handled = true;
		}

		return handled;
	}

public:
	void load(float width, float height, float gwidth, float gheight) {
		Turtle<SW>* pTurtle = new Turtle<SW>(gwidth, gheight, false, SW::Hatch);

		pTurtle->move_right(3);
		pTurtle->move_down(1, SW::d12)->move_right(5, SW::DGV12)->move_right(6, SW::PSFP);
		pTurtle->move_right(10)->move_down(2)->jump_back();
		pTurtle->move_down(4, SW::d11)->move_right(5, SW::DGV11)->move_right(6, SW::SBFP);
		pTurtle->move_right(10)->move_up(2)->move_right(4, SW::ToFlushs)->move_right(4, SW::flushs)->jump_back(SW::d11);

		pTurtle->move_down(4, SW::d13)->move_right(5, SW::DGV13)->move_right(6, SW::PSHPa)->move_right(6, SW::DGV3);
		pTurtle->move_right(11)->turn_right_down(SW::pshp)->move_down(3)->turn_down_right()->jump_back(SW::d13);
		pTurtle->move_down(4, SW::d14)->move_right(5, SW::DGV14)->move_right(6, SW::PSHPb)->move_right(6, SW::DGV4);
		pTurtle->move_right(14, SW::d44)->move_right(9, SW::DGV44)->move_right(12, SW::PSHP)->jump_back();
		pTurtle->move_up_right(2.5F, SW::SS1)->move_up_right(2.5F)->move_right(4, SW::DGV8);
		pTurtle->move_right(12)->move_down(5)->jump_back(SW::d14);

		pTurtle->move_down(5, SW::d15)->move_right(5, SW::DGV15)->move_right(6, SW::SBHPa)->move_right(6, SW::DGV5);
		pTurtle->move_right(14, SW::d45)->move_right(9, SW::DGV45)->move_right(12, SW::SBHP)->jump_back();
		pTurtle->move_down_right(2.5F, SW::SS2)->move_down_right(2.5F)->move_right(4, SW::DGV7);
		pTurtle->move_right(12)->move_up(5)->jump_back(SW::d15);
		pTurtle->move_down(4, SW::d16)->move_right(5, SW::DGV16)->move_right(6, SW::SBHPb)->move_right(6, SW::DGV6);
		pTurtle->move_right(11)->turn_right_up(SW::sbhp)->move_up(3)->turn_up_right()->jump_back(SW::d16);

		pTurtle->jump_down(3)->jump_left(SW::Sea)->move_right();

		pTurtle->move_down(2, SW::d17)->move_right(5, SW::DGV17)->move_right(6, SW::PSUWP1)->move_right(6, SW::DGV1);
		pTurtle->move_right(11)->turn_right_down(SW::psuwp)->move_down(3)->turn_down_right()->jump_back(SW::d17);
		pTurtle->move_down(4, SW::d18)->move_right(5, SW::DGV18)->move_right(6, SW::PSUWP2)->move_right(6, SW::DGV2);
		pTurtle->move_right(14, SW::d46)->move_right(9)->move_right(12, SW::PSUWP)->jump_back(SW::d18);

		pTurtle->move_down(5, SW::d19)->move_right(5, SW::DGV19)->move_right(6, SW::SBUWP1)->move_right(6, SW::DGV9);
		pTurtle->move_right(14, SW::d47)->move_right(9)->move_right(12, SW::SBUWP)->jump_back(SW::d19);
		pTurtle->move_down(4, SW::d20)->move_right(5, SW::DGV20)->move_right(6, SW::SBUWP2)->move_right(6, SW::DGV10);
		pTurtle->move_right(11)->turn_right_up(SW::sbuwp)->move_up(3)->turn_up_right();

		this->station = this->master->insert_one(new Tracklet<SW>(pTurtle, default_pipe_thickness, default_pipe_color));
		this->to_flushs = this->master->insert_one(new ArrowHeadlet(gheight, 0.0, Colours::Silver));
		this->sea = this->master->insert_one(new VLinelet(gheight * 2.0F, default_pipe_thickness,
			water_color, make_dash_stroke(CanvasDashStyle::Dash)));
		
		{ // load devices
			float radius = resolve_gridsize(gwidth, gheight);
			float hpradius = std::fminf(gwidth, gheight);

			this->hatch = this->master->insert_one(new Hatchlet(hpradius * 2.5F));

			this->load_devices(this->mvalves, this->labels, Colours::Silver, SW::DGV3, SW::DGV8, radius, 0.0);
			this->load_devices(this->pumps, this->labels, Colours::Salmon, SW::PSFP, SW::SBFP, radius, 0.0);
			this->load_devices(this->pumps, this->labels, Colours::Salmon, SW::PSHPa, SW::SBHPb, radius, 0.0);
			this->load_devices(this->pumps, this->labels, Colours::Salmon, SW::PSUWP1, SW::SBUWP2, radius, 0.0);
			
			this->load_device(this->hoppers, this->captions, SW::PSHP, hpradius, -2.0F, true);
			this->load_device(this->hoppers, this->captions, SW::SBHP, hpradius, +2.0F, true);
			this->load_device(this->hoppers, this->captions, SW::PSUWP, hpradius, +2.0F, false);
			this->load_device(this->hoppers, this->captions, SW::SBUWP, hpradius, -2.0F, false);
		}

		{ // load labels and dimensions
			this->load_labels(this->captions, SW::ToFlushs, SW::SS2, Colours::Silver);
			this->load_labels(this->captions, SW::Hatch, SW::Sea, Colours::Salmon);

			this->load_dimension(this->pressures, SW::PSFP, "bar", "P");
			this->load_dimensions(this->pressures, SW::PSUWP1, SW::SBUWP2, "bar", "P");
			this->load_dimensions(this->pressures, SW::PSHP, SW::SBHP, "bar", "P");
			this->load_dimensions(this->flows, SW::PSHP, SW::SBHP, "m3ph", "F");

			this->load_settings(this->settings, SW::PSHPa, SW::SBHPb, "rpm", "S");
		}
	}

public:
	void reflow(float width, float height, float gwidth, float gheight, float vinset) {
		float toff = default_pipe_thickness * 2.0F;
		
		this->master->move_to(this->station, width * 0.5F, height * 0.5F, GraphletAnchor::CC, -gwidth * 2.0F);
		this->station->map_credit_graphlet(this->captions[SW::Sea], GraphletAnchor::RC, -toff);
		this->station->map_graphlet_at_anchor(this->sea, SW::Sea, GraphletAnchor::LC, 0.0F, this->sea_oscillation);
		this->station->map_graphlet_at_anchor(this->hatch, SW::Hatch, GraphletAnchor::LC);
		this->master->move_to(this->captions[SW::Hatch], this->hatch, GraphletAnchor::CB, GraphletAnchor::CT);

		this->station->map_graphlet_at_anchor(this->to_flushs, SW::flushs, GraphletAnchor::CC);
		this->station->map_credit_graphlet(this->captions[SW::ToFlushs], GraphletAnchor::CB);
		
		this->station->map_credit_graphlet(this->captions[SW::SS1], GraphletAnchor::LC);
		this->station->map_credit_graphlet(this->captions[SW::SS2], GraphletAnchor::LC);

		{ // reflow devices and metrics
			float gridsize = resolve_gridsize(gwidth, gheight);
			float xoff = gwidth * 3.0F;
			
			this->reflow_valves(this->mvalves, this->labels, gridsize);
			
			for (auto it = this->pumps.begin(); it != this->pumps.end(); it++) {
				this->station->map_credit_graphlet(it->second, GraphletAnchor::CC);
				this->master->move_to(this->labels[it->first], it->second, GraphletAnchor::CT, GraphletAnchor::CB);

				switch (it->first) {
				case SW::PSFP: {
					this->station->map_credit_graphlet(this->pressures[it->first], GraphletAnchor::LT, xoff, gridsize);
				}; break;
				case SW::PSUWP1: case SW::PSUWP2: case SW::SBUWP1: case SW::SBUWP2: {
					this->station->map_credit_graphlet(this->pressures[it->first], GraphletAnchor::LB, gwidth * 9.0F, -toff);
				}; break;
				}
			}

			for (auto it = this->hoppers.begin(); it != this->hoppers.end(); it++) {
				SW tanchor = SW::_;

				this->station->map_credit_graphlet(it->second, GraphletAnchor::LC);
				this->master->move_to(this->captions[it->first], it->second, GraphletAnchor::RC, GraphletAnchor::LC, toff);

				if (this->rpms.find(it->first) != this->rpms.end()) {
					this->master->move_to(this->rpms[it->first], it->second, GraphletAnchor::LC, GraphletAnchor::RB, -gwidth * 0.5F, -toff);
					this->master->move_to(this->powers[it->first], this->rpms[it->first], GraphletAnchor::LB, GraphletAnchor::LT, 0.0, toff * 2.0F);
				}
				
				switch (it->first) {
				case SW::PSHP: tanchor = SW::DGV8; break;
				case SW::SBHP: tanchor = SW::DGV7; break;
				}

				if (tanchor != SW::_) {
					this->station->map_graphlet_at_anchor(this->pressures[it->first], tanchor, GraphletAnchor::LB, xoff, -toff);
					this->station->map_graphlet_at_anchor(this->flows[it->first], tanchor, GraphletAnchor::LT, xoff, toff);
				}
			}

			for (auto it = this->settings.begin(); it != this->settings.end(); it++) {
				this->station->map_credit_graphlet(it->second, GraphletAnchor::LB, xoff * 3.0F, -toff);
			}
		}
	}

public:
	void update(long long count, long long interval, long long uptime) {
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
	void load_device(std::map<E, G*>& gs, std::map<E, Credit<Labellet, E>*>& ls, E id, float rx, float fy, bool has_dimensions) {
		this->load_label(ls, id, Colours::Salmon);

		gs[id] = this->master->insert_one(new G(rx, std::fabsf(rx) * fy), id);

		if (has_dimensions) {
			this->load_dimension(this->rpms, id, "rpm", "S");
			this->load_dimension(this->powers, id, "kwatt", "P");
		}
	}

	template<typename E>
	void load_settings(std::map<E, Credit<Dimensionlet, E>*>& ds, E id0, E idn, Platform::String^ unit, Platform::String^ label) {
		for (E id = id0; id <= idn; id++) {
			ds[id] = this->master->insert_one(new Credit<Dimensionlet, E>(DimensionStatus::Input, this->setting_style, unit, label), id);
			ds[id]->set_maximum(double(sealed_water_pump_rpm_range));
		}
	}

	template<typename E>
	void load_dimension(std::map<E, Credit<Dimensionlet, E>*>& ds, E id, Platform::String^ unit, Platform::String^label) {
		ds[id] = this->master->insert_one(new Credit<Dimensionlet, E>(this->dimension_style, unit, label), id);
	}

	template<typename E>
	void load_dimensions(std::map<E, Credit<Dimensionlet, E>*>& ds, E id0, E idn, Platform::String^ unit, Platform::String^label) {
		for (E id = id0; id <= idn; id++) {
			this->load_dimension(ds, id, unit, label);
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

private:
	void try_flow_water(SW pid, SW start, SW end, CanvasSolidColorBrush^ color) {
		switch (this->pumps[pid]->get_status()) {
		case HydraulicPumpStatus::Running: case HydraulicPumpStatus::StopReady: {
			this->station->append_subtrack(start, end, color);
		}
		}
	}

	void try_flow_water(SW pid, SW* path, unsigned int count, CanvasSolidColorBrush^ color) {
		switch (this->pumps[pid]->get_status()) {
		case HydraulicPumpStatus::Running: case HydraulicPumpStatus::StopReady: {
			this->station->append_subtrack(path, count, color);
		}
		}
	}

	template<unsigned int N>
	void try_flow_water(SW pid, SW (&path)[N], CanvasSolidColorBrush^ color) {
		this->try_flow_water(pid, path, N, color);
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
	std::map<SW, Credit<Dimensionlet, SW>*> pressures;
	std::map<SW, Credit<Dimensionlet, SW>*> flows;
	std::map<SW, Credit<Dimensionlet, SW>*> rpms;
	std::map<SW, Credit<Dimensionlet, SW>*> powers;
	std::map<SW, Credit<Dimensionlet, SW>*> settings;

private:
	CanvasTextFormat^ label_font;
	DimensionStyle dimension_style;
	DimensionStyle setting_style;
	float sea_oscillation;

private:
	SealedWaterPage* master;
};

SealedWaterPage::SealedWaterPage(PLCMaster* plc) : Planet(__MODULE__), device(plc) {
	SealedWaters* dashboard = new SealedWaters(this);

	this->dashboard = dashboard;
	this->pump_op = make_menu<SWPOperation, Credit<HydraulicPumplet, SW>, PLCMaster*>(dashboard, plc);
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
			this->statusbar = this->insert_one(new Statusbarlet(this->name(), this->device));
			this->statusline = this->insert_one(new Statuslinelet(default_logging_level));
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

void SealedWaterPage::update(long long count, long long interval, long long uptime) {
	auto dashboard = dynamic_cast<SealedWaters*>(this->dashboard);

	if (dashboard != nullptr) {
		dashboard->update(count, interval, uptime);
	}
}


bool SealedWaterPage::can_select(IGraphlet* g) {
	return ((dynamic_cast<HydraulicPumplet*>(g) != nullptr));
}

bool SealedWaterPage::on_char(VirtualKey key, bool wargrey_keyboard) {
	bool handled = Planet::on_char(key, wargrey_keyboard);

	if (!handled) {
		auto db = dynamic_cast<SealedWaters*>(this->dashboard);

		if (db != nullptr) {
			handled = db->on_char(key, this->device);
		}
	}

	return handled;
}

void SealedWaterPage::on_focus(IGraphlet* g) {
	auto editor = dynamic_cast<IEditorlet*>(g);

	if (editor != nullptr) {
		this->show_virtual_keyboard(ScreenKeyboard::Numpad);
	}
}

void SealedWaterPage::on_tap_selected(IGraphlet* g, float local_x, float local_y) {
	auto pump = dynamic_cast<HydraulicPumplet*>(g);
	
	if (pump != nullptr) {
		menu_popup(this->pump_op, g, local_x, local_y);
	}
}
