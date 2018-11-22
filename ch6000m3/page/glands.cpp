#include <map>

#include "page/glands.hpp"
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

#include "iotables/ai_pumps.hpp"
#include "iotables/ai_hopper_pumps.hpp"

#include "iotables/di_pumps.hpp"
#include "iotables/di_hopper_pumps.hpp"

#include "iotables/do_hopper_pumps.hpp"
#include "iotables/ao_gland_pumps.hpp"

#include "decorator/page.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::System;
using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

private enum GPMode { WindowUI = 0, Dashboard };

private enum class GPOperation { Start, Stop, Reset, Auto, _ };

static CanvasSolidColorBrush^ water_color = Colours::Green;

// WARNING: order matters
private enum class GP : unsigned int {
	// Pumps
	PSHP, SBHP, PSUWP, SBUWP, PSFP, SBFP,
	PSHPa, PSHPb, SBHPa, SBHPb, PSUWP1, PSUWP2, SBUWP1, SBUWP2,

	// Manual Valves
	DGV3, DGV4, DGV5, DGV6,
	DGV12, DGV11, DGV13, DGV14, DGV15, DGV16,
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

static uint16 DO_glands_action(GlandPumpAction cmd, HydraulicPumplet* pump) {
	auto credit_pump = dynamic_cast<Credit<HydraulicPumplet, GP>*>(pump);
	uint16 index = 0U;
	
	if (credit_pump != nullptr) {
		switch (credit_pump->id) {
		case GP::PSFP: case GP::SBFP: index = DO_gate_flushing_pump_command(cmd, credit_pump->id); break;
		default: index = DO_gland_pump_command(cmd, credit_pump->id); break;
		}
	}

	return index;
}

/*************************************************************************************************/
private class GlandPumps final : public PLCConfirmation {
public:
	GlandPumps(GlandsPage* master) : master(master), sea_oscillation(1.0F) {
		this->label_font = make_bold_text_format("Microsoft YaHei", small_font_size);
		this->dimension_style = make_highlight_dimension_style(large_metrics_font_size, 6U, 0);
		this->setting_style = make_highlight_dimension_style(large_metrics_font_size, 6U, 0, Colours::GhostWhite, Colours::RoyalBlue);
	}

public:
	void pre_read_data(Syslog* logger) override {
		this->master->enter_critical_section();
		this->master->begin_update_sequence();

		this->station->clear_subtacks();
	}

	void on_analog_input(const uint8* DB2, size_t count2, const uint8* DB203, size_t count203, Syslog* logger) override {
		this->pressures[GP::PSFP]->set_value(RealData(DB203, ps_flushing_pump_pressure), GraphletAnchor::LB);

		this->pressures[GP::PSHP]->set_value(RealData(DB203, ps_hopper_gland_pump_pressure), GraphletAnchor::LB);
		this->flows[GP::PSHP]->set_value(RealData(DB203, ps_hopper_gland_pump_flow), GraphletAnchor::LT);
		this->powers[GP::PSHP]->set_value(RealData(DB203, ps_hopper_pump_power), GraphletAnchor::LB);
		this->rpms[GP::PSHP]->set_value(RealData(DB203, ps_hopper_pump_rpm), GraphletAnchor::LB);

		this->pressures[GP::SBHP]->set_value(RealData(DB203, sb_hopper_gland_pump_pressure), GraphletAnchor::LB);
		this->flows[GP::SBHP]->set_value(RealData(DB203, sb_hopper_gland_pump_flow), GraphletAnchor::LB);
		this->powers[GP::SBHP]->set_value(RealData(DB203, sb_hopper_pump_power), GraphletAnchor::LB);
		this->rpms[GP::SBHP]->set_value(RealData(DB203, sb_hopper_pump_rpm), GraphletAnchor::LB);

		this->rpms[GP::PSHPa]->set_value(RealData(DB203, ps_hopper_gland_pump_A_rpm), GraphletAnchor::LB);
		this->rpms[GP::PSHPb]->set_value(RealData(DB203, ps_hopper_gland_pump_B_rpm), GraphletAnchor::LB);
		this->rpms[GP::SBHPa]->set_value(RealData(DB203, sb_hopper_gland_pump_A_rpm), GraphletAnchor::LB);
		this->rpms[GP::SBHPb]->set_value(RealData(DB203, sb_hopper_gland_pump_B_rpm), GraphletAnchor::LB);

		this->pressures[GP::PSUWP1]->set_value(RealData(DB203, ps_underwater_gland_pump1_pressure), GraphletAnchor::LB);
		this->pressures[GP::PSUWP2]->set_value(RealData(DB203, ps_underwater_gland_pump2_pressure), GraphletAnchor::LB);
		
		this->pressures[GP::SBUWP1]->set_value(RealData(DB203, sb_underwater_gland_pump1_pressure), GraphletAnchor::LB);
		this->pressures[GP::SBUWP2]->set_value(RealData(DB203, sb_underwater_gland_pump2_pressure), GraphletAnchor::LB);
	}

	void on_digital_input(const uint8* DB4, size_t count4, const uint8* DB205, size_t count25, WarGrey::SCADA::Syslog* logger) override {
		DI_hopper_pumps(this->hoppers[GP::PSHP], this->hoppers[GP::PSUWP], DB4, ps_hopper_pump_feedback, DB205, ps_hopper_pump_details, ps_underwater_pump_details);
		DI_hopper_pumps(this->hoppers[GP::SBHP], this->hoppers[GP::SBUWP], DB4, sb_hopper_pump_feedback, DB205, sb_hopper_pump_details, sb_underwater_pump_details);

		DI_gate_flushing_pump(this->pumps[GP::PSFP], DB4, pump_ps_gate_flushing_feedback, DB205, pump_ps_gate_flushing_status);
		DI_gate_flushing_pump(this->pumps[GP::SBFP], DB4, pump_sb_gate_flushing_feedback, DB205, pump_sb_gate_flushing_status);

		DI_hopper_gland_pump(this->pumps[GP::PSHPa], DB4, pump_ps_hopper_A_feedback, DB205, pump_ps_hopper_A_status);
		DI_hopper_gland_pump(this->pumps[GP::PSHPb], DB4, pump_ps_hopper_B_feedback, DB205, pump_ps_hopper_B_status);
		DI_hopper_gland_pump(this->pumps[GP::SBHPa], DB4, pump_sb_hopper_A_feedback, DB205, pump_sb_hopper_A_status);
		DI_hopper_gland_pump(this->pumps[GP::SBHPb], DB4, pump_sb_hopper_B_feedback, DB205, pump_sb_hopper_B_status);

		DI_underwater_gland_pump(this->pumps[GP::PSUWP1], DB4, pump_ps_underwater_1_feedback, DB205, pump_ps_underwater_1_status);
		DI_underwater_gland_pump(this->pumps[GP::PSUWP2], DB4, pump_ps_underwater_2_feedback, DB205, pump_ps_underwater_2_status);
		DI_underwater_gland_pump(this->pumps[GP::SBUWP1], DB4, pump_sb_underwater_1_feedback, DB205, pump_sb_underwater_1_status);
		DI_underwater_gland_pump(this->pumps[GP::SBUWP2], DB4, pump_sb_underwater_2_feedback, DB205, pump_sb_underwater_2_status);
	}

	void post_read_data(Syslog* logger) override {
		GP ps_hopper_long_path[] = { GP::DGV13, GP::pshp, GP::d44, GP::DGV8, GP::PSHP };
		GP ps_hopper_short_path[] = { GP::DGV14, GP::d44, GP::DGV8, GP::PSHP };
		GP sb_hopper_short_path[] = { GP::DGV15, GP::d45, GP::DGV7, GP::SBHP };
		GP sb_hopper_long_path[] = { GP::DGV16, GP::sbhp, GP::d45, GP::DGV7, GP::SBHP };
		GP ps_underwater_path[] = { GP::PSUWP1, GP::psuwp, GP::d46, GP::PSUWP };
		GP sb_underwater_path[] = { GP::SBUWP2, GP::sbuwp, GP::d47, GP::SBUWP };

		this->station->append_subtrack(GP::Hatch, GP::DGV16, water_color);
		this->station->append_subtrack(GP::Sea, GP::SBUWP2, water_color);

		this->try_flow_water(GP::PSFP, GP::DGV12, GP::flushs, water_color);
		this->try_flow_water(GP::SBFP, GP::DGV11, GP::flushs, water_color);
		this->try_flow_water(GP::PSHPa, ps_hopper_long_path, water_color);
		this->try_flow_water(GP::PSHPb, ps_hopper_short_path, water_color);
		this->try_flow_water(GP::SBHPa, sb_hopper_short_path, water_color);
		this->try_flow_water(GP::SBHPb, sb_hopper_long_path, water_color);

		this->try_flow_water(GP::PSUWP1, ps_underwater_path, water_color);
		this->try_flow_water(GP::PSUWP2, GP::PSUWP2, GP::PSUWP, water_color);
		this->try_flow_water(GP::SBUWP1, GP::SBUWP1, GP::SBUWP, water_color);
		this->try_flow_water(GP::SBUWP2, sb_underwater_path, water_color);

		this->master->end_update_sequence();
		this->master->leave_critical_section();
	}

public:
	bool on_char(VirtualKey key, PLCMaster* plc) {
		bool handled = false;

		if (key == VirtualKey::Enter) {
			auto editor = dynamic_cast<Credit<Dimensionlet, GP>*>(this->master->get_focus_graphlet());

			if (editor != nullptr) {
				float rpm = float(editor->get_input_number());

				if (rpm > 0.0F) {
					plc->send_setting(AO_gland_pump_setting(editor->id), rpm);
				}
			}

			handled = true;
		}

		return handled;
	}

public:
	void load(float width, float height, float gwidth, float gheight) {
		Turtle<GP>* pTurtle = new Turtle<GP>(gwidth, gheight, false, GP::Hatch);

		pTurtle->move_right(3);
		pTurtle->move_down(1, GP::d12)->move_right(5, GP::DGV12)->move_right(6, GP::PSFP);
		pTurtle->move_right(10)->move_down(2)->jump_back();
		pTurtle->move_down(4, GP::d11)->move_right(5, GP::DGV11)->move_right(6, GP::SBFP);
		pTurtle->move_right(10)->move_up(2)->move_right(4, GP::ToFlushs)->move_right(4, GP::flushs)->jump_back(GP::d11);

		pTurtle->move_down(4, GP::d13)->move_right(5, GP::DGV13)->move_right(6, GP::PSHPa)->move_right(6, GP::DGV3);
		pTurtle->move_right(10)->turn_right_down(GP::pshp)->move_down(3)->turn_down_right()->jump_back(GP::d13);
		pTurtle->move_down(4, GP::d14)->move_right(5, GP::DGV14)->move_right(6, GP::PSHPb)->move_right(6, GP::DGV4);
		pTurtle->move_right(14, GP::d44)->move_right(9, GP::DGV44)->move_right(12, GP::PSHP)->jump_back();
		pTurtle->move_up_right(2.5F, GP::SS1)->move_up_right(2.5F)->move_right(4, GP::DGV8);
		pTurtle->move_right(12)->move_down(5)->jump_back(GP::d14);

		pTurtle->move_down(5, GP::d15)->move_right(5, GP::DGV15)->move_right(6, GP::SBHPa)->move_right(6, GP::DGV5);
		pTurtle->move_right(14, GP::d45)->move_right(9, GP::DGV45)->move_right(12, GP::SBHP)->jump_back();
		pTurtle->move_down_right(2.5F, GP::SS2)->move_down_right(2.5F)->move_right(4, GP::DGV7);
		pTurtle->move_right(12)->move_up(5)->jump_back(GP::d15);
		pTurtle->move_down(4, GP::d16)->move_right(5, GP::DGV16)->move_right(6, GP::SBHPb)->move_right(6, GP::DGV6);
		pTurtle->move_right(10)->turn_right_up(GP::sbhp)->move_up(3)->turn_up_right()->jump_back(GP::d16);

		pTurtle->jump_down(3)->jump_left(GP::Sea)->move_right();

		pTurtle->move_down(2, GP::d17)->move_right(5)->move_right(6, GP::PSUWP1)->move_right(6);
		pTurtle->move_right(10)->turn_right_down(GP::psuwp)->move_down(3)->turn_down_right()->jump_back(GP::d17);
		pTurtle->move_down(4, GP::d18)->move_right(5)->move_right(6, GP::PSUWP2)->move_right(6);
		pTurtle->move_right(14, GP::d46)->move_right(9)->move_right(12, GP::PSUWP)->jump_back(GP::d18);

		pTurtle->move_down(5, GP::d19)->move_right(5)->move_right(6, GP::SBUWP1)->move_right(6);
		pTurtle->move_right(14, GP::d47)->move_right(9)->move_right(12, GP::SBUWP)->jump_back(GP::d19);
		pTurtle->move_down(4, GP::d20)->move_right(5)->move_right(6, GP::SBUWP2)->move_right(6);
		pTurtle->move_right(10)->turn_right_up(GP::sbuwp)->move_up(3)->turn_up_right();

		this->station = this->master->insert_one(new Tracklet<GP>(pTurtle, default_pipe_thickness, default_pipe_color));
		this->to_flushs = this->master->insert_one(new ArrowHeadlet(gheight, 0.0, Colours::Silver));
		this->sea = this->master->insert_one(new VLinelet(gheight * 2.0F, default_pipe_thickness,
			water_color, make_dash_stroke(CanvasDashStyle::Dash)));
		
		{ // load devices
			float radius = resolve_gridsize(gwidth, gheight);
			float hpradius = std::fminf(gwidth, gheight);

			this->hatch = this->master->insert_one(new Hatchlet(hpradius * 2.5F));

			this->load_devices(this->mvalves, this->labels, Colours::Silver, GP::DGV3, GP::DGV8, radius, 0.0);
			this->load_devices(this->pumps, this->labels, Colours::Salmon, GP::PSFP, GP::SBFP, radius, 0.0);
			this->load_devices(this->pumps, this->labels, Colours::Salmon, GP::PSHPa, GP::SBHPb, radius, 0.0);
			this->load_devices(this->pumps, this->labels, Colours::Salmon, GP::PSUWP1, GP::SBUWP2, radius, 0.0);
			
			this->load_device(this->hoppers, this->captions, GP::PSHP, hpradius, -2.0F, true);
			this->load_device(this->hoppers, this->captions, GP::SBHP, hpradius, +2.0F, true);
			this->load_device(this->hoppers, this->captions, GP::PSUWP, hpradius, +2.0F, false);
			this->load_device(this->hoppers, this->captions, GP::SBUWP, hpradius, -2.0F, false);
		}

		{ // load labels and dimensions
			this->load_labels(this->captions, GP::ToFlushs, GP::SS2, Colours::Silver);
			this->load_labels(this->captions, GP::Hatch, GP::Sea, Colours::Salmon);

			this->load_dimension(this->pressures, GP::PSFP, "bar", "P");
			this->load_dimensions(this->pressures, GP::PSUWP1, GP::SBUWP2, "bar", "P");
			this->load_dimensions(this->pressures, GP::PSHP, GP::SBHP, "bar", "P");
			this->load_dimensions(this->flows, GP::PSHP, GP::SBHP, "m3ph", "F");

			this->load_settings(this->rpms, GP::PSHPa, GP::SBHPb, "rpm", "S");
		}
	}

public:
	void reflow(float width, float height, float gwidth, float gheight, float vinset) {
		float toff = default_pipe_thickness * 2.0F;
		
		this->master->move_to(this->station, width * 0.5F, height * 0.5F, GraphletAnchor::CC, -gwidth * 2.0F);
		this->station->map_credit_graphlet(this->captions[GP::Sea], GraphletAnchor::RC, -toff);
		this->station->map_graphlet_at_anchor(this->sea, GP::Sea, GraphletAnchor::LC, 0.0F, this->sea_oscillation);
		this->station->map_graphlet_at_anchor(this->hatch, GP::Hatch, GraphletAnchor::LC);
		this->master->move_to(this->captions[GP::Hatch], this->hatch, GraphletAnchor::CB, GraphletAnchor::CT);

		this->station->map_graphlet_at_anchor(this->to_flushs, GP::flushs, GraphletAnchor::CC);
		this->station->map_credit_graphlet(this->captions[GP::ToFlushs], GraphletAnchor::CB);
		
		this->station->map_credit_graphlet(this->captions[GP::SS1], GraphletAnchor::LC);
		this->station->map_credit_graphlet(this->captions[GP::SS2], GraphletAnchor::LC);

		{ // reflow devices and metrics
			float gridsize = resolve_gridsize(gwidth, gheight);
			
			this->reflow_valves(this->mvalves, this->labels, gridsize);
			
			for (auto it = this->pumps.begin(); it != this->pumps.end(); it++) {
				this->station->map_credit_graphlet(it->second, GraphletAnchor::CC);
				this->master->move_to(this->labels[it->first], it->second, GraphletAnchor::CT, GraphletAnchor::CB);

				switch (it->first) {
				case GP::PSFP: {
					this->station->map_credit_graphlet(this->pressures[it->first], GraphletAnchor::LT, gwidth * 3.0F, gridsize);
				}; break;
				case GP::PSHPa: case GP::PSHPb: case GP::SBHPa: case GP::SBHPb: {
					this->station->map_credit_graphlet(this->rpms[it->first], GraphletAnchor::LB, gwidth * 9.0F, -toff);
				}; break;
				case GP::PSUWP1: case GP::PSUWP2: case GP::SBUWP1: case GP::SBUWP2: {
					this->station->map_credit_graphlet(this->pressures[it->first], GraphletAnchor::LB, gwidth * 9.0F, -toff);
				}; break;
				}
			}

			for (auto it = this->hoppers.begin(); it != this->hoppers.end(); it++) {
				GP tanchor = GP::_;

				this->station->map_credit_graphlet(it->second, GraphletAnchor::LC);
				this->master->move_to(this->captions[it->first], it->second, GraphletAnchor::RC, GraphletAnchor::LC, toff);

				if (this->rpms.find(it->first) != this->rpms.end()) {
					this->master->move_to(this->rpms[it->first], it->second, GraphletAnchor::LC, GraphletAnchor::RB, -gwidth * 0.5F, -toff);
					this->master->move_to(this->powers[it->first], this->rpms[it->first], GraphletAnchor::LB, GraphletAnchor::LT, 0.0, toff * 2.0F);
				}
				
				switch (it->first) {
				case GP::PSHP: tanchor = GP::DGV8; break;
				case GP::SBHP: tanchor = GP::DGV7; break;
				}

				if (tanchor != GP::_) {
					this->station->map_graphlet_at_anchor(this->pressures[it->first], tanchor, GraphletAnchor::LB, gwidth * 3.0F, -toff);
					this->station->map_graphlet_at_anchor(this->flows[it->first], tanchor, GraphletAnchor::LT, gwidth * 3.0F, toff);
				}
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
			ds[id]->set_maximum(double(gland_pump_rpm_range));
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
	void try_flow_water(GP pid, GP start, GP end, CanvasSolidColorBrush^ color) {
		switch (this->pumps[pid]->get_status()) {
		case HydraulicPumpStatus::Running: {
			this->station->append_subtrack(start, end, color);
		}
		}
	}

	void try_flow_water(GP pid, GP* path, unsigned int count, CanvasSolidColorBrush^ color) {
		switch (this->pumps[pid]->get_status()) {
		case HydraulicPumpStatus::Running: {
			this->station->append_subtrack(path, count, color);
		}
		}
	}

	template<unsigned int N>
	void try_flow_water(GP pid, GP (&path)[N], CanvasSolidColorBrush^ color) {
		this->try_flow_water(pid, path, N, color);
	}

// never deletes these graphlets mannually
private:
	Tracklet<GP>* station;
	Hatchlet* hatch;
	ArrowHeadlet* to_flushs;
	Linelet* sea;
	std::map<GP, Credit<Labellet, GP>*> captions;
	std::map<GP, Credit<Labellet, GP>*> labels;
	std::map<GP, Credit<HydraulicPumplet, GP>*> pumps;
	std::map<GP, Credit<HopperPumplet, GP>*> hoppers;
	std::map<GP, Credit<ManualValvelet, GP>*> mvalves;
	std::map<GP, Credit<Dimensionlet, GP>*> pressures;
	std::map<GP, Credit<Dimensionlet, GP>*> flows;
	std::map<GP, Credit<Dimensionlet, GP>*> rpms;
	std::map<GP, Credit<Dimensionlet, GP>*> powers;

private:
	CanvasTextFormat^ label_font;
	DimensionStyle dimension_style;
	DimensionStyle setting_style;
	float sea_oscillation;

private:
	GlandsPage* master;
};

GlandsPage::GlandsPage(PLCMaster* plc) : Planet(__MODULE__), device(plc) {
	GlandPumps* dashboard = new GlandPumps(this);

	this->dashboard = dashboard;
	this->pump_op = make_gland_pump_menu(DO_glands_action, plc);
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

GlandsPage::~GlandsPage() {
	if (this->dashboard != nullptr) {
		delete this->dashboard;
	}

#ifndef _DEBUG
	delete this->grid;
#endif
}

void GlandsPage::load(CanvasCreateResourcesReason reason, float width, float height) {
	auto dashboard = dynamic_cast<GlandPumps*>(this->dashboard);
	
	if (dashboard != nullptr) {
		float vinset = statusbar_height();
		float gwidth = width / 64.0F;
		float gheight = (height - vinset - vinset) / 44.0F;

		this->grid->set_grid_width(gwidth);
		this->grid->set_grid_height(gheight, vinset);

		{ // load graphlets
			this->change_mode(GPMode::Dashboard);
			dashboard->load(width, height, gwidth, gheight);

			this->change_mode(GPMode::WindowUI);
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

void GlandsPage::reflow(float width, float height) {
	auto dashboard = dynamic_cast<GlandPumps*>(this->dashboard);
	
	if (dashboard != nullptr) {
		float vinset = statusbar_height();
		float gwidth = this->grid->get_grid_width();
		float gheight = this->grid->get_grid_height();

		this->change_mode(GPMode::WindowUI);
		this->move_to(this->statusline, 0.0F, height, GraphletAnchor::LB);

		this->change_mode(GPMode::Dashboard);
		dashboard->reflow(width, height, gwidth, gheight, vinset);
	}
}

void GlandsPage::update(long long count, long long interval, long long uptime) {
	auto dashboard = dynamic_cast<GlandPumps*>(this->dashboard);

	if (dashboard != nullptr) {
		dashboard->update(count, interval, uptime);
	}
}


bool GlandsPage::can_select(IGraphlet* g) {
	return ((dynamic_cast<HydraulicPumplet*>(g) != nullptr));
}

bool GlandsPage::on_char(VirtualKey key, bool wargrey_keyboard) {
	bool handled = Planet::on_char(key, wargrey_keyboard);

	if (!handled) {
		auto db = dynamic_cast<GlandPumps*>(this->dashboard);

		if (db != nullptr) {
			handled = db->on_char(key, this->device);
		}
	}

	return handled;
}

void GlandsPage::on_focus(IGraphlet* g) {
	auto editor = dynamic_cast<IEditorlet*>(g);

	if (editor != nullptr) {
		this->show_virtual_keyboard(ScreenKeyboard::Numpad);
	}
}

void GlandsPage::on_tap_selected(IGraphlet* g, float local_x, float local_y) {
	auto pump = dynamic_cast<HydraulicPumplet*>(g);
	
	if (pump != nullptr) {
		menu_popup(this->pump_op, g, local_x, local_y);
	}
}
