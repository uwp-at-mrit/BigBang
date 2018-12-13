#include <map>

#include "page/diagnostics/hydraulic_pump_dx.hpp"
#include "configuration.hpp"

#include "module.hpp"
#include "string.hpp"
#include "brushes.hxx"

#include "graphlet/shapelet.hpp"
#include "graphlet/dashboard/alarmlet.hpp"

#include "iotables/di_pumps.hpp"
#include "iotables/di_valves.hpp"
#include "iotables/di_devices.hpp"
#include "iotables/di_dredges.hpp"
#include "iotables/di_hopper_pumps.hpp"

using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

static CanvasSolidColorBrush^ region_background = Colours::make(diagnostics_region_background);
static CanvasSolidColorBrush^ diagnosis_background = Colours::make(diagnostics_alarm_background);

static CanvasSolidColorBrush^ caption_color = Colours::Salmon;
static CanvasSolidColorBrush^ diagnosis_color = Colours::Silver;

// WARNING: order matters
private enum class P : unsigned int {
	// Groups
	MiscCondition, PumpCondition,

	// Misc Conditions
	NoConsolePSStop, NoConsoleSBStop, NoSailingStop, NoPSWinchGantryStop, NoSBWinchGantryStop,
	NoMasterTankFS01, NoMasterTankFS02, NoMasterTankLS1, NoMasterTankLS2,
	NoVisorTankFS, NoVisorTankLS1, NoVisorTankLS2,
	SQ1Open, SQ2Open, CoolantRunning,

	// Pump Conditions
	Ready, NoRunning, NoBroken,
	SQAOpen,
	NoCrA, NoJrA,

	_
};

static P other[] = { P::NoSailingStop, P::NoMasterTankFS02, P::NoMasterTankLS1, P::NoMasterTankLS2 };
static P visor[] = { P::NoSailingStop, P::NoVisorTankFS, P::NoVisorTankLS1, P::NoVisorTankLS2 };
static P ps[] = { P::NoConsolePSStop, P::NoSailingStop, P::NoPSWinchGantryStop, P::NoMasterTankFS01,
	P::NoMasterTankLS1, P::NoMasterTankLS2, P::SQ2Open, P::CoolantRunning };
static P sb[] = { P::NoConsoleSBStop, P::NoSailingStop, P::NoSBWinchGantryStop, P::NoMasterTankFS01,
	P::NoMasterTankLS1, P::NoMasterTankLS2, P::SQ1Open, P::CoolantRunning };

static const P* select_group_conditions(HPDX gid, unsigned int* count) {
	const P* group = nullptr;

	switch (gid) {
	case HPDX::PS: group = ps; SET_BOX(count, sizeof(ps) / sizeof(P)); break;
	case HPDX::SB: group = sb; SET_BOX(count, sizeof(sb) / sizeof(P)); break;
	case HPDX::Visor: group = visor; SET_BOX(count, sizeof(visor) / sizeof(P)); break;
	case HPDX::Other: group = other; SET_BOX(count, sizeof(other) / sizeof(P)); break;
	}

	return group;
}

/*************************************************************************************************/
private class PumpDx final : public PLCConfirmation {
public:
	PumpDx(HydraulicPumpDiagnostics* master) : master(master) {
		this->region_font = make_bold_text_format("Microsoft YaHei", normal_font_size);
		this->diagnosis_font = make_bold_text_format("Microsoft YaHei", small_font_size);
	}

public:
	void pre_read_data(Syslog* logger) override {
		this->master->enter_critical_section();
		this->master->begin_update_sequence();
	}

	void on_digital_input(const uint8* DB4, size_t count4, const uint8* DB205, size_t count205, Syslog* logger) override {
		this->diagnoses[P::NoConsolePSStop]->set_state(DBX(DB4, console_ps_hydraulic_stop_button - 1U), AlarmState::None, AlarmState::Notice);
		this->diagnoses[P::NoConsoleSBStop]->set_state(DBX(DB4, console_sb_hydraulic_stop_button - 1U), AlarmState::None, AlarmState::Notice);
		this->diagnoses[P::NoSailingStop]->set_state(DBX(DB4, sailing_hydraulic_stop_button - 1U), AlarmState::None, AlarmState::Notice);
		this->diagnoses[P::NoPSWinchGantryStop]->set_state(DBX(DB4, console_ps_winch_gantry_stop_button - 1U), AlarmState::None, AlarmState::Notice);
		this->diagnoses[P::NoSBWinchGantryStop]->set_state(DBX(DB4, console_sb_winch_gantry_stop_button - 1U), AlarmState::None, AlarmState::Notice);
		this->diagnoses[P::NoMasterTankFS01]->set_state(DBX(DB4, filter_01_status - 1U), AlarmState::None, AlarmState::Notice);
		this->diagnoses[P::NoMasterTankFS02]->set_state(DBX(DB4, filter_02_status - 1U), AlarmState::None, AlarmState::Notice);
		this->diagnoses[P::NoVisorTankFS]->set_state(DBX(DB4, filter_10_status - 1U), AlarmState::None, AlarmState::Notice);
		this->diagnoses[P::NoMasterTankLS1]->set_state(DI_tank_level_low(DB4, master_tank_status), AlarmState::None, AlarmState::Notice);
		this->diagnoses[P::NoMasterTankLS2]->set_state(DI_tank_level_too_low(DB4, master_tank_status), AlarmState::None, AlarmState::Notice);
		this->diagnoses[P::NoVisorTankLS1]->set_state(DI_tank_level_low(DB4, visor_tank_status), AlarmState::None, AlarmState::Notice);
		this->diagnoses[P::NoVisorTankLS2]->set_state(DI_tank_level_too_low(DB4, visor_tank_status), AlarmState::None, AlarmState::Notice);
		this->diagnoses[P::SQ1Open]->set_state(DI_manual_valve_open(DB4, manual_valve_SQ1_status), AlarmState::Notice, AlarmState::None);
		this->diagnoses[P::SQ2Open]->set_state(DI_manual_valve_open(DB4, manual_valve_SQ2_status), AlarmState::Notice, AlarmState::None);
		this->diagnoses[P::CoolantRunning]->set_state(DI_hydraulic_pump_running(DB4, pump_K_feedback), AlarmState::Notice, AlarmState::None);
	}

	void post_read_data(Syslog* logger) override {
		this->master->end_update_sequence();
		this->master->leave_critical_section();
	}

public:
	void fill_extent(float title_height, float vgapsize, float* width, float* height) {
		unsigned int gc_count, pc_count;
		float region_reserved_height = vgapsize * 4.0F + this->region_font->FontSize;

		select_group_conditions(HPDX::PS, &gc_count);
		pc_count = 0U;

		this->diagnosis_height = this->diagnosis_font->FontSize * 2.0F;
		this->misc_region_height = (this->diagnosis_height + vgapsize) * float(gc_count) + region_reserved_height;
		this->pump_region_height = (this->diagnosis_height + vgapsize) * float(pc_count) + region_reserved_height;
		
		SET_BOX(width, 400.0F);
		SET_BOX(height, this->misc_region_height + this->pump_region_height + title_height * 3.0F);
	}

	void load(float x, float width, float height, float title_height, float vgapsize) {
		float region_width = width * 0.90F;
		float diagnosis_width = (region_width - title_height * 1.5F);
		float corner_radius = 8.0F;
		
		this->misc_region = this->master->insert_one(
			new RoundedRectanglet(region_width, this->misc_region_height, corner_radius, region_background));

		this->pump_region = this->master->insert_one(
			new RoundedRectanglet(region_width, this->pump_region_height, corner_radius, region_background));

		this->load_label(this->labels, P::MiscCondition, P::MiscCondition.ToString(), caption_color, this->region_font);
		this->load_label(this->labels, P::PumpCondition, P::PumpCondition.ToString(), caption_color, this->region_font);

		{ // load diagnoses
			float icon_size = this->diagnosis_height * 0.618F;
			
			for (P id = P::NoConsolePSStop; id < P::_; id++) {
				this->slots[id] = this->master->insert_one(new Credit<RoundedRectanglet, P>(
					diagnosis_width, this->diagnosis_height, corner_radius, diagnosis_background), id);

				this->diagnoses[id] = this->master->insert_one(new Credit<Alarmlet, P>(icon_size), id);
				this->load_label(this->labels, id, diagnosis_color, this->diagnosis_font);
			}
		}
	}

	void reflow(float width, float height, float title_height, float vgapsize) {
		unsigned int gc_count = 0;
		const P* group = select_group_conditions(this->group, &gc_count);
		
		{ // reflow layout
			float gapsize = (height - title_height - this->misc_region_height - this->pump_region_height) / 3.0F;

			this->master->move_to(this->misc_region, width * 0.5F, title_height + gapsize, GraphletAnchor::CT);
			this->master->move_to(this->pump_region, this->misc_region, GraphletAnchor::CB, GraphletAnchor::CT, 0.0F, gapsize);
			
			this->master->move_to(this->labels[P::MiscCondition], this->misc_region, GraphletAnchor::CT, GraphletAnchor::CT, 0.0F, vgapsize);
			this->master->move_to(this->labels[P::PumpCondition], this->pump_region, GraphletAnchor::CT, GraphletAnchor::CT, 0.0F, vgapsize);
		}

		{ // conceal irrelevant diagnosis
			for (P id = P::NoConsolePSStop; id < P::_; id++) {
				this->master->move_to(this->slots[id], 0.0F, 0.0F);
			}
		}

		this->reflow(this->slots, P::MiscCondition, group, gc_count, GraphletAnchor::CB, GraphletAnchor::CT, vgapsize);
		
		{ // reflow diagnostics
			float inset = vgapsize * 1.618F;
			float step = vgapsize;
			float icon_width;

			this->diagnoses[P::NoConsolePSStop]->fill_extent(0.0F, 0.0F, &icon_width, nullptr);
			step += icon_width;

			for (P id = P::NoConsolePSStop; id < P::_; id++) {
				this->master->move_to(this->diagnoses[id], this->slots[id], GraphletAnchor::LC, GraphletAnchor::LC, step * 0.0F + inset);
				this->master->move_to(this->labels[id], this->slots[id], GraphletAnchor::LC, GraphletAnchor::LC, step * 1.0F + inset + vgapsize);
			}
		}
	}

public:
	bool available() override {
		return (this->master->surface_ready() && this->master->shown());
	}

	void set_pump(Platform::String^ name, HPDX gid) {
		// delay the updating when `switch_id`
		this->pump = name;
		this->group = gid;
	}

	void switch_id(unsigned int id) {
		// `ICreditSatellite` guarantees that the master is loaded and reflowed;
		// furthermore, the current `id` is available in `load` and `reflow` even through they are invoked earlier.
		this->labels[P::MiscCondition]->set_text(_speak(this->group.ToString() + P::MiscCondition.ToString()), GraphletAnchor::CC);
		this->labels[P::PumpCondition]->set_text(_speak(this->pump + P::PumpCondition.ToString()), GraphletAnchor::CC);
	}

private:
	template<typename E>
	void load_label(std::map<E, Credit<Labellet, E>*>& ls, E id, CanvasSolidColorBrush^ color, CanvasTextFormat^ font = nullptr) {
		ls[id] = this->master->insert_one(new Credit<Labellet, E>(_speak(id), font, color), id);
	}

	template<typename E>
	void load_label(std::map<E, Credit<Labellet, E>*>& ls, E id, Platform::String^ prefix, CanvasSolidColorBrush^ color, CanvasTextFormat^ font = nullptr) {
		ls[id] = this->master->insert_one(new Credit<Labellet, E>(_speak(prefix + id.ToString()), font, color), id);
	}

private:
	template<class G, typename E>
	void reflow(std::map<E, Credit<G, E>*>& gs, E label, const E* ds, unsigned int count, GraphletAnchor ta, GraphletAnchor a, float yoff) {
		IGraphlet* target = this->labels[label];

		for (unsigned int idx = 0; idx < count; idx++) {
			auto g = gs[ds[idx]];

			this->master->move_to(g, target, ta, a, 0.0F, yoff);
			target = g;
		}
	}

private: // never delete these graphlets mannually
	std::map<P, Credit<Labellet, P>*> labels;
	std::map<P, Credit<Alarmlet, P>*> diagnoses;
	std::map<P, Credit<RoundedRectanglet, P>*> slots;
	RoundedRectanglet* misc_region;
	RoundedRectanglet* pump_region;
	
private:
	CanvasTextFormat^ region_font;
	CanvasTextFormat^ diagnosis_font;

private:
	float diagnosis_height;
	float misc_region_height;
	float pump_region_height;

private:
	HydraulicPumpDiagnostics* master;
	unsigned int feedback_id;
	Platform::String^ pump;
	HPDX group;
};

HydraulicPumpDiagnostics::HydraulicPumpDiagnostics(PLCMaster* plc) : ICreditSatellite(plc->get_logger(), __MODULE__), device(plc) {
	PumpDx* dashboard = new PumpDx(this);
	
	this->dashboard = dashboard;
	
	this->device->append_confirmation_receiver(dashboard);
}

HydraulicPumpDiagnostics::~HydraulicPumpDiagnostics() {
	if (this->dashboard != nullptr) {
		delete this->dashboard;
	}
}

void HydraulicPumpDiagnostics::fill_satellite_extent(float* width, float* height) {
	auto dashboard = dynamic_cast<PumpDx*>(this->dashboard);
	float db_width = 400.0F;
	float db_height = 600.0F;
	
	this->title_height = large_font_size * 2.0F;
	this->vgapsize = this->title_height * 0.16F;

	if (dashboard != nullptr) {
		dashboard->fill_extent(this->title_height, this->vgapsize, &db_width, &db_height);
	}

	SET_BOX(width, db_width);
	SET_BOX(height, db_height);
}

void HydraulicPumpDiagnostics::load(CanvasCreateResourcesReason reason, float width, float height) {
	auto dashboard = dynamic_cast<PumpDx*>(this->dashboard);
	
	if (dashboard != nullptr) {
		auto caption_font = make_bold_text_format("Microsoft YaHei", large_font_size);
		
		dashboard->load(0.0F, width, height, this->title_height, this->vgapsize);
		
		this->titlebar = this->insert_one(new Rectanglet(width, this->title_height, Colours::make(diagnostics_caption_background)));
		this->title = this->insert_one(new Labellet(this->display_name(), caption_font, diagnostics_caption_foreground));
	}
}

void HydraulicPumpDiagnostics::reflow(float width, float height) {
	auto dashboard = dynamic_cast<PumpDx*>(this->dashboard);

	if (dashboard != nullptr) {
		dashboard->reflow(width, height, this->title_height, this->vgapsize);
		this->move_to(this->title, this->titlebar, GraphletAnchor::CC, GraphletAnchor::CC);
	}
}

void HydraulicPumpDiagnostics::set_pump(Platform::String^ id, HPDX group) {
	auto dashboard = dynamic_cast<PumpDx*>(this->dashboard);

	if (dashboard != nullptr) {
		dashboard->set_pump(id, group);
	}
}

void HydraulicPumpDiagnostics::on_id_changed(unsigned int id) {
	auto dashboard = dynamic_cast<PumpDx*>(this->dashboard);

	if (dashboard != nullptr) {
		dashboard->switch_id(id);
	}
}
