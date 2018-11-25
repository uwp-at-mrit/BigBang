#include <map>

#include "page/diagnostics/waterpumpdx.hpp"
#include "configuration.hpp"
#include "menu.hpp"

#include "module.hpp"
#include "brushes.hxx"

#include "graphlet/shapelet.hpp"
#include "graphlet/buttonlet.hpp"
#include "graphlet/dashboard/alarmlet.hpp"

#include "iotables/di_water_pumps.hpp"
#include "iotables/do_water_pumps.hpp"

#include "decorator/page.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI::Xaml::Controls;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

static CanvasSolidColorBrush^ region_background = Colours::make(diagnostics_region_background);
static CanvasSolidColorBrush^ alarm_background = Colours::make(diagnostics_alarm_background);

// WARNING: order matters
private enum class WP : unsigned int {
	// Groups
	StartCondition, RunningCondition,

	// Start Conditions
	RemoteControl,
	NoAlert, NoBroken,
	NotRunning, NoEmergence, NoRepair,
	PipelineReady, SpeedKnobMoved,

	// Running Conditions
	StartReady, SpeedKnobMoved0,

	_
};

private class WaterPumpDx final : public PLCConfirmation {
public:
	WaterPumpDx(WaterPumpDiagnostics* master, bool ps, unsigned int color) : master(master), ps(ps) {
		this->group_font = make_bold_text_format("Microsoft YaHei", normal_font_size);
		this->alarm_font = make_bold_text_format("Microsoft YaHei", small_font_size);

		this->color = Colours::make(color);

		this->sc_start = WP::RemoteControl;
		this->sc_end = WP::SpeedKnobMoved;
		this->rc_start = WP::StartReady;
		this->rc_end = WP::SpeedKnobMoved0;
	}

public:
	void pre_read_data(Syslog* logger) override {
		this->master->enter_critical_section();
		this->master->begin_update_sequence();
	}

	void on_digital_input(const uint8* DB4, size_t count4, const uint8* DB205, size_t count205, Syslog* logger) {
		unsigned int feedback = (this->ps ? ps_water_pump_feedback : sb_water_pump_feedback);
		unsigned int plready = (this->ps ? ps_water_pump_pipeline_ready : sb_water_pump_pipeline_ready);
		unsigned int knob = (this->ps ? ps_water_pump_speed_knob_moved : sb_water_pump_speed_knob_moved);
		unsigned int knob0 = (this->ps ? ps_water_pump_speed_knob_moved0 : sb_water_pump_speed_knob_moved0);

		this->alarms[WP::RemoteControl]->set_status(DI_water_pump_remote_control(DB4, feedback), AlarmStatus::Notice, AlarmStatus::None);
		this->alarms[WP::NoAlert]->set_status(DI_water_pump_alert(DB4, feedback), AlarmStatus::None, AlarmStatus::Notice);
		this->alarms[WP::NoBroken]->set_status(DI_water_pump_broken(DB4, feedback), AlarmStatus::None, AlarmStatus::Notice);
		this->alarms[WP::NotRunning]->set_status(DI_water_pump_running(DB4, feedback), AlarmStatus::None, AlarmStatus::Notice);
		this->alarms[WP::NoEmergence]->set_status(DI_water_pump_emergence(DB4, feedback), AlarmStatus::None, AlarmStatus::Notice);
		this->alarms[WP::NoRepair]->set_status(DI_water_pump_repair(DB4, feedback), AlarmStatus::None, AlarmStatus::Notice);
		this->alarms[WP::PipelineReady]->set_status(DBX(DB205, plready), AlarmStatus::Notice, AlarmStatus::None);
		this->alarms[WP::SpeedKnobMoved]->set_status(DBX(DB4, knob), AlarmStatus::Notice, AlarmStatus::None);

		this->alarms[WP::StartReady]->set_status(DI_water_pump_ready(DB4, feedback), AlarmStatus::Notice, AlarmStatus::None);
		this->alarms[WP::SpeedKnobMoved0]->set_status(DBX(DB4, knob0), AlarmStatus::Notice, AlarmStatus::None);
	}

	void post_read_data(Syslog* logger) override {
		this->master->end_update_sequence();
		this->master->leave_critical_section();
	}

public:
	void load(float x, float width, float height, float vinset) {
		unsigned int sc_count = _I(this->sc_end) - _I(this->sc_start) + 1U;
		unsigned int rc_count = _I(this->rc_end) - _I(this->rc_start) + 1U;
		float gapsize = vinset * 0.25F;
		float region_width = width * 0.85F;
		float region_reserved_height = gapsize * 4.0F + this->group_font->FontSize;
		float alarm_box_width = (region_width - vinset * 1.5F);
		float alarm_box_height = this->alarm_font->FontSize * 2.0F;
		float corner_radius = 8.0F;
		float sc_height = (alarm_box_height + gapsize) * float(sc_count) + region_reserved_height;
		float rc_height = (alarm_box_height + gapsize) * float(rc_count) + region_reserved_height;
		
		this->start_condition_bg = this->master->insert_one(new RoundedRectanglet(region_width, sc_height, corner_radius, region_background));
		this->running_condition_bg = this->master->insert_one(new RoundedRectanglet(region_width, rc_height, corner_radius, region_background));

		this->load_label(this->labels, WP::StartCondition, this->color, this->group_font, true);
		this->load_label(this->labels, WP::RunningCondition, this->color, this->group_font, true);

		this->reset = this->master->insert_one(new Credit<Buttonlet, bool>(PSWaterPumpAction::Reset.ToString()), this->ps);

		{ // load alarms
			float alarm_size = alarm_box_height * 0.618F;
		
			for (WP id = WP::RemoteControl; id <= WP::SpeedKnobMoved0; id++) {
				this->boxes[id] = this->master->insert_one(new Credit<RoundedRectanglet, WP>(
					alarm_box_width, alarm_box_height, corner_radius, alarm_background), id);

				this->alarms[id] = this->master->insert_one(new Credit<Alarmlet, WP>(alarm_size), id);
				this->load_label(this->labels, id, Colours::Silver, this->alarm_font);
			}
		}
	}

	void reflow(float x, float width, float height, float vinset) {
		unsigned int sc_count = _I(this->sc_end) - _I(this->sc_start) + 1U;
		float region_width, sc_height, rc_height, btn_height, alarm_height;
		float cx = x + width * 0.5F;
		float txtgap = 0.0F;
		
		this->start_condition_bg->fill_extent(0.0F, 0.0F, &region_width, &sc_height);
		this->running_condition_bg->fill_extent(0.0F, 0.0F, nullptr, &rc_height);
		this->reset->fill_extent(0.0F, 0.0F, nullptr, &btn_height);
		this->boxes[this->sc_start]->fill_extent(0.0F, 0.0F, nullptr, &alarm_height);

		txtgap = (sc_height - alarm_height * float(sc_count) - this->group_font->FontSize) / float(sc_count + 4);

		{ // reflow layout
			float gapsize = (height - vinset - sc_height - rc_height - btn_height) * 0.25F;

			this->master->move_to(this->start_condition_bg, cx, vinset + gapsize, GraphletAnchor::CT);
			this->master->move_to(this->running_condition_bg, this->start_condition_bg, GraphletAnchor::CB, GraphletAnchor::CT, 0.0F, gapsize);

			this->master->move_to(this->labels[WP::StartCondition], this->start_condition_bg, GraphletAnchor::CT, GraphletAnchor::CT, 0.0F, txtgap);
			this->master->move_to(this->labels[WP::RunningCondition], this->running_condition_bg, GraphletAnchor::CT, GraphletAnchor::CT, 0.0F, txtgap);
			this->master->move_to(this->reset, this->running_condition_bg, GraphletAnchor::CB, GraphletAnchor::CT, 0.0F, gapsize);
		}

		{ // reflow start condition boxes
			IGraphlet* target = this->labels[WP::StartCondition];

			for (WP id = WP::RemoteControl; id <= WP::SpeedKnobMoved; id++) {
				this->master->move_to(this->boxes[id], target, GraphletAnchor::CB, GraphletAnchor::CT, 0.0F, txtgap);
				target = this->boxes[id];
			}
		}

		{ // reflow running condition boxes
			IGraphlet* target = this->labels[WP::RunningCondition];

			for (WP id = WP::StartReady; id <= WP::SpeedKnobMoved0; id++) {
				this->master->move_to(this->boxes[id], target, GraphletAnchor::CB, GraphletAnchor::CT, 0.0F, txtgap);
				target = this->boxes[id];
			}
		}

		for (WP id = WP::RemoteControl; id <= WP::SpeedKnobMoved0; id++) {
			this->master->move_to(this->alarms[id], this->boxes[id], GraphletAnchor::LC, GraphletAnchor::LC, txtgap);
			this->master->move_to(this->labels[id], this->alarms[id], GraphletAnchor::RC, GraphletAnchor::LC, (width - region_width) * 0.5F);
		}
	}

public:
	bool available() override {
		return (this->master->surface_ready() && this->master->shown());
	}

private:
	template<typename E>
	void load_label(std::map<E, Credit<Labellet, E>*>& ls, E id, CanvasSolidColorBrush^ color, CanvasTextFormat^ font = nullptr, bool prefix = false) {
		Platform::String^ label = (prefix ? _speak((this->ps ? "PS" : "SB") + id.ToString()) : _speak(id));

		ls[id] = this->master->insert_one(new Credit<Labellet, E>(label, font, color), id);
	}

// never deletes these graphlets mannually
private:
	std::map<WP, Credit<Labellet, WP>*> labels;
	std::map<WP, Credit<Alarmlet, WP>*> alarms;
	std::map<WP, Credit<RoundedRectanglet, WP>*> boxes;
	Credit<Buttonlet, bool>* reset;
	RoundedRectanglet* start_condition_bg;
	RoundedRectanglet* running_condition_bg;
	
private:
	CanvasSolidColorBrush^ color;
	CanvasTextFormat^ group_font;
	CanvasTextFormat^ alarm_font;

private:
	WP sc_start;
	WP sc_end;
	WP rc_start;
	WP rc_end;

private:
	WaterPumpDiagnostics* master;
	bool ps;
};

WaterPumpDiagnostics::WaterPumpDiagnostics(PLCMaster* plc) : ISatellite(plc->get_logger(), __MODULE__), device(plc) {
	WaterPumpDx* ps_dashboard = new WaterPumpDx(this, true, default_ps_color);
	WaterPumpDx* sb_dashboard = new WaterPumpDx(this, false, default_sb_color);

	this->ps_dashboard = ps_dashboard;
	this->sb_dashboard = sb_dashboard;
	
	this->device->append_confirmation_receiver(ps_dashboard);
	this->device->append_confirmation_receiver(sb_dashboard);
}

WaterPumpDiagnostics::~WaterPumpDiagnostics() {
	if (this->ps_dashboard != nullptr) {
		delete this->ps_dashboard;
		delete this->sb_dashboard;
	}
}

void WaterPumpDiagnostics::fill_satellite_extent(float* width, float* height) {
	SET_BOX(width, diagnostics_width);
	SET_BOX(height, diagnostics_height);
}

void WaterPumpDiagnostics::load(CanvasCreateResourcesReason reason, float width, float height) {
	auto ps_dashboard = dynamic_cast<WaterPumpDx*>(this->ps_dashboard);
	auto sb_dashboard = dynamic_cast<WaterPumpDx*>(this->sb_dashboard);
	
	if ((ps_dashboard != nullptr) && (sb_dashboard)) {
		auto caption_font = make_bold_text_format("Microsoft YaHei", large_font_size);
		float vinset = large_font_size * 2.0F;
		float half_width = width * 0.5F;

		ps_dashboard->load(0.0F, half_width, height, vinset);
		sb_dashboard->load(half_width, half_width, height, vinset);

		this->caption_background = this->insert_one(new Rectanglet(width, vinset, Colours::make(diagnostics_caption_background)));
		this->caption = this->insert_one(new Labellet(this->display_name(), caption_font, diagnostics_caption_foreground));
	}
}

void WaterPumpDiagnostics::reflow(float width, float height) {
	auto ps_dashboard = dynamic_cast<WaterPumpDx*>(this->ps_dashboard);
	auto sb_dashboard = dynamic_cast<WaterPumpDx*>(this->sb_dashboard);

	if ((ps_dashboard != nullptr) && (sb_dashboard != nullptr)) {
		float half_width = width * 0.5F;
		float vinset = 0.0F;

		this->caption_background->fill_extent(0.0F, 0.0F, nullptr, &vinset);

		ps_dashboard->reflow(0.0F,       half_width, height, vinset);
		sb_dashboard->reflow(half_width, half_width, height, vinset);

		this->move_to(this->caption, this->caption_background, GraphletAnchor::CC, GraphletAnchor::CC);
	}
}

bool WaterPumpDiagnostics::can_select(IGraphlet* g) {
	bool okay = false;

	if (this->device->get_mode() != PLCMasterMode::User) {
		okay = ((dynamic_cast<Buttonlet*>(g) != nullptr));
	}

	return okay;
}

void WaterPumpDiagnostics::on_tap_selected(IGraphlet* g, float local_x, float local_y) {
	auto reset = dynamic_cast<Credit<Buttonlet, bool>*>(g);

	if (reset != nullptr) {
		this->device->send_command(DO_water_pump_reset_command(reset->id));
	}
}
