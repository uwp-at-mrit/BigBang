#include <map>

#include "page/diagnostics/dredges_dx.hpp"
#include "configuration.hpp"
#include "menu.hpp"

#include "module.hpp"
#include "brushes.hxx"

#include "graphlet/shapelet.hpp"
#include "graphlet/dashboard/alarmlet.hpp"

#include "iotables/di_hopper_pumps.hpp"
#include "iotables/do_hopper_pumps.hpp"

#include "decorator/page.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI::Xaml::Controls;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

static CanvasSolidColorBrush^ region_background = Colours::make(diagnostics_region_background);
static CanvasSolidColorBrush^ diagnosis_background = Colours::make(diagnostics_alarm_background);

// WARNING: order matters
private enum class WG : unsigned int {
	// Groups
	MiscCondition, WinchCondition, GantryCondition, WinchMetrics,

	// Misc Conditions
	PumpsRunning,
	NoConsoleStop, NoSceneStop,
	NoHeadLifting, NoIntermediateLifting,
	ConsoleAllowed, BackOilPressureOkay,

	// Gantry Conditions
	WinchAtTop, NoWindedUp, NoWindedOut,

	// Winch Conditions
	AllGantriesInOrOut, HopperStopped, InflatingValveClosed,
	NoSoftUpper, NoUpper, NoSuction, NoSlack, NoSaddle,

	// Metrics
	CurrentPulse, TopPulse, BottomPulse, SaddlePulse,

	_
};

private class DredgesDx final : public PLCConfirmation {
public:
	DredgesDx(DredgesDiagnostics* master, DX side) : master(master), side(side) {
		this->region_font = make_bold_text_format("Microsoft YaHei", normal_font_size);
		this->diagnosis_font = make_bold_text_format("Microsoft YaHei", small_font_size);

		this->color = Colours::make((side == DX::PS) ? default_ps_color : default_sb_color);
		this->diagnosis_color = Colours::Silver;
		this->inset_ratio = 1.618F;

		this->misc_start = WG::PumpsRunning;
		this->misc_end = WG::BackOilPressureOkay;
		this->gantry_start = WG::WinchAtTop;
		this->gantry_end = WG::NoWindedOut;
		this->winch_start = WG::AllGantriesInOrOut;
		this->winch_end = WG::NoSaddle;
		this->metrics_start = WG::CurrentPulse;
		this->metrics_end = WG::SaddlePulse;
	}

public:
	void pre_read_data(Syslog* logger) override {
		this->master->enter_critical_section();
		this->master->begin_update_sequence();
	}

	void on_digital_input(const uint8* DB4, size_t count4, const uint8* DB205, size_t count205, Syslog* logger) {
	}

	void post_read_data(Syslog* logger) override {
		this->master->end_update_sequence();
		this->master->leave_critical_section();
	}

public:
	void fill_extent(float title_height, float vgapsize, float* width, float* height) {
		unsigned int mc_count = _I(this->misc_end) - _I(this->misc_start) + 1U;
		unsigned int gc_count = _I(this->gantry_end) - _I(this->gantry_start) + 1U;
		unsigned int wc_count = _I(this->winch_end) - _I(this->winch_start) + 1U;
		unsigned int wmc_count = _I(this->metrics_end) - _I(this->metrics_start) + 1U;
		float region_reserved_height = vgapsize * 4.0F + this->region_font->FontSize;
		float left_height, right_height;
		
		this->diagnosis_height = this->diagnosis_font->FontSize * 2.0F;
		this->misc_region_height = (this->diagnosis_height + vgapsize) * float(mc_count) + region_reserved_height;
		this->gantry_region_height = (this->diagnosis_height + vgapsize) * float(gc_count) + region_reserved_height;
		this->winch_region_height = (this->diagnosis_height + vgapsize) * float(wc_count) + region_reserved_height;
		this->metrics_region_height = (this->diagnosis_height + vgapsize) * float(wmc_count) + region_reserved_height;
		
		left_height = this->misc_region_height + this->metrics_region_height;
		right_height = this->winch_region_height + this->gantry_region_height;

		SET_BOX(width, 800.0F);
		SET_BOX(height, std::max(left_height, right_height) + title_height * 3.0F);
	}

	void load(float x, float width, float height, float title_height, float vgapsize) {
		float region_width = width * 0.5F * 0.90F;
		float diagnosis_width = (region_width - title_height * 1.5F);
		float corner_radius = 8.0F;
		
		this->misc_region = this->master->insert_one(
			new RoundedRectanglet(region_width, this->misc_region_height, corner_radius, region_background));

		this->gantry_region = this->master->insert_one(
			new RoundedRectanglet(region_width, this->gantry_region_height, corner_radius, region_background));

		this->winch_region = this->master->insert_one(
			new RoundedRectanglet(region_width, this->winch_region_height, corner_radius, region_background));

		this->metrics_region = this->master->insert_one(
			new RoundedRectanglet(region_width, this->metrics_region_height, corner_radius, region_background));

		this->load_label(this->labels, WG::MiscCondition, side, this->color, this->region_font);
		this->load_label(this->labels, WG::GantryCondition, DxPosition::Trunnion, this->color, this->region_font);
		this->load_label(this->labels, WG::WinchCondition, DxPosition::Trunnion, this->color, this->region_font);
		this->load_label(this->labels, WG::WinchMetrics, DxPosition::Trunnion, this->color, this->region_font);

		{ // load diagnoses
			float icon_size = this->diagnosis_height * 0.618F;
		
			for (WG id = this->misc_start; id <= this->metrics_end; id++) {
				this->slots[id] = this->master->insert_one(new Credit<RoundedRectanglet, WG>(
					diagnosis_width, this->diagnosis_height, corner_radius, diagnosis_background), id);

				this->diagnoses[id] = this->master->insert_one(new Credit<Alarmlet, WG>(icon_size), id);

				switch (id) {
				case WG::HopperStopped: case WG::InflatingValveClosed: {
					this->load_label(this->labels, id, side, this->diagnosis_color, this->diagnosis_font);
				}; break;
				default: {
					this->load_label(this->labels, id, this->diagnosis_color, this->diagnosis_font);
				}
				}
			}
		}
	}

	void reflow(float width, float height, float title_height, float vgapsize) {
		{ // reflow layout
			float gapsize = (height - title_height - this->winch_region_height - this->gantry_region_height) / 3.0F;

			this->master->move_to(this->misc_region, width * 0.25F, title_height + gapsize, GraphletAnchor::CT);
			this->master->move_to(this->metrics_region, this->misc_region, GraphletAnchor::CB, GraphletAnchor::CT, 0.0F, gapsize);
			this->master->move_to(this->winch_region, width * 0.75F, title_height + gapsize, GraphletAnchor::CT);
			this->master->move_to(this->gantry_region, this->winch_region, GraphletAnchor::CB, GraphletAnchor::CT, 0.0F, gapsize);
			
			this->master->move_to(this->labels[WG::MiscCondition], this->misc_region, GraphletAnchor::CT, GraphletAnchor::CT, 0.0F, vgapsize);
			this->master->move_to(this->labels[WG::GantryCondition], this->gantry_region, GraphletAnchor::CT, GraphletAnchor::CT, 0.0F, vgapsize);
			this->master->move_to(this->labels[WG::WinchCondition], this->winch_region, GraphletAnchor::CT, GraphletAnchor::CT, 0.0F, vgapsize);
			this->master->move_to(this->labels[WG::WinchMetrics], this->metrics_region, GraphletAnchor::CT, GraphletAnchor::CT, 0.0F, vgapsize);
		}

		{ // reflow misc condition boxes
			IGraphlet* target = this->labels[WG::MiscCondition];

			for (WG id = this->misc_start; id <= this->misc_end; id++) {
				this->master->move_to(this->slots[id], target, GraphletAnchor::CB, GraphletAnchor::CT, 0.0F, vgapsize);
				target = this->slots[id];
			}
		}

		{ // reflow gantry condition boxes
			IGraphlet* target = this->labels[WG::GantryCondition];

			for (WG id = this->gantry_start; id <= this->gantry_end; id++) {
				this->master->move_to(this->slots[id], target, GraphletAnchor::CB, GraphletAnchor::CT, 0.0F, vgapsize);
				target = this->slots[id];
			}
		}

		{ // reflow winch condition boxes
			IGraphlet* target = this->labels[WG::WinchCondition];

			for (WG id = this->winch_start; id <= this->winch_end; id++) {
				this->master->move_to(this->slots[id], target, GraphletAnchor::CB, GraphletAnchor::CT, 0.0F, vgapsize);
				target = this->slots[id];
			}
		}

		{ // reflow winch metrics
			IGraphlet* target = this->labels[WG::WinchMetrics];

			for (WG id = this->metrics_start; id <= this->metrics_end; id++) {
				this->master->move_to(this->slots[id], target, GraphletAnchor::CB, GraphletAnchor::CT, 0.0F, vgapsize);
				target = this->slots[id];
			}
		}

		{ // reflow diagnostics
			float inset = vgapsize * this->inset_ratio;
			float icon_width;

			this->diagnoses[this->misc_start]->fill_extent(0.0F, 0.0F, &icon_width, nullptr);

			for (WG id = this->misc_start; id <= this->metrics_end; id++) {
				bool visible = true;

				switch (id) {
				case WG::NoSuction: case WG::HopperStopped: case WG::InflatingValveClosed: case WG::NoSlack: {
					visible = (this->master->get_id() == DxPosition::Trunnion);
				}; break;
				}

				if (!visible) {
					this->master->move_to(this->diagnoses[id], 0.0F, 0.0F);
					this->master->move_to(this->labels[id], 0.0F, 0.0F);
				} else {
					this->master->move_to(this->diagnoses[id], this->slots[id], GraphletAnchor::LC,
						GraphletAnchor::LC, (icon_width + vgapsize) * 1.0F + inset);

					this->master->move_to(this->labels[id], this->slots[id], GraphletAnchor::LC,
						GraphletAnchor::LC, (icon_width + vgapsize) * 3.0F + inset + vgapsize);
				}
			}
		}
	}

public:
	bool available() override {
		return (this->master->surface_ready() && this->master->shown());
	}

	void switch_position(DxPosition pos) {
		for (WG id = WG::GantryCondition; id <= WG::WinchMetrics; id++) {
			this->labels[id]->set_text(_speak(pos.ToString() + id.ToString()));
		}
	}

private:
	template<typename E>
	void load_label(std::map<E, Credit<Labellet, E>*>& ls, E id, CanvasSolidColorBrush^ color, CanvasTextFormat^ font = nullptr) {
		ls[id] = this->master->insert_one(new Credit<Labellet, E>(_speak(id), font, color), id);
	}

	template<typename E, typename P>
	void load_label(std::map<E, Credit<Labellet, E>*>& ls, E id, P prefix, CanvasSolidColorBrush^ color, CanvasTextFormat^ font = nullptr) {
		ls[id] = this->master->insert_one(new Credit<Labellet, E>(_speak(prefix.ToString() + id.ToString()), font, color), id);
	}

private: // never delete these graphlets mannually
	std::map<WG, Credit<Labellet, WG>*> labels;
	std::map<WG, Credit<Alarmlet, WG>*> diagnoses;
	std::map<WG, Credit<RoundedRectanglet, WG>*> slots;
	RoundedRectanglet* misc_region;
	RoundedRectanglet* gantry_region;
	RoundedRectanglet* winch_region;
	RoundedRectanglet* metrics_region;
	
private:
	CanvasSolidColorBrush^ color;
	CanvasSolidColorBrush^ diagnosis_color;
	CanvasTextFormat^ region_font;
	CanvasTextFormat^ diagnosis_font;

private:
	float diagnosis_height;
	float misc_region_height;
	float gantry_region_height;
	float winch_region_height;
	float metrics_region_height;
	float inset_ratio;

private:
	WG misc_start;
	WG misc_end;
	WG gantry_start;
	WG gantry_end;
	WG winch_start;
	WG winch_end;
	WG metrics_start;
	WG metrics_end;

private:
	DredgesDiagnostics* master;
	DX side;
};

DredgesDiagnostics::DredgesDiagnostics(DX side, PLCMaster* plc)
	: ICreditSatellite(plc->get_logger(), side.ToString() + "_" + __MODULE__), device(plc) {
	DredgesDx* dashboard = new DredgesDx(this, side);
	
	this->dashboard = dashboard;
	
	this->device->append_confirmation_receiver(dashboard);
}

DredgesDiagnostics::~DredgesDiagnostics() {
	if (this->dashboard != nullptr) {
		delete this->dashboard;
	}
}

void DredgesDiagnostics::fill_satellite_extent(float* width, float* height) {
	auto dashboard = dynamic_cast<DredgesDx*>(this->dashboard);
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

void DredgesDiagnostics::load(CanvasCreateResourcesReason reason, float width, float height) {
	auto dashboard = dynamic_cast<DredgesDx*>(this->dashboard);
	
	if (dashboard != nullptr) {
		auto caption_font = make_bold_text_format("Microsoft YaHei", large_font_size);
		
		dashboard->load(0.0F, width, height, this->title_height, this->vgapsize);
		
		this->titlebar = this->insert_one(new Rectanglet(width, this->title_height, Colours::make(diagnostics_caption_background)));
		this->title = this->insert_one(new Labellet(this->display_name(), caption_font, diagnostics_caption_foreground));
	}
}

void DredgesDiagnostics::reflow(float width, float height) {
	auto dashboard = dynamic_cast<DredgesDx*>(this->dashboard);

	if (dashboard != nullptr) {
		dashboard->reflow(width, height, this->title_height, this->vgapsize);
		this->move_to(this->title, this->titlebar, GraphletAnchor::CC, GraphletAnchor::CC);
	}
}

void DredgesDiagnostics::on_id_changed(DxPosition pos) {
	auto dashboard = dynamic_cast<DredgesDx*>(this->dashboard);

	if (dashboard != nullptr) {
		dashboard->switch_position(pos);
	}
}
