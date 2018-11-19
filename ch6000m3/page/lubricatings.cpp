#include <map>

#include "page/lubricatings.hpp"
#include "configuration.hpp"
#include "menu.hpp"

#include "module.hpp"
#include "text.hpp"
#include "paint.hpp"
#include "brushes.hxx"
#include "turtle.hpp"

#include "graphlet/shapelet.hpp"
#include "graphlet/dashboard/alarmlet.hpp"
#include "graphlet/symbol/pump/hydraulic_pumplet.hpp"

#include "schema/ai_pumps.hpp"

#include "schema/di_pumps.hpp"
#include "schema/di_hopper_pumps.hpp"

#include "schema/do_hopper_pumps.hpp"

#include "decorator/page.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI::Xaml::Controls;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

private enum LUMode { WindowUI = 0, Dashboard };

private enum class LUOperation { Start, Stop, _ };
private enum class LUGBOperation { Start, Stop, Auto, _ };

static CanvasSolidColorBrush^ region_background = Colours::make(0x212121U);
static CanvasSolidColorBrush^ alarm_background = Colours::make(0x181818U);

// WARNING: order matters
private enum class LU : unsigned int {
	// Group
	PS, SB,

	// Key Labels
	Gearbox, Unit, Master, Spare,

	// alarms
	Pressure, Level, Oil, Water,
	Bearing1, Bearing2, Bearing3,
	GearboxOil, GearboxPressure,
	
	_
};

private class Lubricatings final
	: public PLCConfirmation
	, public IMenuCommand<LUOperation, GroupCredit<HydraulicPumplet, LU, LU>, PLCMaster*>
	, public IMenuCommand<LUGBOperation, GroupCredit<HydraulicPumplet, LU, LU>, PLCMaster*> {
public:
	Lubricatings(LubricatingsPage* master, LU gid, unsigned int color) : master(master), gid(gid) {
		this->caption_font = make_bold_text_format("Microsoft YaHei", large_font_size);
		this->alarm_font = make_bold_text_format("Microsoft YaHei", normal_font_size);
		this->label_font = make_bold_text_format("Microsoft YaHei", small_font_size);

		this->color = Colours::make(color);
	}

public:
	void pre_read_data(Syslog* logger) override {
		this->master->enter_critical_section();
		this->master->begin_update_sequence();
	}

	void on_digital_input(const uint8* DB4, size_t count4, const uint8* DB205, size_t count205, Syslog* logger) {
		unsigned int unit_feedback = ((this->gid == LU::PS) ? ps_hopper_lubricating_unit_feedback : sb_hopper_lubricating_unit_feedback);
		unsigned int master_feedback = ((this->gid == LU::PS) ? ps_hopper_gearbox_master_feedback : sb_hopper_gearbox_master_feedback);
		unsigned int spare_feedback = ((this->gid == LU::PS) ? ps_hopper_gearbox_spare_feedback : sb_hopper_gearbox_spare_feedback);
		unsigned int master_details = ((this->gid == LU::PS) ? ps_hopper_gearbox_master_details : sb_hopper_gearbox_master_details);
		unsigned int spare_details = ((this->gid == LU::PS) ? ps_hopper_gearbox_spare_details : sb_hopper_gearbox_spare_details);
		unsigned int unit_alarms = ((this->gid == LU::PS) ? ps_hopper_lubricating_unit_alarms : sb_hopper_lubricating_unit_alarms);
		unsigned int gearbox_alarms = ((this->gid == LU::PS) ? ps_hopper_gearbox_alarms : sb_hopper_gearbox_alarms);

		DI_hopper_pump_lubricating_unit(this->pumps[LU::Unit], DB4, unit_feedback, DB205, unit_feedback);
		DI_hopper_pump_gearbox(this->pumps[LU::Master], DB4, master_feedback, DB205, master_details);
		DI_hopper_pump_gearbox(this->pumps[LU::Spare], DB4, spare_feedback, DB205, spare_details);

		DI_hopper_lubricating_unit_alarm(this->alarms[LU::Pressure], DB4, unit_alarms + 0U);
		DI_hopper_lubricating_unit_alarm(this->alarms[LU::Level],    DB4, unit_alarms + 1U);
		DI_hopper_lubricating_unit_alarm(this->alarms[LU::Oil],      DB4, unit_alarms + 2U);
		DI_hopper_lubricating_unit_alarm(this->alarms[LU::Water],    DB4, unit_alarms + 3U);
		DI_hopper_lubricating_unit_alarm(this->alarms[LU::Bearing1], DB4, unit_alarms + 4U);
		DI_hopper_lubricating_unit_alarm(this->alarms[LU::Bearing2], DB4, unit_alarms + 5U);
		DI_hopper_lubricating_unit_alarm(this->alarms[LU::Bearing3], DB4, unit_alarms + 6U);

		DI_hopper_lubricating_unit_alarm(this->alarms[LU::GearboxOil],      DB4, gearbox_alarms + 0U);
		DI_hopper_lubricating_unit_alarm(this->alarms[LU::GearboxPressure], DB4, gearbox_alarms + 1U);
	}

	void post_read_data(Syslog* logger) override {
		this->master->end_update_sequence();
		this->master->leave_critical_section();
	}

public:
	bool can_execute(LUOperation cmd, GroupCredit<HydraulicPumplet, LU, LU>* pump, PLCMaster* plc, bool acc_executable) override {
		return plc->connected();
	}

	void execute(LUOperation cmd, GroupCredit<HydraulicPumplet, LU, LU>* pump, PLCMaster* plc) override {
		plc->send_command(DO_hopper_lubricating_unit_command(cmd, pump->gid == LU::PS));
	}

public:
	bool can_execute(LUGBOperation cmd, GroupCredit<HydraulicPumplet, LU, LU>* pump, PLCMaster* plc, bool acc_executable) override {
		return plc->connected();
	}

	void execute(LUGBOperation cmd, GroupCredit<HydraulicPumplet, LU, LU>* pump, PLCMaster* plc) override {
		plc->send_command(DO_hopper_gearbox_command(cmd, pump->id, pump->gid == LU::PS));
	}

public:
	void load(float x, float width, float height, float vinset) {
		Turtle<LU>* turtle = new Turtle<LU>(vinset, vinset);
		float region_width = width * 0.618F;
		float region_height = (height - vinset * 5.0F) * 0.5F;
		float corner_radius = 8.0F;
		float pump_radius = vinset * 1.618F;
		
		turtle->move_down()->move_left(4);
		turtle->move_down(4, LU::Master)->move_down(4);
		turtle->move_right(4)->move_down()->jump_up()->move_right(4);
		turtle->move_up(4, LU::Spare)->move_up(4)->move_left(4);

		this->unit = this->master->insert_one(new RoundedRectanglet(region_width, region_height, corner_radius, region_background));
		this->gearbox = this->master->insert_one(new RoundedRectanglet(region_width, region_height, corner_radius, region_background));
		this->station = this->master->insert_one(new Tracklet<LU>(turtle, default_pipe_thickness, default_pipe_color));

		this->load_label(this->labels, LU::Unit, this->color, this->caption_font);
		this->load_label(this->labels, LU::Gearbox, this->color, this->caption_font);
		this->load_devices(this->pumps, LU::Unit, LU::Spare, pump_radius);

		{ // load alarms
			float alarm_box_width = (region_width - vinset * 3.0F) * 0.5F;
			float alarm_box_height = vinset * 2.0F;
			float alarm_size = alarm_box_height * 0.618F;
		
			for (LU id = LU::Pressure; id <= LU::GearboxPressure; id++) {
				this->boxes[id] = this->master->insert_one(new Credit<RoundedRectanglet, LU>(
					alarm_box_width, alarm_box_height, corner_radius, alarm_background), id);

				this->alarms[id] = this->master->insert_one(new Credit<Alarmlet, LU>(alarm_size), id);
				this->load_label(this->labels, id, Colours::Silver, this->alarm_font);
			}
		}
	}

public:
	void reflow(float x, float width, float height, float vinset) {
		float cx = x + width * 0.5F;
		float gapsize = vinset * 0.5F;

		this->master->move_to(this->unit, cx, vinset * 2.0F, GraphletAnchor::CT);
		this->master->move_to(this->gearbox, cx, height - vinset * 2.0F, GraphletAnchor::CB);
		this->master->move_to(this->pumps[LU::Unit], this->unit, 0.5F, 0.24F, GraphletAnchor::CC);
		this->master->move_to(this->station, this->gearbox, 0.5F, 0.42F, GraphletAnchor::CC);

		this->station->map_credit_graphlet(this->pumps[LU::Master], GraphletAnchor::CC);
		this->station->map_credit_graphlet(this->pumps[LU::Spare], GraphletAnchor::CC);

		this->master->move_to(this->labels[LU::Master], this->pumps[LU::Master], GraphletAnchor::CT, GraphletAnchor::RB, -gapsize);
		this->master->move_to(this->labels[LU::Spare], this->pumps[LU::Spare], GraphletAnchor::CT, GraphletAnchor::LB, +gapsize);

		{ // reflow alarm boxes
			float region_bottom, object_bottom;
			float alarm_cy = 0.0F;

			this->master->fill_graphlet_location(this->unit, nullptr, &region_bottom, GraphletAnchor::CB);
			this->master->fill_graphlet_location(this->pumps[LU::Unit], nullptr, &object_bottom, GraphletAnchor::CB);

			alarm_cy = object_bottom + (region_bottom - object_bottom) * 0.5F;
			this->master->move_to(this->boxes[LU::Level], cx, alarm_cy, GraphletAnchor::RB, -gapsize, -gapsize * 0.5F);
			this->master->move_to(this->boxes[LU::Pressure], this->boxes[LU::Level], GraphletAnchor::LT, GraphletAnchor::LB, 0.0F, -gapsize);
			this->master->move_to(this->boxes[LU::Oil], cx, alarm_cy, GraphletAnchor::RT, -gapsize, +gapsize * 0.5F);
			this->master->move_to(this->boxes[LU::Water], this->boxes[LU::Oil], GraphletAnchor::LB, GraphletAnchor::LT, 0.0F, +gapsize);

			this->master->move_to(this->boxes[LU::Bearing1], this->boxes[LU::Pressure], GraphletAnchor::RC, GraphletAnchor::LC, vinset);
			this->master->move_to(this->boxes[LU::Bearing2], this->boxes[LU::Bearing1], GraphletAnchor::LB, GraphletAnchor::LT, 0.0F, gapsize);
			this->master->move_to(this->boxes[LU::Bearing3], this->boxes[LU::Bearing2], GraphletAnchor::LB, GraphletAnchor::LT, 0.0F, gapsize);

			this->master->fill_graphlet_location(this->gearbox, nullptr, &region_bottom, GraphletAnchor::CB);
			this->master->fill_graphlet_location(this->station, nullptr, &object_bottom, GraphletAnchor::CB);

			alarm_cy = object_bottom + (region_bottom - object_bottom) * 0.5F;
			this->master->move_to(this->boxes[LU::GearboxOil], cx, alarm_cy, GraphletAnchor::RC, -gapsize);
			this->master->move_to(this->boxes[LU::GearboxPressure], cx, alarm_cy, GraphletAnchor::LC, gapsize);
		}

		for (LU id = LU::Pressure; id <= LU::GearboxPressure; id++) {
			this->master->move_to(this->alarms[id], this->boxes[id], GraphletAnchor::LC, GraphletAnchor::LC, vinset);
			this->master->move_to(this->labels[id], this->alarms[id], GraphletAnchor::RC, GraphletAnchor::LC, vinset);
		}

		this->master->move_to(this->labels[LU::Unit], this->pumps[LU::Unit], GraphletAnchor::CT, GraphletAnchor::CB, 0.0F, -vinset);
		this->master->move_to(this->labels[LU::Gearbox], this->station, GraphletAnchor::CT, GraphletAnchor::CB, 0.0F, -vinset);
	}

private:
	template<class P, typename E>
	void load_devices(std::map<E, P*>& ps, E id0, E idn, float radius) {
		for (E id = id0; id <= idn; id++) {
			ps[id] = this->master->insert_one(new P(radius, -90.0), this->gid, id);

			if (id != id0) {
				this->load_label(this->labels, id, Colours::Silver, this->label_font);
			}
		}
	}

	template<typename E>
	void load_label(std::map<E, Credit<Labellet, E>*>& ls, E id, CanvasSolidColorBrush^ color, CanvasTextFormat^ font = nullptr) {
		ls[id] = this->master->insert_one(new Credit<Labellet, E>(_speak(id), font, color), id);
	}

// never deletes these graphlets mannually
private:
	Tracklet<LU>* station;
	std::map<LU, Credit<Labellet, LU>*> labels;
	std::map<LU, GroupCredit<HydraulicPumplet, LU, LU>*> pumps;
	std::map<LU, Credit<Alarmlet, LU>*> alarms;
	std::map<LU, Credit<RoundedRectanglet, LU>*> boxes;
	RoundedRectanglet* unit;
	RoundedRectanglet* gearbox;
	
private:
	CanvasSolidColorBrush^ color;
	CanvasTextFormat^ caption_font;
	CanvasTextFormat^ label_font;
	CanvasTextFormat^ alarm_font;

private:
	LubricatingsPage* master;
	LU gid;
};

LubricatingsPage::LubricatingsPage(PLCMaster* plc) : Planet(__MODULE__), device(plc) {
	Lubricatings* ps_dashboard = new Lubricatings(this, LU::PS, default_ps_color);
	Lubricatings* sb_dashboard = new Lubricatings(this, LU::SB, default_sb_color);

	this->ps_dashboard = ps_dashboard;
	this->sb_dashboard = sb_dashboard;

	this->ps_unit_op = make_menu<LUOperation, GroupCredit<HydraulicPumplet, LU, LU>, PLCMaster*>(ps_dashboard, plc);
	this->ps_gearbox_op = make_menu<LUGBOperation, GroupCredit<HydraulicPumplet, LU, LU>, PLCMaster*>(ps_dashboard, plc);
	this->sb_unit_op = make_menu<LUOperation, GroupCredit<HydraulicPumplet, LU, LU>, PLCMaster*>(sb_dashboard, plc);
	this->sb_gearbox_op = make_menu<LUGBOperation, GroupCredit<HydraulicPumplet, LU, LU>, PLCMaster*>(sb_dashboard, plc);
	
	this->device->append_confirmation_receiver(ps_dashboard);
	this->device->append_confirmation_receiver(sb_dashboard);
	this->append_decorator(new PageDecorator());
}

LubricatingsPage::~LubricatingsPage() {
	if (this->ps_dashboard != nullptr) {
		delete this->ps_dashboard;
		delete this->sb_dashboard;
	}
}

void LubricatingsPage::load(CanvasCreateResourcesReason reason, float width, float height) {
	auto ps_dashboard = dynamic_cast<Lubricatings*>(this->ps_dashboard);
	auto sb_dashboard = dynamic_cast<Lubricatings*>(this->sb_dashboard);
	
	if ((ps_dashboard != nullptr) && (sb_dashboard)) {
		float vinset = statusbar_height();
		float half_width = width * 0.5F;

		{ // load graphlets
			this->change_mode(LUMode::Dashboard);
			ps_dashboard->load(0.0F,       half_width, height, vinset);
			sb_dashboard->load(half_width, half_width, height, vinset);

			this->change_mode(LUMode::WindowUI);
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

void LubricatingsPage::reflow(float width, float height) {
	auto ps_dashboard = dynamic_cast<Lubricatings*>(this->ps_dashboard);
	auto sb_dashboard = dynamic_cast<Lubricatings*>(this->sb_dashboard);

	if ((ps_dashboard != nullptr) && (sb_dashboard != nullptr)) {
		float vinset = statusbar_height();
		float half_width = width * 0.5F;
		
		this->change_mode(LUMode::WindowUI);
		this->move_to(this->statusline, 0.0F, height, GraphletAnchor::LB);

		this->change_mode(LUMode::Dashboard);
		ps_dashboard->reflow(0.0F,       half_width, height, vinset);
		sb_dashboard->reflow(half_width, half_width, height, vinset);
	}
}

bool LubricatingsPage::can_select(IGraphlet* g) {
	return ((dynamic_cast<HydraulicPumplet*>(g) != nullptr));
}

void LubricatingsPage::on_tap_selected(IGraphlet* g, float local_x, float local_y) {
	auto pump = dynamic_cast<GroupCredit<HydraulicPumplet, LU, LU>*>(g);
	
	if (pump != nullptr) {
		if (pump->gid == LU::PS) {
			if (pump->id == LU::Unit) {
				menu_popup(this->ps_unit_op, g, local_x, local_y);
			} else {
				menu_popup(this->ps_gearbox_op, g, local_x, local_y);
			}
		} else {
			if (pump->id == LU::Unit) {
				menu_popup(this->ps_unit_op, g, local_x, local_y);
			} else {
				menu_popup(this->ps_gearbox_op, g, local_x, local_y);
			}
		}
	}
}
