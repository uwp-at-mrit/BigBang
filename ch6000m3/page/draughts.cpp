#include <map>

#include "page/draughts.hpp"
#include "configuration.hpp"
#include "menu.hpp"

#include "graphlet/dashboard/cylinderlet.hpp"
#include "graphlet/dashboard/timeserieslet.hpp"
#include "graphlet/device/overflowlet.hpp"
#include "graphlet/buttonlet.hpp"

#include "iotables/ai_metrics.hpp"

#include "iotables/di_doors.hpp"

#include "iotables/do_devices.hpp"
#include "iotables/do_doors.hpp"

#include "decorator/page.hpp"

#include "module.hpp"
#include "shape.hpp"
#include "geometry.hpp"
#include "transformation.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::System;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

private enum DLMode { WindowUI = 0, Dashboard };

private enum class DLTS { EarthWork, Vessel, HopperHeight, Loading, Displacement, _ };

// WARNING: order matters
private enum class DL : unsigned int {
	SternDraft, psSternDraft, psSuctionDraft, sbSternDraft, psSternHeight, sbSternHeight,
	BowDraft, psBowDraft, sbSuctionDraft, sbBowDraft, psBowHeight, sbBowHeight,

	Overflow, NetWeight,

	_
};

private class DraughtDecorator : public IPlanetDecorator {
public:
	DraughtDecorator() {
		float height = 0.618F * 0.618F;
		float radius = height * 0.5F;
		
		this->ship_width = 0.618F;
		this->x = (1.0F - this->ship_width - radius) * 0.5F;
		this->y = 0.5F;
		this->ship = geometry_union(rectangle(this->ship_width, height),
			segment(this->ship_width, radius, -90.0, 90.0, radius, radius));
	}

public:
	void draw_before(CanvasDrawingSession^ ds, float Width, float Height) override {
		auto real_ship = geometry_scale(this->ship, Width, Height);
		Rect ship_box = real_ship->ComputeBounds();
		float thickness = 2.0F;
		float sx = this->x * Width;
		float sy = this->y * Height;
		
		ds->DrawGeometry(real_ship, sx, sy, Colours::Silver, thickness);
	}

public:
	void fill_ship_extent(float* x, float* y, float* width, float* height, bool full = false) {
		float awidth = this->actual_width();
		float aheight = this->actual_height();
		auto abox = this->ship->ComputeBounds(make_scale_matrix(awidth, aheight));

		SET_VALUES(x, this->x * awidth, y, this->y * aheight);
		SET_BOX(width, (full ? abox.Width : this->ship_width * awidth));
		SET_BOX(height, abox.Height);
	}

	void fill_ship_anchor(float fx, float fy, float* x, float *y, bool full = false) {
		float awidth = this->actual_width();
		float aheight = this->actual_height();
		auto abox = this->ship->ComputeBounds(make_scale_matrix(awidth, aheight));
		float width = (full ? abox.Width : this->ship_width * awidth);

		SET_BOX(x, this->x * awidth + width * fx);
		SET_BOX(y, this->y * aheight + abox.Height * fy);
	}

private:
	CanvasGeometry^ ship;
	ICanvasBrush^ seq_color;

private:
	float x;
	float y;
	float ship_width;
};

private class Draughts final : public PLCConfirmation {
public:
	Draughts(DraughtsPage* master, DraughtDecorator* ship) : master(master), decorator(ship) {
		this->label_font = make_bold_text_format(large_font_size);
		this->plain_style = make_plain_dimension_style(small_metrics_font_size, 5U, 2U);
		this->metrics_style = make_plain_dimension_style(small_metrics_font_size, normal_font_size);
		this->netweight_style = make_plain_dimension_style(small_metrics_font_size, normal_font_size, 0U);
	}

public:
	void pre_read_data(Syslog* logger) override {
		this->master->enter_critical_section();
		this->master->begin_update_sequence();
	}

	void on_analog_input(const uint8* DB2, size_t count2, const uint8* DB203, size_t count203, Syslog* logger) override {
		this->overflowpipe->set_value(RealData(DB203, overflow_pipe_progress));
		this->overflowpipe->set_liquid_height(DBD(DB2, average_hopper_height));
		this->dimensions[DL::Overflow]->set_value(this->overflowpipe->get_value(), GraphletAnchor::CC);
		this->dimensions[DL::NetWeight]->set_value(DBD(DB2, vessal_netweight), GraphletAnchor::CC);

		this->dimensions[DL::psBowDraft]->set_value(DBD(DB2, ps_fixed_bow_draught));
		this->dimensions[DL::psSuctionDraft]->set_value(DBD(DB2, ps_suction_draught));
		this->dimensions[DL::psSternDraft]->set_value(DBD(DB2, ps_fixed_stern_draught));

		this->dimensions[DL::sbBowDraft]->set_value(DBD(DB2, sb_fixed_bow_draught));
		this->dimensions[DL::sbSuctionDraft]->set_value(DBD(DB2, sb_suction_draught));
		this->dimensions[DL::sbSternDraft]->set_value(DBD(DB2, sb_fixed_stern_draught));

		this->dimensions[DL::BowDraft]->set_value(DBD(DB2, fixed_bow_draught));
		this->dimensions[DL::SternDraft]->set_value(DBD(DB2, fixed_stern_draught));
		this->dimensions[DL::psBowHeight]->set_value(DBD(DB2, ps_bow_hopper_height));
		this->dimensions[DL::sbBowHeight]->set_value(DBD(DB2, sb_bow_hopper_height));
		this->dimensions[DL::psSternHeight]->set_value(DBD(DB2, ps_stern_hopper_height));
		this->dimensions[DL::sbSternHeight]->set_value(DBD(DB2, sb_stern_hopper_height));

		{ // set timeseries in batch
			double values[_N(DLTS)];

			this->set_cylinder(DLTS::HopperHeight, values, DBD(DB2, average_hopper_height));
			this->set_cylinder(DLTS::Displacement, values, DBD(DB2, displacement_value));
			this->set_cylinder(DLTS::Loading, values, DBD(DB2, loading_value));
			this->set_cylinder(DLTS::EarthWork, values, DBD(DB2, earthwork_value));
			this->set_cylinder(DLTS::Vessel, values, DBD(DB2, vessel_value));

			this->timeseries->set_values(values);
		}
	}

	void on_digital_input(const uint8* DB4, size_t count4, const uint8* DB205, size_t count205, Syslog* logger) override {
		DI_hopper_doors_checks_button(this->hdchecks[BottomDoorCommand::OpenDoorCheck], BottomDoorCommand::OpenDoorCheck, DB205);
		DI_hopper_doors_checks_button(this->hdchecks[BottomDoorCommand::CloseDoorCheck], BottomDoorCommand::CloseDoorCheck, DB205);
	}

	void post_read_data(Syslog* logger) override {
		this->master->end_update_sequence();
		this->master->leave_critical_section();
	}

public:
	void load(float width, float height, float vinset) {
		float ship_y, ship_height, cylinder_height, lines_width, lines_height;
		
		this->decorator->fill_ship_extent(nullptr, &ship_y, &lines_width, &ship_height, true);
		
		lines_height = ship_y * 0.618F;
		this->timeseries = this->master->insert_one(new TimeSerieslet<DLTS>(__MODULE__,
			timeseries_range, make_hour_series(6U, 5U), lines_width, lines_height, 5U, 0U));

		this->overflowpipe = this->master->insert_one(new OverflowPipelet(hopper_height_range, ship_height * 0.618F));

		cylinder_height = ship_height * 0.42F;
		this->load_cylinder(this->cylinders, DLTS::EarthWork, cylinder_height, earthwork_range, 0U, "meter3");
		this->load_cylinder(this->cylinders, DLTS::Vessel, cylinder_height, vessel_range, 0U, "meter3");
		this->load_cylinder(this->cylinders, DLTS::HopperHeight, cylinder_height, hopper_height_range, 2U, "meter");
		this->load_cylinder(this->cylinders, DLTS::Loading, cylinder_height, loading_range, 0U, "ton");
		this->load_cylinder(this->cylinders, DLTS::Displacement, cylinder_height, displacement_range, 0U, "ton");

		this->load_dimensions(this->dimensions, DL::SternDraft, DL::sbSternHeight, "meter");
		this->load_dimensions(this->dimensions, DL::BowDraft, DL::sbBowHeight, "meter");
		this->load_dimension(this->dimensions, DL::Overflow, "meter", this->plain_style);
		this->load_dimension(this->dimensions, DL::NetWeight, "ton", this->netweight_style);

		this->load_buttons(this->hdchecks, BottomDoorCommand::OpenDoorCheck, BottomDoorCommand::CloseDoorCheck);
	}

	void reflow(float width, float height, float vinset) {
		float tsx, tsy, ofpx, ofpy, gapsize;

		this->dimensions[DL::Overflow]->fill_extent(0.0F, 0.0F, nullptr, &gapsize);
		gapsize *= 0.5F;

		this->decorator->fill_ship_anchor(0.9F, 0.5F, &ofpx, &ofpy);
		this->decorator->fill_ship_anchor(0.5F, 0.0F, &tsx, &tsy, true);
		tsy *= 0.5F;

		this->reflow_cylinders(this->cylinders, DLTS::EarthWork, DLTS::Displacement, gapsize);
		this->master->move_to(this->timeseries, tsx, tsy, GraphletAnchor::CC);
		this->master->move_to(this->overflowpipe, ofpx, ofpy, GraphletAnchor::CC, 0.0F, -gapsize);
		this->master->move_to(this->dimensions[DL::Overflow], this->overflowpipe, GraphletAnchor::CB, GraphletAnchor::CT, 0.0F, gapsize);

		{ // reflow dimensions
			float cpt_height, xoff, yoff;

			this->dimensions[DL::BowDraft]->fill_extent(0.0F, 0.0F, nullptr, &cpt_height);
			xoff = cpt_height * 0.50F;
			yoff = cpt_height * 0.20F;

			this->reflow_dimension(this->dimensions, DL::BowDraft, 1.0F, 0.5F, GraphletAnchor::RC, -xoff, 0.0F, true);

			this->reflow_dimension(this->dimensions, DL::psBowDraft, 1.0F, 0.0F, GraphletAnchor::RB, -yoff, -yoff);
			this->reflow_dimension(this->dimensions, DL::psBowHeight, 1.0F, 0.0F, GraphletAnchor::RT, -yoff, yoff);
			this->reflow_dimension(this->dimensions, DL::sbBowDraft, 1.0F, 1.0F, GraphletAnchor::RT, -yoff, yoff);
			this->reflow_dimension(this->dimensions, DL::sbBowHeight, 1.0F, 1.0F, GraphletAnchor::RB, -yoff, -yoff);
			
			this->reflow_dimension(this->dimensions, DL::psSuctionDraft, 0.618F, 0.0F, GraphletAnchor::CB, 0.0F, -yoff);
			this->reflow_dimension(this->dimensions, DL::sbSuctionDraft, 0.618F, 1.0F, GraphletAnchor::CT, 0.0F, yoff);

			this->reflow_dimension(this->dimensions, DL::psSternDraft, 0.0F, 0.0F, GraphletAnchor::LB, yoff, -yoff);
			this->reflow_dimension(this->dimensions, DL::psSternHeight, 0.0F, 0.0F, GraphletAnchor::LT, yoff, yoff);
			this->reflow_dimension(this->dimensions, DL::sbSternDraft, 0.0F, 1.0F, GraphletAnchor::LT, yoff, yoff);
			this->reflow_dimension(this->dimensions, DL::sbSternHeight, 0.0F, 1.0F, GraphletAnchor::LB, yoff, -yoff);

			this->reflow_dimension(this->dimensions, DL::SternDraft, 0.0F, 0.5F, GraphletAnchor::RC, -xoff);
		}

		{ // reflow buttons and relevant dimensions
			IGraphlet* target = this->cydimensions[DLTS::HopperHeight];
			
			gapsize = vinset * 1.0F;
			this->master->move_to(this->hdchecks[BottomDoorCommand::OpenDoorCheck], target, GraphletAnchor::CB, GraphletAnchor::RT, -gapsize, gapsize);
			this->master->move_to(this->hdchecks[BottomDoorCommand::CloseDoorCheck], target, GraphletAnchor::CB, GraphletAnchor::LT, +gapsize, gapsize);
			this->master->move_to(this->dimensions[DL::NetWeight], this->cylabels[DLTS::HopperHeight], GraphletAnchor::CT, GraphletAnchor::CB, 0.0F, -gapsize);
		}
	}

private:
	template<typename E>
	void load_dimension(std::map<E, Credit<Dimensionlet, E>*>& ds, E id, Platform::String^ unit, DimensionStyle& style) {
		ds[id] = this->master->insert_one(new Credit<Dimensionlet, E>(style, unit, _speak(id)), id);
	}

	template<typename E>
	void load_dimensions(std::map<E, Credit<Dimensionlet, E>*>& ds, E id0, E idn, Platform::String^ unit) {
		for (E id = id0; id <= idn; id++) {
			this->load_dimension(ds, id, unit, this->plain_style);
		}
	}

	template<typename E>
	void load_dimension(std::map<E, Credit<Dimensionlet, E>*>& ds, std::map<E, Credit<Labellet, E>*>& ls, E id
		, Platform::String^ unit, unsigned int precision = 2U) {
		this->metrics_style.precision = precision;

		ls[id] = this->master->insert_one(new Credit<Labellet, E>(_speak(id), this->label_font, Colours::Silver), id);
		ds[id] = this->master->insert_one(new Credit<Dimensionlet, E>(this->metrics_style, unit), id);
	}

	template<typename E>
	void load_dimensions(std::map<E, Credit<Dimensionlet, E>*>& ds, std::map<E, Credit<Labellet, E>*>& ls, E id0, E idn, Platform::String^ unit) {
		for (E id = id0; id <= idn; id++) {
			this->load_dimension(ds, ls, id, unit);
		}
	}

	template<typename E>
	void load_cylinder(std::map<E, Credit<Cylinderlet, E>*>& cs, E id, float height, double range
		, unsigned int precision, Platform::String^ unit) {
		cs[id] = new Credit<Cylinderlet, E>(LiquidSurface::Convex, range, height * 0.2718F, height, 3.0F, 8U, precision);
		
		this->master->insert_one(cs[id], id);
		this->load_dimension(this->cydimensions, this->cylabels, id, unit, precision);
	}

	template<class B, typename CMD>
	void load_buttons(std::map<CMD, Credit<B, CMD>*>& bs, CMD cmd0, CMD cmdn, float width = 128.0F, float height = 32.0F) {
		for (CMD cmd = cmd0; cmd <= cmdn; cmd++) {
			bs[cmd] = this->master->insert_one(new Credit<B, CMD>(cmd.ToString(), width, height), cmd);
		}
	}

private:
	template<class C, typename E>
	void reflow_cylinders(std::map<E, Credit<C, E>*>& is, E id0, E idn, float gapsize) {
		float flcount = float(_I(idn) - _I(id0) + 1 + 4);
		float x, y;

		for (E id = id0; id <= idn; id++) {
			this->decorator->fill_ship_anchor(float(_I(id) - _I(id0) + 1) / flcount, 0.5F, &x, &y);

			this->master->move_to(is[id], x, y, GraphletAnchor::LC);
			this->master->move_to(this->cylabels[id], is[id], GraphletAnchor::CT, GraphletAnchor::CB, 0.0F, -gapsize);
			this->master->move_to(this->cydimensions[id], is[id], GraphletAnchor::CB, GraphletAnchor::CT, 0.0F, +gapsize);
		}
	}

	template<typename E>
	void reflow_dimension(std::map<E, Credit<Dimensionlet, E>*>& ds, E id , float fx, float fy
		, GraphletAnchor a, float dx = 0.0F, float dy = 0.0F, bool full = false) {
		float ax, ay;

		this->decorator->fill_ship_anchor(fx, fy, &ax, &ay, full);
		this->master->move_to(ds[id], ax, ay, a, dx, dy);
	}

private:
	void set_cylinder(DLTS id, double* values, float value) {
		this->cylinders[id]->set_value(value);
		this->cydimensions[id]->set_value(value, GraphletAnchor::CC);
		values[_I(id)] = value;
	}

private: // never delete these graphlets manually.
	std::map<DL, Credit<Labellet, DL>*> labels;
	std::map<DL, Credit<Percentagelet, DL>*> progresses;
	std::map<DL, Credit<Dimensionlet, DL>*> dimensions;
	std::map<DLTS, Credit<Labellet, DLTS>*> cylabels;
	std::map<DLTS, Credit<Dimensionlet, DLTS>*> cydimensions;
	std::map<DLTS, Credit<Cylinderlet, DLTS>*> cylinders;
	std::map<BottomDoorCommand, Credit<Buttonlet, BottomDoorCommand>*> hdchecks;
	TimeSerieslet<DLTS>* timeseries;
	OverflowPipelet* overflowpipe;

private:
	CanvasTextFormat^ label_font;
	DimensionStyle plain_style;
	DimensionStyle netweight_style;
	DimensionStyle metrics_style;

private:
	DraughtsPage* master;
	DraughtDecorator* decorator;
};

/*************************************************************************************************/
DraughtsPage::DraughtsPage(PLCMaster* plc) : Planet(__MODULE__), device(plc) {
	DraughtDecorator* decorator = new DraughtDecorator();
	Draughts* dashboard = new Draughts(this, decorator);

	this->dashboard = dashboard;
	this->overflow_op = make_overflow_menu(plc);

	this->device->append_confirmation_receiver(dashboard);

	this->append_decorator(new PageDecorator());
	this->append_decorator(decorator);
}

DraughtsPage::~DraughtsPage() {
	if (this->dashboard != nullptr) {
		delete this->dashboard;
	}
}

void DraughtsPage::load(CanvasCreateResourcesReason reason, float width, float height) {
	auto db = dynamic_cast<Draughts*>(this->dashboard);

	if (db != nullptr) {
		float vinset = statusbar_height();

		{ // load graphlets
			this->change_mode(DLMode::Dashboard);
			db->load(width, height, vinset);

			this->change_mode(DLMode::WindowUI);
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

void DraughtsPage::reflow(float width, float height) {
	auto db = dynamic_cast<Draughts*>(this->dashboard);
	
	if (db != nullptr) {
		float vinset = statusbar_height();

		this->change_mode(DLMode::WindowUI);
		this->move_to(this->statusline, 0.0F, height, GraphletAnchor::LB);
		
		this->change_mode(DLMode::Dashboard);
		db->reflow(width, height, vinset);
	}
}

bool DraughtsPage::can_select(IGraphlet* g) {
	bool okay = false;

	if (this->device->get_mode() != PLCMasterMode::User) {
		auto hdchecker = dynamic_cast<Buttonlet*>(g);

		okay = ((dynamic_cast<OverflowPipelet*>(g) != nullptr)
			|| ((hdchecker != nullptr) && (hdchecker->get_status() != ButtonStatus::Disabled)));
	}

	okay = okay || (dynamic_cast<ITimeSerieslet*>(g) != nullptr);

	return okay;
}

bool DraughtsPage::can_select_multiple() {
	return true;
}

void DraughtsPage::on_focus(IGraphlet* g) {
	auto timeseries = dynamic_cast<ITimeSerieslet*>(g);

	if (timeseries != nullptr) {
		this->show_virtual_keyboard(ScreenKeyboard::Arrowpad, timeseries, GraphletAnchor::CB, 0.0F, 4.0F);
	}
}

void DraughtsPage::on_tap_selected(IGraphlet* g, float local_x, float local_y) {
	auto overflow = dynamic_cast<OverflowPipelet*>(g);
	auto hdchecker = dynamic_cast<Credit<Buttonlet, BottomDoorCommand>*>(g);
	
	if (overflow != nullptr) {
		menu_popup(this->overflow_op, g, local_x, local_y);
	} else if (hdchecker != nullptr) {
		this->device->send_command(DO_bottom_doors_special_command(hdchecker->id));
	}
}

void DraughtsPage::on_gesture(std::list<Windows::Foundation::Numerics::float2>& points, float x, float y) {
	//this->get_logger()->log_message(Log::Info, L"(%f, %f)", x, y);
}
