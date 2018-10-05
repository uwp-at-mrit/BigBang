#include <map>

#include "page/draughts.hpp"
#include "configuration.hpp"
#include "menu.hpp"

#include "graphlet/dashboard/cylinderlet.hpp"
#include "graphlet/dashboard/timeserieslet.hpp"
#include "graphlet/device/overflowlet.hpp"

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

private enum class DLTS { EarthWork, Vessel, Loading, Displacement, Draught, _ };

// WARNING: order matters
private enum class DL : unsigned int {
	EarthWork, Vessel, HopperHeight, Loading, Displacement,
	
	SternDraft, psSternDraft, psSuctionDraft, sbSternDraft, psSternHeight, sbSternHeight,
	BowDraft, psBowDraft, sbSuctionDraft, sbBowDraft, psBowHeight, sbBowHeight,

	OverflowPipe,

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
		this->plain_style = make_plain_dimension_style(normal_font_size, 5U);
	}

public:
	void on_analog_input_data(const uint8* AI_DB203, size_t count, Syslog* logger) override {
		this->master->enter_critical_section();
		this->master->begin_update_sequence();

		this->overflowpipe->set_value(RealData(AI_DB203, 55));
		this->dimensions[DL::OverflowPipe]->set_value(RealData(AI_DB203, 55));

		this->dimensions[DL::psBowDraft]->set_value(RealData(AI_DB203, 31));
		this->dimensions[DL::psSuctionDraft]->set_value(RealData(AI_DB203, 32));
		this->dimensions[DL::psSternDraft]->set_value(RealData(AI_DB203, 102));

		this->dimensions[DL::sbBowDraft]->set_value(RealData(AI_DB203, 46));
		this->dimensions[DL::sbSuctionDraft]->set_value(RealData(AI_DB203, 47));
		this->dimensions[DL::sbSternDraft]->set_value(RealData(AI_DB203, 103));

		this->master->end_update_sequence();
		this->master->leave_critical_section();
	}

	void on_realtime_data(const uint8* DB2, size_t count, Syslog* logger) override {
		this->master->enter_critical_section();
		this->master->begin_update_sequence();

		this->timeseries->set_value(DLTS::Draught, DBD(DB2, 192U));

		this->dimensions[DL::BowDraft]->set_value(DBD(DB2, 164U));
		this->dimensions[DL::SternDraft]->set_value(DBD(DB2, 188U));
		this->dimensions[DL::psBowHeight]->set_value(DBD(DB2, 208U));
		this->dimensions[DL::sbBowHeight]->set_value(DBD(DB2, 212U));
		this->dimensions[DL::psSternHeight]->set_value(DBD(DB2, 216U));
		this->dimensions[DL::sbSternHeight]->set_value(DBD(DB2, 220U));

		this->overflowpipe->set_liquid_height(DBD(DB2, 224U));
		this->set_cylinder(DL::HopperHeight, DBD(DB2, 224U));
		this->set_cylinder(DL::Displacement, DLTS::Displacement, DBD(DB2, 228U));
		this->set_cylinder(DL::Loading, DLTS::Loading, DBD(DB2, 232U));
		this->set_cylinder(DL::EarthWork, DLTS::EarthWork, DBD(DB2, 236U));
		this->set_cylinder(DL::Vessel, DLTS::Vessel, DBD(DB2, 320U));

		this->master->end_update_sequence();
		this->master->leave_critical_section();
	}

public:
	void load(float width, float height, float vinset) {
		float ship_y, ship_height, cylinder_height, lines_width, lines_height, overflow_size;
		
		this->decorator->fill_ship_extent(nullptr, &ship_y, &lines_width, &ship_height, true);
		
		lines_height = ship_y * 0.618F;
		this->timeseries = this->master->insert_one(new TimeSerieslet<DLTS>(__MODULE__, 18000.0, lines_width, lines_height));

		overflow_size = ship_height * 0.618F;
		this->overflowpipe = this->master->insert_one(new OverflowPipelet(15.0, overflow_size, overflow_size));

		cylinder_height = ship_height * 0.42F;
		this->load_cylinder(this->cylinders, DL::EarthWork, cylinder_height, 15000.0, "meter3", LiquidSurface::_);
		this->load_cylinder(this->cylinders, DL::Vessel, cylinder_height, 15000.0, "meter3", LiquidSurface::_);
		this->load_cylinder(this->cylinders, DL::HopperHeight, cylinder_height, 15.0, "meter", LiquidSurface::Convex);
		this->load_cylinder(this->cylinders, DL::Loading, cylinder_height, 18000.0, "ton", LiquidSurface::_);
		this->load_cylinder(this->cylinders, DL::Displacement, cylinder_height, 4000.0, "ton", LiquidSurface::Convex);

		this->load_dimensions(this->dimensions, DL::SternDraft, DL::sbSternHeight, "meter");
		this->load_dimensions(this->dimensions, DL::BowDraft, DL::sbBowHeight, "meter");
		this->load_dimension(this->dimensions, DL::OverflowPipe, "meter");
	}

	void reflow(float width, float height, float vinset) {
		float tsx, tsy, ofpx, ofpy, gapsize;

		this->dimensions[DL::OverflowPipe]->fill_extent(0.0F, 0.0F, nullptr, &gapsize);
		gapsize *= 0.5F;

		this->decorator->fill_ship_anchor(0.9F, 0.5F, &ofpx, &ofpy);
		this->decorator->fill_ship_anchor(0.5F, 0.0F, &tsx, &tsy, true);
		tsy *= 0.5F;

		this->reflow_cylinders(this->cylinders, this->dimensions, this->captions, DL::EarthWork, DL::Displacement, gapsize);
		this->master->move_to(this->timeseries, tsx, tsy, GraphletAnchor::CC);
		this->master->move_to(this->overflowpipe, ofpx, ofpy, GraphletAnchor::CC, 0.0F, -gapsize);
		this->master->move_to(this->dimensions[DL::OverflowPipe], this->overflowpipe, GraphletAnchor::CB, GraphletAnchor::CT, 0.0F, gapsize);

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
	}

public:
	bool on_char(VirtualKey key, IMRMaster* plc) {
		bool handled = false;

		return handled;
	}

private:
	template<typename E>
	void load_dimension(std::map<E, Credit<Dimensionlet, E>*>& ds, E id, Platform::String^ unit) {
		ds[id] = this->master->insert_one(new Credit<Dimensionlet, E>(this->plain_style, unit, _speak(id)), id);
	}
	
	template<typename E>
	void load_dimension(std::map<E, Credit<Dimensionlet, E>*>& ds, std::map<E, Credit<Labellet, E>*>& ls, E id, Platform::String^ unit) {
		this->load_label(ls, id, Colours::Silver);
		ds[id] = this->master->insert_one(new Credit<Dimensionlet, E>(unit), id);
	}

	template<typename E>
	void load_dimensions(std::map<E, Credit<Dimensionlet, E>*>& ds, E id0, E idn, Platform::String^ unit) {
		for (E id = id0; id <= idn; id++) {
			this->load_dimension(ds, id, unit);
		}
	}

	template<typename E>
	void load_dimensions(std::map<E, Credit<Dimensionlet, E>*>& ds, std::map<E, Credit<Labellet, E>*>& ls, E id0, E idn, Platform::String^ unit) {
		for (E id = id0; id <= idn; id++) {
			this->load_dimension(ds, ls, id, unit);
		}
	}

	template<typename E>
	void load_cylinder(std::map<E, Credit<Cylinderlet, E>*>& cs, E id, float height, double range
		, Platform::String^ unit, LiquidSurface surface) {
		auto cylinder = new Credit<Cylinderlet, E>(surface, range, height * 0.2718F, height);

		cs[id] = this->master->insert_one(cylinder, id);

		this->load_dimension(this->dimensions, this->captions, id, unit);
	}

	template<typename E>
	void load_label(std::map<E, Credit<Labellet, E>*>& ls, E id, ICanvasBrush^ color, CanvasTextFormat^ font = nullptr) {
		ls[id] = this->master->insert_one(new Credit<Labellet, E>(_speak(id), font, color), id);
	}

private:
	template<class C, typename E>
	void reflow_cylinders(std::map<E, Credit<C, E>*>& is, std::map<E, Credit<Dimensionlet, E>*>& ds
		, std::map<E, Credit<Labellet, E>*>& ls, E id0, E idn, float gapsize) {
		float flcount = float(_I(idn) - _I(id0) + 1 + 4);
		float x, y;

		for (E id = id0; id <= idn; id++) {
			this->decorator->fill_ship_anchor(float(_I(id) - _I(id0) + 1) / flcount, 0.5F, &x, &y);

			this->master->move_to(is[id], x, y, GraphletAnchor::LC);
			this->master->move_to(ls[id], is[id], GraphletAnchor::CT, GraphletAnchor::CB, 0.0F, -gapsize);
			this->master->move_to(ds[id], is[id], GraphletAnchor::CB, GraphletAnchor::CT, 0.0F, gapsize);
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
	void set_cylinder(DL id, float value) {
		this->cylinders[id]->set_value(value);
		this->dimensions[id]->set_value(value);
	}

	void set_cylinder(DL id, DLTS ts_id, float value) {
		this->set_cylinder(id, value);
		this->timeseries->set_value(ts_id, value);
	}

private: // never delete these graphlets manually.
	std::map<DL, Credit<Labellet, DL>*> captions;
	std::map<DL, Credit<Percentagelet, DL>*> progresses;
	std::map<DL, Credit<Dimensionlet, DL>*> dimensions;
	std::map<DL, Credit<Cylinderlet, DL>*> cylinders;
	TimeSerieslet<DLTS>* timeseries;
	OverflowPipelet* overflowpipe;

private:
	DimensionStyle plain_style;

private:
	DraughtsPage* master;
	DraughtDecorator* decorator;
};

/*************************************************************************************************/
DraughtsPage::DraughtsPage(IMRMaster* plc) : Planet(__MODULE__), device(plc) {
	DraughtDecorator* decorator = new DraughtDecorator();
	Draughts* dashboard = new Draughts(this, decorator);

	this->dashboard = dashboard;

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
			this->statusline = new Statuslinelet(default_logging_level);
			this->statusbar = new Statusbarlet(this->name());
			this->insert(this->statusbar);
			this->insert(this->statusline);
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
	return false;
}
