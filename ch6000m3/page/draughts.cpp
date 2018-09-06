#include <map>

#include "page/draughts.hpp"
#include "configuration.hpp"
#include "menu.hpp"

#include "graphlet/dashboard/cylinderlet.hpp"
#include "graphlet/dashboard/timeserieslet.hpp"

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

private enum class DLTS { EarthWork, Capacity, Load, Displacement, Draught, _ };

// WARNING: order matters
private enum class DL : unsigned int {
	EarthWork, Capacity, Height, Load, Displacement,
	
	SternDraft, lSternDraft, lSuctionDraft, rSternDraft, lSternHeight, rSternHeight,
	BowDraft, lBowDraft, rSuctionDraft, rBowDraft, lBowHeight, rBowHeight,

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

	void fill_ship_anchor(float fx, float fy, float* x, float *y) {
		float awidth = this->actual_width();
		float aheight = this->actual_height();
		auto abox = this->ship->ComputeBounds(make_scale_matrix(awidth, aheight));

		SET_BOX(x, this->x * awidth + this->ship_width * awidth * fx);
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
		this->percentage_style.unit_color = Colours::Silver;
		this->highlight_style = make_highlight_dimension_style(18.0F, 5U);
	}

public:
	void on_realtime_data(const uint8* DB2, size_t count, Syslog* logger) override {
		this->master->enter_critical_section();
		this->master->begin_update_sequence();

		this->dimensions[DL::EarthWork]->set_value(DBD(DB2, 236U));
		this->timeseries->set_value(DLTS::EarthWork, DBD(DB2, 236U) * 1000.0F);

		this->master->end_update_sequence();
		this->master->leave_critical_section();
	}

public:
	void load(float width, float height, float vinset) {
		float ship_y, ship_height, cylinder_height, lines_width, lines_height;
		
		this->decorator->fill_ship_extent(nullptr, &ship_y, &lines_width, &ship_height, true);
		
		lines_height = ship_y * 0.618F;
		this->timeseries = this->master->insert_one(new TimeSerieslet<DLTS>(__MODULE__, 18000.0, lines_width, lines_height));

		cylinder_height = ship_height * 0.382F;
		this->load_cylinder(this->cylinders, DL::EarthWork, cylinder_height, 15000.0, "meter3", LiquidSurface::_);
		this->load_cylinder(this->cylinders, DL::Capacity, cylinder_height, 15000.0, "meter3", LiquidSurface::_);
		this->load_cylinder(this->cylinders, DL::Height, cylinder_height, 15.0, "meter", LiquidSurface::_);
		this->load_cylinder(this->cylinders, DL::Load, cylinder_height, 18000.0, "ton", LiquidSurface::_);
		this->load_cylinder(this->cylinders, DL::Displacement, cylinder_height, 4000.0, "ton", LiquidSurface::_);

		this->load_dimensions(this->dimensions, DL::SternDraft, DL::rSternHeight, "meter");
		this->load_dimensions(this->dimensions, DL::BowDraft, DL::rBowHeight, "meter");
	}

	void reflow(float width, float height, float vinset) {
		float ship_y, lines_cy;
		float lines_cx = width * 0.5F;

		this->decorator->fill_ship_extent(nullptr, &ship_y, nullptr, nullptr);
		lines_cy = ship_y * 0.5F;

		this->master->move_to(this->timeseries, lines_cx, lines_cy, GraphletAnchor::CC);

		this->reflow_cylinders(this->cylinders, this->dimensions, this->captions, DL::EarthWork, DL::Displacement);

		{ // reflow dimensions
			float cpt_height, xoff, yoff;

			this->dimensions[DL::BowDraft]->fill_extent(0.0F, 0.0F, nullptr, &cpt_height);
			xoff = cpt_height * 0.50F;
			yoff = cpt_height * 0.20F;

			this->reflow_dimension(this->dimensions, DL::BowDraft, 1.0F, 0.5F, GraphletAnchor::LC, xoff);

			this->reflow_dimension(this->dimensions, DL::lBowDraft, 1.0F, 0.0F, GraphletAnchor::RB, -yoff, -yoff);
			this->reflow_dimension(this->dimensions, DL::lBowHeight, 1.0F, 0.0F, GraphletAnchor::RT, -yoff, yoff);
			this->reflow_dimension(this->dimensions, DL::rBowDraft, 1.0F, 1.0F, GraphletAnchor::RT, -yoff, yoff);
			this->reflow_dimension(this->dimensions, DL::rBowHeight, 1.0F, 1.0F, GraphletAnchor::RB, -yoff, -yoff);
			
			this->reflow_dimension(this->dimensions, DL::lSuctionDraft, 0.618F, 0.0F, GraphletAnchor::CB, 0.0F, -yoff);
			this->reflow_dimension(this->dimensions, DL::rSuctionDraft, 0.618F, 1.0F, GraphletAnchor::CT, 0.0F, yoff);

			this->reflow_dimension(this->dimensions, DL::lSternDraft, 0.0F, 0.0F, GraphletAnchor::LB, yoff, -yoff);
			this->reflow_dimension(this->dimensions, DL::lSternHeight, 0.0F, 0.0F, GraphletAnchor::LT, yoff, yoff);
			this->reflow_dimension(this->dimensions, DL::rSternDraft, 0.0F, 1.0F, GraphletAnchor::LT, yoff, yoff);
			this->reflow_dimension(this->dimensions, DL::rSternHeight, 0.0F, 1.0F, GraphletAnchor::LB, yoff, -yoff);

			this->reflow_dimension(this->dimensions, DL::SternDraft, 0.0F, 0.5F, GraphletAnchor::RC, - xoff);
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
		ds[id] = this->master->insert_one(new Credit<Dimensionlet, E>(unit, _speak(id)), id);
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
	void load_cylinder(std::map<E, Credit<Cylinderlet, E>*>& cs, E id, float height
		, double range, Platform::String^ unit, LiquidSurface surface) {
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
		, std::map<E, Credit<Labellet, E>*>& ls, E id0, E idn) {
		float x, y, gapsize;
		float flcount = float(_I(idn) - _I(id0) + 1 + 4);

		for (E id = id0; id <= idn; id++) {
			ls[id]->fill_extent(0.0F, 0.0F, nullptr, &gapsize);
			gapsize *= 0.5F;

			this->decorator->fill_ship_anchor(float(_I(id) - _I(id0) + 1) / flcount, 0.5F, &x, &y);

			this->master->move_to(is[id], x, y, GraphletAnchor::LC);
			this->master->move_to(ls[id], is[id], GraphletAnchor::CT, GraphletAnchor::CB, 0.0F, -gapsize);
			this->master->move_to(ds[id], is[id], GraphletAnchor::CB, GraphletAnchor::CT, 0.0F, gapsize);
		}
	}

	template<typename E>
	void reflow_dimension(std::map<E, Credit<Dimensionlet, E>*>& ds, E id , float fx, float fy
		, GraphletAnchor a, float dx = 0.0F, float dy = 0.0F) {
		float ax, ay;

		this->decorator->fill_ship_anchor(fx, fy, &ax, &ay);
		this->master->move_to(ds[id], ax, ay, a, dx, dy);
	}

private:
	void set_cylinder(DL id, float value) {
		this->cylinders[id]->set_value(value);
		this->dimensions[id]->set_value(value);
	}

private: // never delete these graphlets manually.
	std::map<DL, Credit<Labellet, DL>*> captions;
	std::map<DL, Credit<Percentagelet, DL>*> progresses;
	std::map<DL, Credit<Dimensionlet, DL>*> dimensions;
	std::map<DL, Credit<Cylinderlet, DL>*> cylinders;
	TimeSerieslet<DLTS>* timeseries;

private:
	DimensionStyle percentage_style;
	DimensionStyle highlight_style;

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
