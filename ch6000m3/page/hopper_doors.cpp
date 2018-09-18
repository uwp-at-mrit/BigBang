#include <map>

#include "page/hopper_doors.hpp"
#include "configuration.hpp"
#include "menu.hpp"

#include "graphlet/symbol/door/hopper_doorlet.hpp"
#include "graphlet/dashboard/cylinderlet.hpp"

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

private enum HDMode { WindowUI = 0, Dashboard };

private enum class HDOperation { Open, Stop, Close, Disable, _ };

// WARNING: order matters
private enum class HD : unsigned int {
	SternDraft, EarthWork, Vessel, HopperHeight, Loading, Displacement, BowDraft,
	
	pLeftDrag, pLock, pRightDrag, Heel, Trim,
	Port, Starboard, OpenFlow, CloseFlow, Pressure,

	SB1, SB2, SB3, SB4, SB5, SB6, SB7,
	PS1, PS2, PS3, PS4, PS5, PS6, PS7,

	_
};

private class DoorDecorator : public IPlanetDecorator {
public:
	DoorDecorator() {
		float height = 0.618F * 0.618F;
		float radius = height * 0.5F;
		
		this->ship_width = 0.618F;
		this->x = (1.0F - this->ship_width - radius) * 0.618F;
		this->y = (0.618F - height) * 0.618F;
		this->ship = geometry_union(rectangle(this->ship_width, height),
			segment(this->ship_width, radius, -90.0, 90.0, radius, radius));

		{ // initializing sequence labels
			CanvasTextFormat^ cpt_font = make_bold_text_format("Microsoft YaHei", large_font_size);
			
			this->seq_color = Colours::Tomato;

			for (size_t idx = 0; idx < hopper_count ; idx++) {
				size_t ridx = hopper_count - idx;

				this->sequences[idx] = make_text_layout(ridx.ToString() + "#", cpt_font);
			}
		}
	}

public:
	void draw_before(CanvasDrawingSession^ ds, float Width, float Height) override {
		auto real_ship = geometry_scale(this->ship, Width, Height);
		Rect ship_box = real_ship->ComputeBounds();
		float thickness = 2.0F;
		float sx = this->x * Width;
		float sy = this->y * Height;
		float cell_width = this->ship_width * Width / float(hopper_count);
		float seq_y = sy + (ship_box.Height - this->sequences[0]->LayoutBounds.Height) * 0.5F;
		
		ds->DrawGeometry(real_ship, sx, sy, Colours::Silver, thickness);
		
		for (size_t idx = 0; idx < hopper_count; idx++) {
			float cell_x = sx + cell_width * float(idx);
			float seq_width = this->sequences[idx]->LayoutBounds.Width;
			
			ds->DrawTextLayout(this->sequences[idx],
				cell_x + (cell_width - seq_width) * 0.5F, seq_y,
				this->seq_color);
		}
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

	void fill_door_cell_extent(float* x, float* y, float* width, float* height, size_t idx, float side_hint) {
		float awidth = this->actual_width();
		float aheight = this->actual_height();
		auto abox = this->ship->ComputeBounds(make_scale_matrix(awidth, aheight));
		float cell_width = this->ship_width * awidth / float(hopper_count);
		float cell_height = abox.Height / 4.0F;

		SET_VALUES(width, cell_width, height, cell_height);
		SET_BOX(x, this->x * awidth + cell_width * float(hopper_count - idx));
		SET_BOX(y, this->y * aheight + cell_height * side_hint);
	}

	void fill_ascent_anchor(float fx, float fy, float* x, float *y) {
		float awidth = this->actual_width();
		float aheight = this->actual_height();
		auto abox = this->ship->ComputeBounds(make_scale_matrix(awidth, aheight));

		SET_BOX(x, this->x * awidth + this->ship_width * awidth * fx);
		SET_BOX(y, this->y * aheight * fy);
	}

	void fill_descent_anchor(float fx, float fy, float* x, float *y) {
		float awidth = this->actual_width();
		float aheight = this->actual_height();
		auto abox = this->ship->ComputeBounds(make_scale_matrix(awidth, aheight));
		
		SET_BOX(x, this->x * awidth + this->ship_width * awidth * fx);
		SET_BOX(y, aheight * fy + (this->y * aheight + abox.Height) * (1.0F - fy));
	}

private:
	CanvasGeometry^ ship;
	CanvasTextLayout^ sequences[hopper_count];
	ICanvasBrush^ seq_color;

private:
	float x;
	float y;
	float ship_width;
};

private class Doors final
	: public PLCConfirmation
	, public IMenuCommand<HDOperation, Credit<HopperDoorlet, HD>, IMRMaster*> {
public:
	Doors(HopperDoorsPage* master, DoorDecorator* ship) : master(master), decorator(ship) {
		this->percentage_style.unit_color = Colours::Silver;
		this->highlight_style = make_highlight_dimension_style(18.0F, 5U);
		this->setting_style = make_setting_dimension_style(18.0F, 6U);
	}

public:
	void on_realtime_data(const uint8* DB2, size_t count, Syslog* logger) override {
		this->master->enter_critical_section();
		this->master->begin_update_sequence();

		this->set_cylinder(HD::HopperHeight, DBD(DB2, 224U));
		this->set_cylinder(HD::Displacement, DBD(DB2, 228U));
		this->set_cylinder(HD::Loading, DBD(DB2, 232U));
		this->set_cylinder(HD::EarthWork, DBD(DB2, 236U));
		this->set_cylinder(HD::Vessel, DBD(DB2, 320U));

		this->dimensions[HD::BowDraft]->set_value(DBD(DB2, 164U));
		this->dimensions[HD::SternDraft]->set_value(DBD(DB2, 188U));
		this->dimensions[HD::Trim]->set_value(DBD(DB2, 200U));
		this->dimensions[HD::Heel]->set_value(DBD(DB2, 204U));

		this->master->end_update_sequence();
		this->master->leave_critical_section();
	}

public:
	void load(float width, float height, float vinset) {
		float cell_width, cell_height, radius, cylinder_height;
		
		this->decorator->fill_door_cell_extent(nullptr, nullptr, &cell_width, &cell_height, 1, 0.0F);
		
		radius = std::fminf(cell_width, cell_height) * 0.75F * 0.5F;
		this->load_doors(this->hdoors, this->progresses, this->doors, HD::PS1, HD::PS7, radius);
		this->load_doors(this->hdoors, this->progresses, this->doors, HD::SB1, HD::SB7, radius);

		cylinder_height = cell_height * 1.618F;
		this->load_cylinder(this->cylinders, HD::BowDraft, cylinder_height, 12.0, "meter", LiquidSurface::Convex);
		this->load_cylinder(this->cylinders, HD::EarthWork, cylinder_height, 15000.0, "meter3", LiquidSurface::_);
		this->load_cylinder(this->cylinders, HD::Vessel, cylinder_height, 15000.0, "meter3", LiquidSurface::_);
		this->load_cylinder(this->cylinders, HD::HopperHeight, cylinder_height, 15.0, "meter", LiquidSurface::Convex);
		this->load_cylinder(this->cylinders, HD::Loading, cylinder_height, 18000.0, "ton", LiquidSurface::_);
		this->load_cylinder(this->cylinders, HD::Displacement, cylinder_height, 4000.0, "ton", LiquidSurface::_);
		this->load_cylinder(this->cylinders, HD::SternDraft, cylinder_height, 12.0, "meter", LiquidSurface::Convex);

		this->load_dimensions(this->dimensions, this->captions, HD::pLeftDrag, HD::pRightDrag, "bar");
		this->load_dimensions(this->dimensions, this->captions, HD::Heel, HD::Trim, "degrees");

		{ // load settings
			CanvasTextFormat^ cpt_font = make_bold_text_format("Microsoft YaHei", large_font_size);
			ICanvasBrush^ ps_color = Colours::make(default_port_color);
			ICanvasBrush^ sb_color = Colours::make(default_starboard_color);

			this->load_label(this->captions, HD::Port, ps_color, cpt_font);
			this->load_label(this->captions, HD::Starboard, sb_color, cpt_font);

			this->load_setting(this->ports, HD::Pressure, "bar");
			this->load_setting(this->starboards, HD::Pressure, "bar");
		}
	}

	void reflow(float width, float height, float vinset) {
		this->reflow_doors(this->hdoors, this->progresses, this->doors, HD::PS1, HD::PS7, 1.0F, -0.5F);
		this->reflow_doors(this->hdoors, this->progresses, this->doors, HD::SB1, HD::SB7, 3.0F, 0.5F);

		this->reflow_cylinders(this->cylinders, this->dimensions, this->captions, HD::SternDraft, HD::BowDraft);

		{ // reflow settings
			float x, y, yoff;

			this->captions[HD::Port]->fill_extent(0.0F, 0.0F, nullptr, &yoff);
			yoff *= 0.618F;

			this->decorator->fill_ascent_anchor(0.1F, 1.0F, &x, &y);
			this->master->move_to(this->captions[HD::Port], x, y, GraphletAnchor::LB, 0.0F, -yoff);

			this->decorator->fill_descent_anchor(0.1F, 0.0F, &x, &y);
			this->master->move_to(this->captions[HD::Starboard], x, y, GraphletAnchor::LT, 0.0F, yoff);

			this->master->move_to(this->ports[HD::Pressure], this->captions[HD::Port], GraphletAnchor::RB, GraphletAnchor::LB);
			this->master->move_to(this->dimensions[HD::pLeftDrag], this->ports[HD::Pressure], GraphletAnchor::RB, GraphletAnchor::LB);

			this->master->move_to(this->starboards[HD::Pressure], this->captions[HD::Starboard], GraphletAnchor::RB, GraphletAnchor::LB);
			this->master->move_to(this->dimensions[HD::pRightDrag], this->starboards[HD::Pressure], GraphletAnchor::RB, GraphletAnchor::LB);
		}
		
		{ // reflow dimensions
			float x, y, label_height, xoff, yoff;

			this->master->fill_graphlet_location(this->cylinders[HD::BowDraft], nullptr, &y, GraphletAnchor::CC);
			this->captions[HD::Heel]->fill_extent(0.0F, 0.0F, nullptr, &label_height);

			xoff = label_height * 0.5F;
			yoff = label_height * 2.0F;

			this->decorator->fill_descent_anchor(0.10F, 0.0F, &x, nullptr);
			this->decorator->fill_descent_anchor(0.90F, 0.0F, &x, nullptr);
		}
	}

public:
	void execute(HDOperation cmd, Credit<HopperDoorlet, HD>* door, IMRMaster* plc) {
		plc->get_logger()->log_message(Log::Info, L"%s %s",
			cmd.ToString()->Data(),
			door->id.ToString()->Data());
	}

public:
	bool on_char(VirtualKey key, IMRMaster* plc) {
		bool handled = false;

		if (key == VirtualKey::Enter) {
			auto editor = dynamic_cast<Credit<Dimensionlet, HD>*>(this->master->get_focus_graphlet());

			if (editor != nullptr) {
				plc->get_logger()->log_message(Log::Info, L"%s: %lf",
					editor->id.ToString()->Data(),
					editor->get_input_number());

				editor->set_value(editor->get_input_number());
				if (editor == this->ports[editor->id]) {
					this->dimensions[HD::pLeftDrag]->set_value(editor->get_input_number());
				} else {
					this->dimensions[HD::pRightDrag]->set_value(editor->get_input_number());
				}
			}

			handled = true;
		}

		return handled;
	}

private:
	template<typename E>
	void load_setting(std::map<E, Credit<Dimensionlet, E>*>& ds, E id, Platform::String^ unit) {
		ds[id] = this->master->insert_one(new Credit<Dimensionlet, E>(EditorStatus::Enabled, this->setting_style, unit, _speak(id)), id);
	}

	template<typename E>
	void load_dimension(std::map<E, Credit<Dimensionlet, E>*>& ds, std::map<E, Credit<Labellet, E>*>& ls, E id, Platform::String^ unit) {
		this->load_label(ls, id, Colours::Silver);
		ds[id] = this->master->insert_one(new Credit<Dimensionlet, E>(unit), id);
	}

	template<typename E>
	void load_dimensions(std::map<E, Credit<Dimensionlet, E>*>& ds, std::map<E, Credit<Labellet, E>*>& ls, E id0, E idn, Platform::String^ unit) {
		for (E id = id0; id <= idn; id++) {
			this->load_dimension(ds, ls, id, unit);
		}
	}

	template<class D, typename E>
	void load_doors(std::map<E, Credit<D, E>*>& ds, std::map<E, Credit<Percentagelet, E>*>& ps
		, std::map<E, Credit<Doorlet, E>*>& ts, E id0, E idn, float radius) {
		float door_height = radius * 2.0F * 1.618F;
		float door_width = door_height * 0.20F;

		for (E id = id0; id <= idn; id++) {
			ds[id] = this->master->insert_one(new Credit<D, E>(radius), id);
			ps[id] = this->master->insert_one(new Credit<Percentagelet, E>(this->percentage_style), id);
			ts[id] = this->master->insert_one(new Credit<Doorlet, E>(door_width, door_height), id);
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
	template<class D, typename E>
	void reflow_doors(std::map<E, Credit<D, E>*>& ds, std::map<E, Credit<Percentagelet, E>*>& ps
		, std::map<E, Credit<Doorlet, E>*>& ts, E id0, E idn, float side_hint, float fy) {
		GraphletAnchor t_anchor = GraphletAnchor::CT;
		GraphletAnchor p_anchor = GraphletAnchor::CB;
		float cell_x, cell_y, cell_width, cell_height, center;
		float tube_width, door_width, center_xoff;

		ds[id0]->fill_extent(0.0F, 0.0F, &door_width);
		ts[id0]->fill_extent(0.0F, 0.0F, &tube_width);
		center_xoff = tube_width + door_width * 0.5F;

		if (fy > 0.0F) { // Starboard
			t_anchor = GraphletAnchor::CB;
			p_anchor = GraphletAnchor::CT;
		}
		
		for (E id = id0; id <= idn; id++) {
			size_t idx = static_cast<size_t>(id) - static_cast<size_t>(id0) + 1;

			this->decorator->fill_door_cell_extent(&cell_x, &cell_y, &cell_width, &cell_height, idx, side_hint);
			center = cell_x + cell_width * 0.64F;
			
			this->master->move_to(ds[id], center, cell_y + cell_height * fy, GraphletAnchor::CC);
			this->master->move_to(ts[id], ds[id], t_anchor, t_anchor, -center_xoff);
			this->master->move_to(ps[id], ts[id], p_anchor, p_anchor, center_xoff);
		}
	}

	template<class C, typename E>
	void reflow_cylinders(std::map<E, Credit<C, E>*>& is, std::map<E, Credit<Dimensionlet, E>*>& ds
		, std::map<E, Credit<Labellet, E>*>& ls, E id0, E idn) {
		float x, y, xoff, gapsize;
		float flcount = float(_I(idn) - _I(id0) + 1);
	
		this->decorator->fill_door_cell_extent(nullptr, &y, &xoff, nullptr, 1, 5.5F);
		xoff *= 0.5F;

		for (E id = id0; id <= idn; id++) {
			ls[id]->fill_extent(0.0F, 0.0F, nullptr, &gapsize);
			gapsize *= 0.5F;

			this->decorator->fill_descent_anchor(float(_I(id) - _I(id0)) / flcount, 0.0F, &x, nullptr);

			this->master->move_to(is[id], x + xoff, y, GraphletAnchor::CT);
			this->master->move_to(ls[id], is[id], GraphletAnchor::CT, GraphletAnchor::CB, 0.0F, -gapsize);
			this->master->move_to(ds[id], is[id], GraphletAnchor::CB, GraphletAnchor::CT, 0.0F, gapsize);
		}
	}

private:
	void set_cylinder(HD id, float value) {
		this->cylinders[id]->set_value(value);
		this->dimensions[id]->set_value(value);
	}

private: // never delete these graphlets manually.
	std::map<HD, Credit<Labellet, HD>*> captions;
	std::map<HD, Credit<HopperDoorlet, HD>*> hdoors;
	std::map<HD, Credit<Percentagelet, HD>*> progresses;
	std::map<HD, Credit<Doorlet, HD>*> doors;
	std::map<HD, Credit<Dimensionlet, HD>*> dimensions;
	std::map<HD, Credit<Cylinderlet, HD>*> cylinders;
	std::map<HD, Credit<Dimensionlet, HD>*> ports;
	std::map<HD, Credit<Dimensionlet, HD>*> starboards;

private:
	DimensionStyle percentage_style;
	DimensionStyle highlight_style;
	DimensionStyle setting_style;

private:
	HopperDoorsPage* master;
	DoorDecorator* decorator;
};

/*************************************************************************************************/
HopperDoorsPage::HopperDoorsPage(IMRMaster* plc) : Planet(__MODULE__), device(plc) {
	DoorDecorator* decorator = new DoorDecorator();
	Doors* dashboard = new Doors(this, decorator);

	this->dashboard = dashboard;
	this->door_op = make_menu<HDOperation, Credit<HopperDoorlet, HD>, IMRMaster*>(dashboard, plc);

	this->device->append_confirmation_receiver(dashboard);

	this->append_decorator(new PageDecorator());
	this->append_decorator(decorator);
}

HopperDoorsPage::~HopperDoorsPage() {
	if (this->dashboard != nullptr) {
		delete this->dashboard;
	}
}

void HopperDoorsPage::load(CanvasCreateResourcesReason reason, float width, float height) {
	auto db = dynamic_cast<Doors*>(this->dashboard);

	if (db != nullptr) {
		float vinset = statusbar_height();

		{ // load graphlets
			this->change_mode(HDMode::Dashboard);
			db->load(width, height, vinset);

			this->change_mode(HDMode::WindowUI);
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

void HopperDoorsPage::reflow(float width, float height) {
	auto db = dynamic_cast<Doors*>(this->dashboard);
	
	if (db != nullptr) {
		float vinset = statusbar_height();

		this->change_mode(HDMode::WindowUI);
		this->move_to(this->statusline, 0.0F, height, GraphletAnchor::LB);
		
		this->change_mode(HDMode::Dashboard);
		db->reflow(width, height, vinset);
	}
}

bool HopperDoorsPage::can_select(IGraphlet* g) {
	return (dynamic_cast<HopperDoorlet*>(g) != nullptr);
}

bool HopperDoorsPage::on_char(VirtualKey key, bool wargrey_keyboard) {
	bool handled = Planet::on_char(key, wargrey_keyboard);

	if (!handled) {
		auto db = dynamic_cast<Doors*>(this->dashboard);
		
		if (db != nullptr) {
			handled = db->on_char(key, this->device);
		}
	}

	return handled;
}

void HopperDoorsPage::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool ctrled) {
	auto hdoor = dynamic_cast<HopperDoorlet*>(g);
	auto editor = dynamic_cast<IEditorlet*>(g);

	Planet::on_tap(g, local_x, local_y, shifted, ctrled);

	if (hdoor != nullptr) {
		menu_popup(this->door_op, hdoor, local_x, local_y);
	} else if (editor != nullptr) {
		this->show_virtual_keyboard(ScreenKeyboard::Numpad);
	}
}
