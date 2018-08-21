#include <map>

#include "page/doors.hpp"
#include "configuration.hpp"
#include "menu.hpp"

#include "graphlet/symbol/doorlet.hpp"
#include "graphlet/dashboard/tubelet.hpp"
#include "graphlet/dashboard/cylinderlet.hpp"

#include "decorator/page.hpp"

#include "module.hpp"
#include "shape.hpp"
#include "geometry.hpp"
#include "transformation.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

private enum DSMode { WindowUI = 0, Dashboard };

private enum class DSOperation { Open, Stop, Close, Disable, _ };

// WARNING: order matters
private enum class DS : unsigned int {
	Bow, Stern,
	pLeftDrag, pLock, pRightDrag,
	EarthWork, Heel, Trim,
	SB1, SB2, SB3, SB4, SB5, SB6, SB7,
	PS1, PS2, PS3, PS4, PS5, PS6, PS7,
	_
};

static const size_t door_count_per_side = 7;

private class DoorDecorator : public IPlanetDecorator {
public:
	DoorDecorator() {
		float height = 0.618F * 0.618F;
		float radius = height * 0.5F;
		
		this->ship_width = 0.618F;
		this->x = (1.0F - this->ship_width - radius) * 0.618F;
		this->y = (0.618F - height) * 0.75F;
		this->ship = geometry_union(rectangle(this->ship_width, height),
			segment(this->ship_width, radius, -90.0, 90.0, radius, radius));

		{ // initializing sequence labels
			CanvasTextFormat^ seq_font = make_bold_text_format("Microsoft YaHei UI", large_font_size);
			
			this->seq_color = Colours::Tomato;

			for (size_t idx = 0; idx < door_count_per_side ; idx++) {
				size_t ridx = door_count_per_side - idx;

				this->sequences[idx] = make_text_layout(ridx.ToString() + "#", seq_font);
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
		float cell_width = this->ship_width * Width / float(door_count_per_side);
		float seq_y = sy + (ship_box.Height - this->sequences[0]->LayoutBounds.Height) * 0.5F;
		
		ds->DrawGeometry(real_ship, sx, sy, Colours::Silver, thickness);

		for (size_t idx = 0; idx < door_count_per_side; idx++) {
			float cell_x = sx + cell_width * float(idx);
			float seq_width = this->sequences[idx]->LayoutBounds.Width;
			
			ds->DrawTextLayout(this->sequences[idx],
				cell_x + (cell_width - seq_width) * 0.5F, seq_y,
				this->seq_color);
		}
	}

public:
	void fill_ship_extent(float* x, float* y, float* width, float* height) {
		float awidth = this->actual_width();
		float aheight = this->actual_height();
		auto abox = this->ship->ComputeBounds(make_scale_matrix(awidth, aheight));

		SET_VALUES(x, this->x * awidth, y, this->y * aheight);
		SET_VALUES(width, this->ship_width * awidth, height, abox.Height);
	}

	void fill_door_cell_extent(float* x, float* y, float* width, float* height, size_t idx, float side_hint) {
		float awidth = this->actual_width();
		float aheight = this->actual_height();
		auto abox = this->ship->ComputeBounds(make_scale_matrix(awidth, aheight));
		float cell_width = this->ship_width * awidth / float(door_count_per_side);
		float cell_height = abox.Height / 4.0F;

		SET_VALUES(width, cell_width, height, cell_height);
		SET_BOX(x, this->x * awidth + cell_width * float(door_count_per_side - idx));
		SET_BOX(y, this->y * aheight + cell_height * side_hint);
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
	CanvasTextLayout^ sequences[door_count_per_side];
	ICanvasBrush^ seq_color;

private:
	float x;
	float y;
	float ship_width;
};

private class Doors final : public PLCConfirmation, public IMenuCommand<DSOperation, IMRMaster*> {
public:
	Doors(DoorsPage* master, DoorDecorator* ship) : master(master), decorator(ship) {}

public:
	void load(float width, float height, float vinset) {
		float cell_width, cell_height, radius;
		float ship_y, ship_height, cylinder_height;
		
		this->decorator->fill_ship_extent(nullptr, &ship_y, nullptr, &ship_height);
		this->decorator->fill_door_cell_extent(nullptr, nullptr, &cell_width, &cell_height, 1, 0.0F);
		
		radius = std::fminf(cell_width, cell_height) * 0.75F * 0.5F;
		this->load_doors(this->doors, this->progresses, this->tubes, DS::PS1, DS::PS7, radius);
		this->load_doors(this->doors, this->progresses, this->tubes, DS::SB1, DS::SB7, radius);

		cylinder_height = (height - ship_y - ship_height - vinset * 2.0F) * 0.5F;
		this->load_indicator(this->draughts, this->dimensions, DS::Bow, cylinder_height, FitPosition::Left);
		this->load_indicator(this->draughts, this->dimensions, DS::Stern, cylinder_height, FitPosition::Right);

		this->load_dimension(this->dimensions, this->labels, DS::EarthWork, "meter3");
		this->load_dimensions(this->dimensions, this->labels, DS::pLeftDrag, DS::pRightDrag, "bar");
		this->load_dimensions(this->dimensions, this->labels, DS::Heel, DS::Trim, "degrees");
	}

	void reflow(float width, float height, float vinset) {
		this->reflow_doors(this->doors, this->progresses, this->tubes, DS::PS1, DS::PS7, 1.0F, -0.5F);
		this->reflow_doors(this->doors, this->progresses, this->tubes, DS::SB1, DS::SB7, 3.0F, 0.5F);

		this->reflow_indicators(this->draughts, this->dimensions, DS::Bow, 0.618F);
		this->reflow_indicators(this->draughts, this->dimensions, DS::Stern, 0.382F);

		{ // reflow dimensions
			float x, y, label_height, xoff, yoff;

			this->master->fill_graphlet_location(this->draughts[DS::Bow], nullptr, &y, GraphletAnchor::CC);
			this->labels[DS::EarthWork]->fill_extent(0.0F, 0.0F, nullptr, &label_height);

			xoff = label_height * 0.5F;
			yoff = label_height * 2.0F;

			this->decorator->fill_descent_anchor(0.10F, 0.0F, &x, nullptr);
			this->reflow_tabular(this->labels, this->dimensions, DS::pLock, x, y, DS::pLeftDrag, DS::pRightDrag, xoff, yoff);
			
			this->decorator->fill_descent_anchor(0.90F, 0.0F, &x, nullptr);
			this->reflow_tabular(this->labels, this->dimensions, DS::Heel, x, y, DS::EarthWork, DS::Trim, xoff, yoff);
		}
	}

public:
	void on_realtime_data(const uint8* DB2, size_t count, Syslog* logger) override {
		this->master->enter_critical_section();
		this->master->begin_update_sequence();

		this->set_draught(DS::Bow, DBD(DB2, 164U));
		this->set_draught(DS::Stern, DBD(DB2, 188U));

		this->dimensions[DS::Trim]->set_value(DBD(DB2, 200U));
		this->dimensions[DS::Heel]->set_value(DBD(DB2, 204U));
		this->dimensions[DS::EarthWork]->set_value(DBD(DB2, 236U));

		this->master->end_update_sequence();
		this->master->leave_critical_section();
	}

public:
	void execute(DSOperation cmd, IGraphlet* target, IMRMaster* plc) {
		auto door = dynamic_cast<Credit<BottomDoorlet, DS>*>(target);

		if (door != nullptr) {
			plc->get_logger()->log_message(Log::Info, L"%s %s",
				cmd.ToString()->Data(),
				door->id.ToString()->Data());
		}
	}

private:
	template<typename E>
	void load_dimension(std::map<E, Credit<Dimensionlet, E>*>& ds, E id, Platform::String^ unit, Platform::String^ label = nullptr) {
		ds[id] = this->master->insert_one(new Credit<Dimensionlet, E>(unit, label, Colours::Yellow, Colours::Silver), id);
	}

	template<typename E>
	void load_dimension(std::map<E, Credit<Dimensionlet, E>*>& ds, std::map<E, Credit<Labellet, E>*>& ls, E id, Platform::String^ unit) {
		ls[id] = this->master->insert_one(new Credit<Labellet, E>(_speak(id), Colours::Silver), id);
		this->load_dimension(ds, id, unit);
	}

	template<typename E>
	void load_dimensions(std::map<E, Credit<Dimensionlet, E>*>& ds, std::map<E, Credit<Labellet, E>*>& ls, E id0, E idn, Platform::String^ unit) {
		for (E id = id0; id <= idn; id++) {
			this->load_dimension(ds, ls, id, unit);
		}
	}

	template<typename E>
	void load_doors(std::map<E, Credit<BottomDoorlet, E>*>& ds, std::map<E, Credit<Percentagelet, E>*>& ps
		, std::map<E, Credit<Tubelet, E>*>& ts, E id0, E idn, float radius) {
		float tube_height = radius * 2.0F * 1.618F;
		float tube_width = tube_height * 0.10F;

		for (E id = id0; id <= idn; id++) {
			ds[id] = this->master->insert_one(new Credit<BottomDoorlet, E>(radius), id);
			ps[id] = this->master->insert_one(new Credit<Percentagelet, E>(Colours::Yellow, Colours::Silver), id);
			ts[id] = this->master->insert_one(new Credit<Tubelet, E>(tube_width, tube_height), id);
		}
	}

	template<typename E>
	void load_indicator(std::map<E, Credit<Cylinderlet, E>*>& cs, std::map<E, Credit<Dimensionlet, E>*>& ds, E id
		, float height, FitPosition mark_position) {
		float width = height * 0.382F;
		auto cylinder = new Credit<Cylinderlet, E>(LiquidSurface::Convex, mark_position, 10.0, width, height);

		cs[id] = this->master->insert_one(cylinder, id);
		this->load_dimension(ds, id, "meter", _speak(id.ToString()));
	}

private:
	template<typename E>
	void reflow_doors(std::map<E, Credit<BottomDoorlet, E>*>& ds, std::map<E, Credit<Percentagelet, E>*>& ps
		, std::map<E, Credit<Tubelet, E>*>& ts, E id0, E idn, float side_hint, float fy) {
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
			center = cell_x + cell_width * 0.5F;
			
			this->master->move_to(ds[id], center, cell_y + cell_height * fy, GraphletAnchor::CC);
			this->master->move_to(ts[id], ds[id], t_anchor, t_anchor, -center_xoff);
			this->master->move_to(ps[id], ts[id], p_anchor, p_anchor, center_xoff);
		}
	}

	template<class C, typename E>
	void reflow_indicators(std::map<E, Credit<C, E>*>& is, std::map<E, Credit<Dimensionlet, E>*>& ds, E id, float fx) {
		float label_height, x, y;
		
		ds[id]->fill_extent(0.0F, 0.0F, nullptr, &label_height);
		this->decorator->fill_descent_anchor(fx, 0.382F, &x, &y);	

		this->master->move_to(is[id], x, y, GraphletAnchor::LC);
		this->master->move_to(ds[id], is[id], GraphletAnchor::CB, GraphletAnchor::CT, 0.0F, label_height * 0.5F);
	}

	template <class G1, class G2, typename E>
	void reflow_tabular(std::map<E, Credit<G1, E>*>& g1s, std::map<E, Credit<G2, E>*>& g2s
		, E base, float x, float y, E above, E below, float xoff, float yoff) {
		this->master->move_to(g1s[base], x, y, GraphletAnchor::CC);
		this->master->move_to(g1s[above], g1s[base], GraphletAnchor::RT, GraphletAnchor::RB, 0.0F, -yoff);
		this->master->move_to(g1s[below], g1s[base], GraphletAnchor::RB, GraphletAnchor::RT, 0.0F, yoff);

		this->master->move_to(g2s[base], g1s[base], GraphletAnchor::RC, GraphletAnchor::LC, xoff, 0.0F);
		this->master->move_to(g2s[above], g1s[above], GraphletAnchor::RC, GraphletAnchor::LC, xoff, 0.0F);
		this->master->move_to(g2s[below], g1s[below], GraphletAnchor::RC, GraphletAnchor::LC, xoff, 0.0F);
	}

private:
	void set_draught(DS id, float value) {
		this->draughts[id]->set_value(value);
		this->dimensions[id]->set_value(value);
	}

private: // never delete these graphlets manually.
	std::map<DS, Credit<Labellet, DS>*> labels;
	std::map<DS, Credit<BottomDoorlet, DS>*> doors;
	std::map<DS, Credit<Percentagelet, DS>*> progresses;
	std::map<DS, Credit<Tubelet, DS>*> tubes;
	std::map<DS, Credit<Dimensionlet, DS>*> dimensions;
	std::map<DS, Credit<Cylinderlet, DS>*> draughts;

private:
	DoorsPage* master;
	DoorDecorator* decorator;
};

/*************************************************************************************************/
DoorsPage::DoorsPage(IMRMaster* plc) : Planet(__MODULE__), device(plc) {
	DoorDecorator* decorator = new DoorDecorator();
	Doors* dashboard = new Doors(this, decorator);

	this->dashboard = dashboard;
	this->operation = make_menu<DSOperation, IMRMaster*>(dashboard, plc);

	this->device->append_confirmation_receiver(dashboard);

	this->append_decorator(new PageDecorator());
	this->append_decorator(decorator);
}

DoorsPage::~DoorsPage() {
	if (this->dashboard != nullptr) {
		delete this->dashboard;
	}
}

void DoorsPage::load(CanvasCreateResourcesReason reason, float width, float height) {
	auto db = dynamic_cast<Doors*>(this->dashboard);

	if (db != nullptr) {
		float vinset = statusbar_height();

		{ // load graphlets
			this->change_mode(DSMode::Dashboard);
			db->load(width, height, vinset);

			this->change_mode(DSMode::WindowUI);
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

void DoorsPage::reflow(float width, float height) {
	auto db = dynamic_cast<Doors*>(this->dashboard);
	
	if (db != nullptr) {
		float vinset = statusbar_height();

		this->change_mode(DSMode::WindowUI);
		this->move_to(this->statusline, 0.0F, height, GraphletAnchor::LB);
		
		this->change_mode(DSMode::Dashboard);
		db->reflow(width, height, vinset);
	}
}

bool DoorsPage::can_select(IGraphlet* g) {
	return (dynamic_cast<BottomDoorlet*>(g) != nullptr);
}

void DoorsPage::on_tap(IGraphlet* g, float local_x, float local_y, bool shifted, bool ctrled) {
	Planet::on_tap(g, local_x, local_y, shifted, ctrled);

	if (this->can_select(g)) {
		menu_popup(this->operation, g, local_x, local_y);
	}
}
