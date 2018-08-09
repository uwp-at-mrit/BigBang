#include <map>

#include "page/doors.hpp"
#include "configuration.hpp"
#include "menu.hpp"

#include "graphlet/symbol/doorlet.hpp"
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
	LDPressure, LockPressure, RDPressure,
	Heel, Trim, EarthWork,
	SBD1, SBD2, SBD3, SBD4, SBD5, SBD6, SBD7,
	PSD1, PSD2, PSD3, PSD4, PSD5, PSD6, PSD7,
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
			CanvasTextFormat^ seq_font = make_bold_text_format("Microsoft YaHei", large_font_size);
			
			this->seq_color = Colours::Tomato;

			for (size_t idx = 0; idx < door_count_per_side ; idx++) {
				size_t ridx = door_count_per_side - idx;

				this->sequences[idx] = make_text_layout(ridx.ToString() + "#", seq_font);
			}
		}
	}

public:
	void draw_before(CanvasDrawingSession^ ds, float Width, float Height) override {
		float thickness = 2.0F;
		float sx = this->x * Width;
		float sy = this->y * Height;
		float sw = this->ship_width * Width;
		float cell_width = sw / float(door_count_per_side);
		float seq_y = sy - this->sequences[0]->LayoutBounds.Height * 1.618F;
		
		ds->DrawGeometry(this->ship->Transform(make_scale_matrix(Width, Height)),
			sx, sy, Colours::Silver, thickness);

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
		this->load_doors(this->doors, this->progresses, DS::PSD1, DS::PSD7, radius);
		this->load_doors(this->doors, this->progresses, DS::SBD1, DS::SBD7, radius);

		cylinder_height = (height - ship_y - ship_height - vinset * 2.0F) * 0.5F;
		this->load_indicators(this->draughts, this->dimensions, DS::Bow, DS::Stern, cylinder_height);
	}

	void reflow(float width, float height, float vinset) {
		float aheight = height - vinset - vinset;

		this->reflow_doors(this->doors, this->progresses, DS::PSD1, DS::PSD7, 1.0F, -0.5F, GraphletAnchor::CT);
		this->reflow_doors(this->doors, this->progresses, DS::SBD1, DS::SBD7, 3.0F, 0.5F, GraphletAnchor::CB);

		this->reflow_indicators(this->draughts, this->dimensions, DS::Bow, aheight, 0.70F);
		this->reflow_indicators(this->draughts, this->dimensions, DS::Stern, aheight, 0.30F);
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
	void load_dimension(std::map<E, Credit<Dimensionlet, E>*>& ds, E id, Platform::String^ unit, Platform::String^ label
		, CanvasTextFormat^ font = nullptr) {
		ds[id] = this->master->insert_one(new Credit<Dimensionlet, E>(unit, label, font, Colours::Yellow, Colours::Silver), id);
	}

	template<typename E>
	void load_dimension(std::map<E, Credit<Dimensionlet, E>*>& ds, E id, Platform::String^ unit, CanvasTextFormat^ font = nullptr) {
		this->load_dimension(ds, id, unit, _speak(id.ToString()), font);
	}

	template<typename E>
	void load_dimensions(std::map<E, Credit<Dimensionlet, E>*>& ds, E id0, E idn, Platform::String^ unit, Platform::String^ label = nullptr) {
		for (E id = id0; id <= idn; id++) {
			this->load_dimension(ds, id, unit, label);
		}
	}

	template<typename E>
	void load_label(std::map<E, Credit<Labellet, E>*>& ls, Platform::String^ caption, E id
		, CanvasSolidColorBrush^ color, CanvasTextFormat^ font = nullptr) {
		ls[id] = this->master->insert_one(new Credit<Labellet, E>(caption, font, color), id);
	}

	template<typename E>
	void load_label(std::map<E, Credit<Labellet, E>*>& ls, E id, CanvasSolidColorBrush^ color, CanvasTextFormat^ font = nullptr) {
		this->load_label(ls, _speak(id), id, color, font);
	}

	template<typename E>
	void load_doors(std::map<E, Credit<BottomDoorlet, E>*>& ds, std::map<E, Credit<Percentagelet, E>*>& ps, E id0, E idn, float radius) {
		for (E id = id0; id <= idn; id++) {
			ds[id] = this->master->insert_one(new Credit<BottomDoorlet, E>(radius), id);
			ps[id] = this->master->insert_one(new Credit<Percentagelet, E>(Colours::Yellow, Colours::Silver), id);
		}
	}

	template<typename E>
	void load_indicators(std::map<E, Credit<ConcaveCylinderlet, E>*>& cs, std::map<E, Credit<Dimensionlet, E>*>& ds, E id0, E idn, float height) {
		float width = height * 0.382F;

		for (E id = id0; id <= idn; id++) {
			cs[id] = this->master->insert_one(new Credit<ConcaveCylinderlet, E>(10.0F, width, height), id);
			this->load_dimension(ds, id, "meter");
		}
	}

private:
	template<typename E>
	void reflow_doors(std::map<E, Credit<BottomDoorlet, E>*>& ds, std::map<E, Credit<Percentagelet, E>*>& ps, E id0, E idn, float side_hint, float fy, GraphletAnchor p_anchor) {
		float cx, cy, cwidth, cheight, center;

		for (E id = id0; id <= idn; id++) {
			size_t idx = static_cast<size_t>(id) - static_cast<size_t>(id0) + 1;

			this->decorator->fill_door_cell_extent(&cx, &cy, &cwidth, &cheight, idx, side_hint);
			center = cx + cwidth * 0.5F;
			
			this->master->move_to(ds[id], center, cy + cheight * fy, GraphletAnchor::CC);
			this->master->move_to(ps[id], center, cy, p_anchor);
		}
	}

	template<class C, typename E>
	void reflow_indicators(std::map<E, Credit<C, E>*>& is, std::map<E, Credit<Dimensionlet, E>*>& ds, E id, float height, float fx) {
		float ship_x, ship_y, ship_width, ship_height, indicator_height, label_height;
		float ship_bottom, x, y;
		
		this->decorator->fill_ship_extent(&ship_x, &ship_y, &ship_width, &ship_height);
		is[id]->fill_extent(0.0F, 0.0F, nullptr, &indicator_height);
		ds[id]->fill_extent(0.0F, 0.0F, nullptr, &label_height);
		
		ship_bottom = ship_y + ship_height;
		x = ship_x + ship_width * fx;
		y = (height - ship_bottom) * 0.5F + ship_bottom;
		
		this->master->move_to(is[id], x, y, GraphletAnchor::CC);
		this->master->move_to(ds[id], is[id], GraphletAnchor::CB, GraphletAnchor::CT, 0.0F, label_height * 0.5F);
	}

private: // never delete these graphlets manually.
	std::map<DS, Credit<Labellet, DS>*> labels;
	std::map<DS, Credit<BottomDoorlet, DS>*> doors;
	std::map<DS, Credit<Percentagelet, DS>*> progresses;
	std::map<DS, Credit<Dimensionlet, DS>*> dimensions;
	std::map<DS, Credit<ConcaveCylinderlet, DS>*> draughts;

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
