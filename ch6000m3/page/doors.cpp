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
	WaterDepth,
	BowDraught, SternDraught,
	F, G, C, B, DoorPressure,
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
			CanvasTextFormat^ seq_font = make_bold_text_format("Microsoft YaHei", large_font_size);
			
			this->seq_color = Colours::Tomato;

			for (size_t idx = 1; idx <= sizeof(this->sequences) / sizeof(CanvasTextFormat^); idx++) {
				this->sequences[idx - 1] = make_text_layout(idx.ToString() + "#", seq_font);
			}
		}
	}

public:
	void draw_before(CanvasDrawingSession^ ds, float Width, float Height) override {
		size_t seq_count = sizeof(this->sequences) / sizeof(CanvasTextFormat^);
		float thickness = 2.0F;
		float sx = this->x * Width;
		float sy = this->y * Height;
		float sw = this->ship_width * Width;
		float cell_width = sw / float(seq_count);
		float seq_y = sy - this->sequences[0]->LayoutBounds.Height * 1.618F;
		
		ds->DrawGeometry(this->ship->Transform(make_scale_matrix(Width, Height)),
			sx, sy, Colours::Silver, thickness);

		for (size_t idx = 0; idx < seq_count; idx++) {
			size_t ridx = seq_count - idx - 1;
			float cell_x = sx + cell_width * float(idx);
			float seq_width = this->sequences[ridx]->LayoutBounds.Width;
			
			ds->DrawTextLayout(this->sequences[ridx],
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
	Doors(DoorsPage* master, DoorDecorator* ship) : master(master), decorator(ship) {
		this->cpt_font = make_text_format("Microsoft YaHei", large_font_size * 1.2F);
	}

public:
	void load(float width, float height, float vinset) {
		float unitsize = 32.0F;
		float cylinder_height = height * 0.618F * 0.618F;
		
		this->load_dimension(this->dimensions, DS::WaterDepth, "meter", this->cpt_font);
		this->load_doors(this->doors, this->progresses, DS::SB1, DS::SB7, unitsize);
		this->load_doors(this->doors, this->progresses, DS::PS1, DS::PS7, unitsize);
	}

	void reflow(float width, float height, float vinset) {
		float shipx, shipy, shipw, shiph, cx;
		
		this->decorator->fill_ship_extent(&shipx, &shipy, &shipw, &shiph);
		cx = shipx + shipw * 0.5F;

		this->master->move_to(this->dimensions[DS::WaterDepth], cx, shipy * 0.5F, GraphletAnchor::CC);
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
	template<class G, typename E>
	void load_doors(std::map<E, G*>& gs, E id0, E idn, float radius) {
		for (E id = id0; id <= idn; id++) {
			gs[id] = this->master->insert_one(new G(radius), id);
		}
	}

	template<class G, typename E>
	void load_doors(std::map<E, G*>& gs, std::map<E, Credit<Percentagelet, E>*>& ps, E id0, E idn, float radius) {
		this->load_doors(gs, id0, idn, radius);

		for (E id = id0; id <= idn; id++) {
			ps[id] = this->master->insert_one(new Credit<Percentagelet, E>(nullptr, Colours::Yellow, Colours::Silver), id);
		}
	}

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

private: // never delete these graphlets manually.
	std::map<DS, Credit<BottomDoorlet, DS>*> doors;
	std::map<DS, Credit<Percentagelet, DS>*> progresses;
	std::map<DS, Credit<Labellet, DS>*> labels;
	std::map<DS, Credit<Dimensionlet, DS>*> dimensions;
	std::map<DS, Credit<Cylinderlet, DS>*> indicators;

private:
	CanvasTextFormat^ cpt_font;

private:
	DoorsPage* master;
	DoorDecorator* decorator;
};

/*************************************************************************************************/
DoorsPage::DoorsPage(IMRMaster* plc) : Planet(__MODULE__), device(plc) {
	DoorDecorator* decorator = new DoorDecorator();
	Doors* dashboard = new Doors(this, decorator);

	this->dashboard = dashboard;
	this->decorator = decorator;
	this->operation = make_menu<DSOperation, IMRMaster*>(dashboard, plc);
}

DoorsPage::~DoorsPage() {
	if (this->dashboard != nullptr) {
		delete this->dashboard;
	}

	// `Planet` will destroy the decorator;
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
			this->append_decorator(new PageDecorator());
			this->append_decorator(this->decorator);

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
