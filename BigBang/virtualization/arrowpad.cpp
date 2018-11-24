#include <map>

#include "virtualization/arrowpad.hpp"

#include "text.hpp"
#include "planet.hpp"

#include "paint.hpp"
#include "colorspace.hpp"
#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::System;

using namespace Windows::UI;
using namespace Windows::UI::Text;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;

private enum NumpadCell { Col = 0, Row, NCol, NRow };

const static KeyboardCell keys[] = {
	{ VirtualKey::PageUp,   0, 0, 1, 1 },
    { VirtualKey::Left,     1, 0, 1, 1 },
	{ VirtualKey::Right,    2, 0, 1, 1 },
    { VirtualKey::PageDown, 3, 0, 1, 1 },
};

static std::map<VirtualKey, CanvasTextLayout^> key_labels;

/*************************************************************************************************/
Arrowpad::Arrowpad(IPlanet* master, float fontsize) : Keyboard(master, keys) {
	CanvasTextFormat^ label_font = make_text_format("Consolas", fontsize);
	
	this->current_key = VirtualKey::None;

	{ // Keyboard::construct()
		TextExtent te = get_text_extent("0", label_font);
		float em = te.height;
		
		this->gapsize = em * 0.382F;
		this->cellsize = em * 1.618F;
		this->radius = this->gapsize * 0.5F;
	}

	if (key_labels.size() == 0) {
		Platform::String^ label;

		for (unsigned int i = 0; i < sizeof(keys) / sizeof(KeyboardCell); i++) {
			VirtualKey key = keys[i].key;

			switch (key) {
			case VirtualKey::PageUp: label = L"⇤"; break;
			case VirtualKey::Left: label = L"←"; break;
			case VirtualKey::Right: label = L"→"; break;
			case VirtualKey::PageDown: label = L"⇥"; break;
			}

			key_labels.insert(std::pair<VirtualKey, CanvasTextLayout^>(key, make_text_layout(label, label_font)));
		}
	}
}

void Arrowpad::construct() {
	Color fg = Colours::Background->Color;
	Color bg = Colours::Foreground->Color;
	
	this->foreground = make_solid_brush(fg, 1.0);
	this->background = make_solid_brush(rgba(bg, 0.8));
	this->border = make_solid_brush(rgba(fg, 0.618));
	this->highlight = make_solid_brush(rgba(fg, 0.382));
	this->taplight = make_solid_brush(rgba(bg, 0.618));
}

void Arrowpad::draw_before(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	ds->FillRoundedRectangle(x, y, this->width, this->height, this->radius, this->radius, this->background);
}

void Arrowpad::draw_cell(CanvasDrawingSession^ ds, VirtualKey key, bool focused, bool tapped, float x, float y, float width, float height) {
	auto maybe_label = key_labels.find(key);

	if (focused) {
		auto highbrush = (tapped ? this->taplight : this->highlight);

		ds->FillRoundedRectangle(x, y, width, height, this->radius, this->radius, highbrush);
	}

	ds->DrawRoundedRectangle(x, y, width, height, this->radius, this->radius, this->border, 2.0F);

	if (maybe_label != key_labels.end()) {
		CanvasTextLayout^ label = maybe_label->second;
		Rect box = label->LayoutBounds;
		float tx = x + (width - box.Width) * 0.5F;
		float ty = y + (height - box.Height) * 0.5F;

		ds->DrawTextLayout(label, tx, ty, this->foreground);
	}
}

void Arrowpad::fill_auto_position(float* x, float* y, IGraphlet* g, GraphletAnchor a) {
	float Width = this->master->actual_width();
	float Height = this->master->actual_height();
	float width, height;

	this->fill_extent(0.0F, 0.0F, &width, &height);

	if (g == nullptr) {
		SET_BOX(x, Width - width);
		SET_BOX(y, Height - height);
	} else {
		float x0, y0;

		this->master->fill_graphlet_location(g, &x0, &y0, a);

		SET_VALUES(x, x0 - width * 0.5F, y, y0);
	}
}
