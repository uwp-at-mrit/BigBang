#include "virtualization/numpad.hpp"

#include "planet.hpp"
#include "text.hpp"
#include "paint.hpp"
#include "syslog.hpp"
#include "colorspace.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::System;

using namespace Windows::UI;
using namespace Windows::UI::Text;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;

private enum NumpadCell { Col = 0, Row, NCol, NRow };

const static KeyboardCell keys[] = {
	{ VirtualKey::Number9, 2, 0, 1, 1 }, { VirtualKey::Number8, 1, 0, 1, 1 }, { VirtualKey::Number7, 0, 0, 1, 1 },
    { VirtualKey::Number6, 2, 1, 1, 1 }, { VirtualKey::Number5, 1, 1, 1, 1 }, { VirtualKey::Number4, 0, 1, 1, 1 },
    { VirtualKey::Number3, 2, 2, 1, 1 }, { VirtualKey::Number2, 1, 2, 1, 1 }, { VirtualKey::Number1, 0, 2, 1, 1 },
    { VirtualKey::Number0, 2, 3, 1, 1 }, { VirtualKey::Decimal, 0, 3, 2, 1 },

    { VirtualKey::Back, 3, 0, 1, 1 }, { VirtualKey::Enter, 3, 1, 1, 3 }
};

static unsigned int NUMPAD_KEYNUM = sizeof(keys) / sizeof(KeyboardCell);

/*************************************************************************************************/
Numpad::Numpad(IPlanet* master, float fontsize) : Keyboard(master, keys, NUMPAD_KEYNUM) {
	this->label_font = make_text_format("Consolas", fontsize);
	this->current_key = VirtualKey::None;
}

void Numpad::create() {
	TextExtent ts = get_text_extent("0", this->label_font);
	Color fg = system_foreground_brush()->Color;
	Color bg = system_background_brush()->Color;
	
	this->foreground = system_foreground_brush();
	this->background = make_solid_brush(rgba(bg, 0.8));
	this->border = make_solid_brush(rgba(fg, 0.618));
	this->highlight = make_solid_brush(rgba(fg, 0.382));
	this->taplight = make_solid_brush(rgba(bg, 0.618));

	this->em = ts.height;
	this->gapsize = this->em * 0.382F;
	this->cellsize = this->em * 1.618F;
	this->radius = this->gapsize * 0.5F;
}

void Numpad::fill_extent(float x, float y, float* w, float* h) {
	float size = this->cellsize * 4.0F + this->gapsize * 5.0F;

	SET_VALUES(w, size, h, size);
}

void Numpad::draw_before(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float width, height;

	this->fill_extent(x, y, &width, &height);
	ds->FillRoundedRectangle(x, y, width, height, this->radius, this->radius, this->background);
}

void Numpad::draw_cell(CanvasDrawingSession^ ds, VirtualKey key, bool focused, bool tapped, float x, float y, float width, float height) {
	Platform::String^ label = "";

	if (focused) {
		auto highbrush = (tapped ? this->taplight : this->highlight);
		ds->FillRoundedRectangle(x, y, width, height, this->radius, this->radius, highbrush);
	}

	ds->DrawRoundedRectangle(x, y, width, height, this->radius, this->radius, this->border, 2.0F);

	switch (key) {
	case VirtualKey::Number1: label = "1"; break;
	case VirtualKey::Number2: label = "2"; break;
	case VirtualKey::Number3: label = "3"; break;
	case VirtualKey::Number4: label = "4"; break;
	case VirtualKey::Number5: label = "5"; break;
	case VirtualKey::Number6: label = "6"; break;
	case VirtualKey::Number7: label = "7"; break;
	case VirtualKey::Number8: label = "8"; break;
	case VirtualKey::Number9: label = "9"; break;
	case VirtualKey::Number0: label = "0"; break;
	case VirtualKey::Decimal: label = "."; break;

	case VirtualKey::Back: label = "B"; break;
	case VirtualKey::Enter: label = "E"; break;
	}

	if (label != nullptr) {
		float tx, ty;

		tx = x + (width - this->em * 0.5F) * 0.5F;
		ty = y + (height - this->em) * 0.5F;
		ds->DrawText(label, tx, ty, this->foreground, this->label_font);
	}
}

void Numpad::fill_auto_position(float* x, float* y) {
	IPlanet* planet = this->master;
	IGraphlet* target = planet->get_focus_graphlet();

	if (target == nullptr) {
		float Width = planet->info->master->actual_width;
		float Height = planet->info->master->actual_height;
		float this_width, this_height;

		this->fill_extent(0.0F, 0.0F, &this_width, &this_height);
		SET_BOX(x, Width - this_width);
		SET_BOX(y, Height - this_height);
	} else {
		planet->fill_graphlet_location(target, x, y, GraphletAlignment::LB);
	}
}
