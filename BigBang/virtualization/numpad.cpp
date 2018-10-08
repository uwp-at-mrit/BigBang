#include <map>

#include "virtualization/numpad.hpp"

#include "planet.hpp"
#include "text.hpp"
#include "paint.hpp"
#include "syslog.hpp"
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
	{ VirtualKey::NumberPad9, 2, 0, 1, 1 }, { VirtualKey::NumberPad8, 1, 0, 1, 1 }, { VirtualKey::NumberPad7, 0, 0, 1, 1 },
    { VirtualKey::NumberPad6, 2, 1, 1, 1 }, { VirtualKey::NumberPad5, 1, 1, 1, 1 }, { VirtualKey::NumberPad4, 0, 1, 1, 1 },
    { VirtualKey::NumberPad3, 2, 2, 1, 1 }, { VirtualKey::NumberPad2, 1, 2, 1, 1 }, { VirtualKey::NumberPad1, 0, 2, 1, 1 },
    { VirtualKey::Subtract,   2, 3, 1, 1 }, { VirtualKey::Decimal,    1, 3, 1, 1 }, { VirtualKey::NumberPad0, 0, 3, 1, 1 },

	{ VirtualKey::PageUp,     3, 0, 1, 1 }, { VirtualKey::Up,         3, 1, 1, 1 },
	{ VirtualKey::Down,       3, 2, 1, 1 },     { VirtualKey::PageDown,   3, 3, 1, 1 },

    { VirtualKey::Back, 4, 0, 1, 1 }, { VirtualKey::Enter, 4, 1, 1, 3 }
};

static std::map<VirtualKey, CanvasTextLayout^> key_labels;

static unsigned int num0 = static_cast<unsigned int>(VirtualKey::Number0);
static unsigned int pad0 = static_cast<unsigned int>(VirtualKey::NumberPad0);

/*************************************************************************************************/
Numpad::Numpad(IPlanet* master, float fontsize) : Keyboard(master, keys) {
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
			case VirtualKey::Subtract: label = "-"; break;
			case VirtualKey::Decimal: label = "."; break;
			case VirtualKey::Back: label = L"←"; break;
			case VirtualKey::Enter: label = L"↵"; break;
			case VirtualKey::PageUp: label = L"↟"; break;
			case VirtualKey::Up: label = L"↑"; break;
			case VirtualKey::PageDown: label = L"↡"; break;
			case VirtualKey::Down: label = L"↓"; break;
			default: label = (static_cast<unsigned int>(key) - pad0).ToString();
			}

			key_labels.insert(std::pair<VirtualKey, CanvasTextLayout^>(key, make_text_layout(label, label_font)));
		}
	}
}

void Numpad::construct() {
	Color fg = Colours::Background->Color;
	Color bg = Colours::Foreground->Color;
	
	this->foreground = make_solid_brush(fg, 1.0);
	this->background = make_solid_brush(rgba(bg, 0.8));
	this->border = make_solid_brush(rgba(fg, 0.618));
	this->highlight = make_solid_brush(rgba(fg, 0.382));
	this->taplight = make_solid_brush(rgba(bg, 0.618));
}

void Numpad::draw_before(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	ds->FillRoundedRectangle(x, y, this->width, this->height, this->radius, this->radius, this->background);
}

void Numpad::draw_cell(CanvasDrawingSession^ ds, VirtualKey key, bool focused, bool tapped, float x, float y, float width, float height) {
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

bool Numpad::on_char(VirtualKey key, bool wargrey_keyboard) {
	if ((VirtualKey::Number0 <= key) && (key <= VirtualKey::Number9)) {
		key = static_cast<VirtualKey>(static_cast<unsigned int>(key) - num0 + pad0);
	}

	return Keyboard::on_char(key, wargrey_keyboard);
}
