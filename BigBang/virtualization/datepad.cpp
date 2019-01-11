#include <map>

#include "virtualization/datepad.hpp"

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

const static KeyboardCell keys[] = {
	{ VirtualKey::Left,     0, 2, 1, 1 },
	{ VirtualKey::Escape,   1, 2, 1, 1 },
	{ VirtualKey::Right,    2, 2, 1, 1 },

	{ VirtualKey::PageUp,   1, 0, 1, 1 },
    { VirtualKey::Up,       1, 1, 1, 1 },
	{ VirtualKey::Down,     1, 3, 1, 1 },
    { VirtualKey::PageDown, 1, 4, 1, 1 },
};

static std::map<VirtualKey, CanvasTextLayout^> key_labels;

/*************************************************************************************************/
Datepad::Datepad(IPlanet* master, float fontsize) : Keyboard(master, keys) {
	CanvasTextFormat^ label_font = make_text_format("Consolas", fontsize);
	
	this->current_key = VirtualKey::None;

	{ // Keyboard::construct()
		TextExtent te = get_text_extent("0", label_font);
		float em = te.height;
		
		this->gapsize = em * 0.271F;
		this->cellsize = em * 1.00F;
		this->radius = this->gapsize * 0.5F;
	}

	if (key_labels.size() == 0) {
		Platform::String^ label;

		for (unsigned int i = 0; i < sizeof(keys) / sizeof(KeyboardCell); i++) {
			VirtualKey key = keys[i].key;

			switch (key) {
			case VirtualKey::PageUp: label = L"↟"; break;
			case VirtualKey::Up: label = L"↑"; break;
			case VirtualKey::Down: label = L"↓"; break;
			case VirtualKey::PageDown: label = L"↡"; break;
			case VirtualKey::Left: label = L"←"; break;
			case VirtualKey::Right: label = L"→"; break;
			case VirtualKey::Escape: label = L"↺"; break;
			}

			key_labels.insert(std::pair<VirtualKey, CanvasTextLayout^>(key, make_text_layout(label, label_font)));
		}
	}
}

void Datepad::fill_auto_position(float* x, float* y, IGraphlet* g, GraphletAnchor a) {
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

CanvasTextLayout^ Datepad::key_label(VirtualKey key) {
	CanvasTextLayout^ label = nullptr;
	auto maybe_label = key_labels.find(key);

	if (maybe_label != key_labels.end()) {
		label = maybe_label->second;
	}

	return label;
}
