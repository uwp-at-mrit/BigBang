#include <map>

#include "virtualization/keyboard/numpad.hpp"

#include "forward.hpp"
#include "text.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::System;

using namespace Windows::UI;
using namespace Windows::UI::Text;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;

const static KeyboardCell keys[] = {
	{ VirtualKey::NumberPad9, 2, 0, 1, 1 }, { VirtualKey::NumberPad8, 1, 0, 1, 1 }, { VirtualKey::NumberPad7, 0, 0, 1, 1 },
    { VirtualKey::NumberPad6, 2, 1, 1, 1 }, { VirtualKey::NumberPad5, 1, 1, 1, 1 }, { VirtualKey::NumberPad4, 0, 1, 1, 1 },
    { VirtualKey::NumberPad3, 2, 2, 1, 1 }, { VirtualKey::NumberPad2, 1, 2, 1, 1 }, { VirtualKey::NumberPad1, 0, 2, 1, 1 },
    { VirtualKey::Subtract,   2, 3, 1, 1 }, { VirtualKey::Decimal,    1, 3, 1, 1 }, { VirtualKey::NumberPad0, 0, 3, 1, 1 },

	{ VirtualKey::PageUp,     3, 0, 1, 1 }, { VirtualKey::Up,         3, 1, 1, 1 },
	{ VirtualKey::Down,       3, 2, 1, 1 }, { VirtualKey::PageDown,   3, 3, 1, 1 },

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

bool Numpad::on_key(VirtualKey key, bool wargrey_keyboard) {
	if ((VirtualKey::Number0 <= key) && (key <= VirtualKey::Number9)) {
		key = static_cast<VirtualKey>(static_cast<unsigned int>(key) - num0 + pad0);
	}

	return Keyboard::on_key(key, wargrey_keyboard);
}

VirtualKey Numpad::find_received_key(unsigned int keycode) {
	VirtualKey key = VirtualKey::None;

	switch (keycode) {
	case 45: key = VirtualKey::Subtract; break;
	}

	return key;
}

CanvasTextLayout^ Numpad::key_label(VirtualKey key) {
	CanvasTextLayout^ label = nullptr;
	auto maybe_label = key_labels.find(key);

	if (maybe_label != key_labels.end()) {
		label = maybe_label->second;
	}

	return label;
}
