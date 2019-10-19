#include <map>

#include "graphlet/ui/colorpickerlet.hpp"

#include "palette/x11.hpp"
#include "palette/xterm256.hpp"

#include "text.hpp"
#include "tongue.hpp"

#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::UI;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

static CanvasTextFormat^ button_default_font = make_bold_text_format("Consolas", 16.0F);

namespace {
	private class Picker : public ISatellite {
	public:
		Picker(IPalette* palette) : ISatellite(Log::Info, palette->name()), palette(palette) {}

		void fill_extent(float* width, float* height) {
			SET_BOX(width, 400.0F);
			SET_BOX(height, 400.0F);
		}

	public:
		void load(CanvasCreateResourcesReason reason, float width, float height) override { }

		void reflow(float width, float height) override { }

	private: // never delete these graphlets manually.

	private:
		IPalette* palette;
	};


	static std::map<Palette, Picker*> pickers;

	static Picker* make_picker(Palette ptype) {
		if (pickers.find(ptype) == pickers.end()) {
			IPalette* palette = nullptr;

			switch (ptype) {
			case Palette::X11:      palette = X11Palette::instance(); break;
			case Palette::Xterm256: palette = Xterm256Palette::instance(); break;
			}

			pickers.insert(std::pair<Palette, Picker*>(ptype, new Picker(palette)));
		}

		return pickers[ptype];
	}
}

/*************************************************************************************************/
ColorPickerlet::ColorPickerlet(Palette ptype, float width, float height) : ColorPickerlet(ptype, nullptr, width, height) {}
ColorPickerlet::ColorPickerlet(Palette ptype, CanvasSolidColorBrush^ color, float width, float height) : _color(color), width(width), height(height), ptype(ptype) {
	this->enable_events(true, false);
}

void ColorPickerlet::fill_extent(float x, float y, float* width, float* height) {
	SET_BOX(width, this->width);
	SET_BOX(height, this->height);
}

void ColorPickerlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	if ((this->_color == nullptr) || (this->_color->Color.A == 0)) {
		ds->DrawRectangle(x + 0.5F, y + 0.5F, Width - 1.0F, Height - 1.0F, Colours::GrayText);
	} else {
		ds->FillRectangle(x, y, Width, Height, this->_color);
	}
}

void ColorPickerlet::color(CanvasSolidColorBrush^ color) {
	this->_color = color;
}

CanvasSolidColorBrush^ ColorPickerlet::color() {
	return this->_color;
}

void ColorPickerlet::own_caret(bool is_own) {
	if (is_own) {
		if (this->picker == nullptr) {
			this->picker = make_picker(this->ptype);
		}

		//this->picker->show();
	}
}
