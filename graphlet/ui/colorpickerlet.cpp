#include "graphlet/ui/colorpickerlet.hpp"

using namespace WarGrey::DTPM;
using namespace WarGrey::SCADA;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

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
			this->picker = ColorPicker::get_instance(this->ptype);
		}

		//this->picker->show();
	}
}
