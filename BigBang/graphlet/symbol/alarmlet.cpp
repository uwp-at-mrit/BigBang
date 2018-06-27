#include "graphlet/symbol/alarmlet.hpp"

#include "paint.hpp"
#include "brushes.hxx"

using namespace WarGrey::SCADA;

using namespace Windows::UI;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;

Alarmlet::Alarmlet(float size) : width(size), height(size) {}

void Alarmlet::fill_extent(float x, float y, float* w, float* h) {
    SET_BOX(w, this->width);
	SET_BOX(h, this->height);
};

void Alarmlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float base_width = this->width * 0.9F;
	float base_height = this->height * 0.15F;
	float body_width = base_width * 0.618F;
	float base_radius = (base_width - body_width) * 0.25F;
	float base_x = x + (this->width - base_width) * 0.5F;
	float base_y = y + this->height - base_height;
	float body_x = x + (this->width - body_width) * 0.5F;
	float body_bottom = base_y - base_radius;
	float body_y = body_bottom - body_width;


	Colour^ colors[] = { Colours::Gray, Colours::DarkGray };
	auto stops = make_gradient_stops(colors);

	ICanvasBrush^ color = make_linear_gradient_brush(body_y, y + this->height, stops);

	ds->FillRectangle(body_x, body_y, body_width, body_width, color);
	ds->FillRoundedRectangle(base_x, base_y, base_width, base_height * 2.0F, base_radius, base_radius, color);
}
