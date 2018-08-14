#include "graphlet/device/gaugelet.hpp"

#include "text.hpp"
#include "paint.hpp"
#include "tongue.hpp"
#include "shape.hpp"
#include "hatch.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

/*************************************************************************************************/
IGaugelet::IGaugelet(float vmin, float vmax, float width, float height, ICanvasBrush^ color, ICanvasBrush^ bcolor, ICanvasBrush^ mcolor)
	: height(height), min_value(vmin), max_value(vmax), color(color), body_color(bcolor), mark_color(mcolor) {
	Rect measure_box;
	auto font = make_text_format(8.0F);
	auto marks = vlhatchmark(height, vmin, vmax, 3.0F, &measure_box, font);

	this->body_x = measure_box.Width;
	this->body_y = measure_box.Y;
	this->body_width = width - this->body_x;
	this->body_height = measure_box.Height;
	this->scale_marks = geometry_freeze(marks);
}

void IGaugelet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->body_x + this->body_width, h, this->height);
}

void IGaugelet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float range = this->max_value - this->min_value;
	float value_height = this->get_value() - this->min_value;
	float scale_height = fminf(value_height * this->body_height / range, this->body_height);
	float empty_height = this->body_height - scale_height;

	ds->FillRectangle(x + this->body_x, y + this->body_y + empty_height, this->body_width, scale_height, this->color);
	ds->FillRectangle(x + this->body_x, y + this->body_y, this->body_width, empty_height, this->body_color);
	ds->DrawCachedGeometry(this->scale_marks, x, y, this->mark_color);
}

/*************************************************************************************************/
LevelGaugelet::LevelGaugelet(float range, float width, float height, ICanvasBrush^ color, ICanvasBrush^ bcolor, ICanvasBrush^ mcolor)
	: IGaugelet(0.0F, range, width, height, color, bcolor, mcolor) {}
