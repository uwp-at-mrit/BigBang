#include <cmath>

#include "graphlet/gaugelet.hpp"

#include "text.hpp"
#include "paint.hpp"
#include "tongue.hpp"
#include "shape.hpp"
#include "hatch.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;
using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

/*************************************************************************************************/
IGaugelet::IGaugelet(float vmin, float vmax, float mbody_ratio, unsigned char step, ICanvasBrush^ color, ICanvasBrush^ bcolor, ICanvasBrush^ mcolor)
	: min_value(vmin), max_value(vmax), color(color), body_color(bcolor), mark_color(mcolor) {
	float mark_width;
	auto font = make_text_format(8.0F);
	auto marks = vhatch(vmin, vmax, ((step == 0) ? 10 : step), font, &mark_width, &this->body_y, &this->body_height);
	Rect box = marks->ComputeStrokeBounds(1.0F);

	this->scale_marks = geometry_freeze(marks);
	this->height = box.Height; 
	this->body_x = box.Width;
	this->body_width = mark_width * mbody_ratio;
}

void IGaugelet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->body_x + this->body_width, h, this->height);
}

void IGaugelet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float range = this->max_value - this->min_value;
	float value_height = this->get_scale() - this->min_value;
	float scale_height = fminf(value_height * this->body_height / range, this->body_height);
	float empty_height = this->body_height - scale_height;

	ds->FillRectangle(x + this->body_x, y + this->body_y + empty_height, this->body_width, scale_height, this->color);
	ds->FillRectangle(x + this->body_x, y + this->body_y, this->body_width, empty_height, this->body_color);
	ds->DrawCachedGeometry(this->scale_marks, x, y, this->mark_color);
}

/*************************************************************************************************/
LiquidGaugelet::LiquidGaugelet(float range, float mbody_ratio, unsigned char step, ICanvasBrush^ color, ICanvasBrush^ bcolor, ICanvasBrush^ mcolor)
	: IGaugelet(0.0F, range, mbody_ratio, step, color, bcolor, mcolor) {}
