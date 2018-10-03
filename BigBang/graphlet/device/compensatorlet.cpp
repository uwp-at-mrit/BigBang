#include "graphlet/device/compensatorlet.hpp"

#include "text.hpp"
#include "shape.hpp"
#include "paint.hpp"
#include "hatch.hpp"
#include "geometry.hpp"
#include "colorspace.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static CanvasSolidColorBrush^ compensator_default_color = Colours::DarkGray;
static CanvasSolidColorBrush^ compensator_default_progress_color = Colours::Gray;

/*************************************************************************************************/
Compensatorlet::Compensatorlet(double range, float width, float height, unsigned int step
	, unsigned int precision, ICanvasBrush^ color, ICanvasBrush^ liquid_color)
	: IRangelet(0.0, range), width(std::fabsf(width)), height(height), thickness(2.0F), step(step), precision(precision)
	, color((color == nullptr) ? compensator_default_color : color)
	, progress_color((liquid_color == nullptr) ? compensator_default_progress_color : liquid_color) {

	if (this->height == 0.0F) {
		this->height = this->width * 2.718F;
	}

	this->progress_width = this->thickness * 3.14F;
	this->pulley_size = this->width * 0.5F;
	this->base_width = this->pulley_size * 0.618F;
	this->base_height = this->height * 0.618F - this->pulley_size - this->thickness;
	this->anchor_by = this->height - this->base_height * 2.0F;
}

void Compensatorlet::construct() {
	this->pulley = circle(this->pulley_size * 0.5F);
	this->base = rectangle(this->base_width, this->base_height);

	this->set_value(0.0, true);
}

void Compensatorlet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->width, h, this->height);
}

void Compensatorlet::on_value_changed(double v) {
	float percentage = float(this->get_percentage());
	float progress_height = (this->height - this->base_height - this->pulley_size - this->thickness * 2.0F) * percentage;

	this->progress = geometry_freeze(rectangle(this->progress_width, std::fmaxf(progress_height, 0.0F)));
	this->anchor_py = this->height - this->base_height - progress_height - this->pulley_size * 0.5F - this->thickness * 1.5F;
}

void Compensatorlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float cx = x + this->width * 0.5F;
	float bx = cx - this->base_width * 0.5F;
	float by = y + this->height - this->base_height - this->thickness * 0.5F;
	float py = y + this->anchor_py + (this->pulley_size + this->thickness) * 0.5F;
	
	ds->FillGeometry(this->base, bx, by, this->progress_color);
	ds->DrawGeometry(this->base, bx, by, this->color, this->thickness);
	ds->DrawGeometry(this->pulley, cx, y + this->anchor_py, this->color, this->thickness);
	ds->DrawCachedGeometry(this->progress, cx - this->progress_width * 0.5F, py, this->progress_color);

	{ // draw lines
		float r = this->pulley_size * 0.5F;

		ds->DrawLine(cx - r, y + this->anchor_py, x,               y + this->anchor_by, this->color, this->thickness);
		ds->DrawLine(cx + r, y + this->anchor_py, x + this->width, y + this->anchor_by, this->color, this->thickness);
	}
}

float Compensatorlet::get_outlet_height(double percentage) {
	return this->base_height;
}
