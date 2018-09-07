#include "graphlet/dashboard/overflowlet.hpp"

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

static CanvasSolidColorBrush^ overflow_default_color = Colours::DarkGray;
static CanvasSolidColorBrush^ overflow_default_liquid_color = Colours::DarkKhaki;
static CanvasSolidColorBrush^ overflow_default_hatch_color = Colours::GhostWhite;

/*************************************************************************************************/
OverflowPipelet::OverflowPipelet(double range, float width, float height, unsigned int step
	, unsigned int precision, ICanvasBrush^ color, ICanvasBrush^ liquid_color, CanvasSolidColorBrush^ hatchmark_color)
	: IRangelet(0.0, range), width(std::fabsf(width)), height(height), thickness(1.0F), step(step), precision(precision)
	, color((color == nullptr) ? overflow_default_color : color)
	, liquid_color((liquid_color == nullptr) ? overflow_default_liquid_color : liquid_color)
	, hatch_color((hatchmark_color == nullptr) ? overflow_default_hatch_color : hatchmark_color) {

	if (this->height < 0.0F) {
		this->height *= (-this->width);
	} else if (this->height == 0.0F) {
		this->height = this->width;
	}
}

void OverflowPipelet::construct() {
	float hatch_thickness = this->thickness * 0.5F;
	VHatchMarkMetrics metrics;
	
	this->hatchmark = vhatchmark(this->height, this->vmin, this->vmax, this->step,
		this->thickness, &metrics, this->precision);

	{ // make overflowlet
		Rect hatchmark_box = this->hatchmark->ComputeBounds();

		this->ofubase = hatchmark_box.Width * 1.4F;
		this->ofbbase = hatchmark_box.Width - (metrics.hatch_width + metrics.gap_space) * 2.0F;
		this->em = metrics.em;

		this->set_value(0.0, true);
		this->set_liquid_height(0.0, true);
	}
}

void OverflowPipelet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->width, h, this->height);
}


void OverflowPipelet::set_liquid_height(double h, bool force) {
	if (force || (this->liquid_height != h)) {
		this->liquid_height = h;
		this->on_liquid_height_changed(h);
	}
}

void OverflowPipelet::on_value_changed(double v) {
	float ofheight = this->get_outlet_height(this->get_percentage());
	float ofux = (this->width - ofubase) * 0.5F;
	float ofbx = (this->width - ofbbase) * 0.5F;
	float ofhheight = this->em * 2.0F; // the height of the hopper shape
	float ofbottom = this->height - (this->em - this->thickness) * 0.5F;
	float pipe_height = ofheight - ofhheight;
	auto trapezium = trapezoid(ofux, ofbottom - ofheight, ofubase, ofbbase, ofhheight);
	auto pipe = ((pipe_height > 0.0F) ? rectangle(ofbx, ofbottom - pipe_height, ofbbase, pipe_height) : blank());

	this->body = geometry_union(trapezium, pipe);

	this->on_liquid_height_changed(this->liquid_height);
}

void OverflowPipelet::on_liquid_height_changed(double h) {
	double percentage = h / (this->vmax - this->vmin);
	Rect region = this->body->ComputeBounds();
	float liquid_height = this->get_outlet_height(percentage);
	float liquid_y = region.Y + region.Height - liquid_height;
	auto liquid = rectangle(0.0F, liquid_y, this->width, liquid_height);

	this->liquid = geometry_freeze(geometry_subtract(liquid, this->body));
}

void OverflowPipelet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	Rect hatchmark_box = this->hatchmark->ComputeBounds();

	ds->DrawCachedGeometry(this->liquid, x, y, this->liquid_color);
	ds->FillGeometry(this->body, x, y, this->color);
	ds->DrawGeometry(this->body, x, y, this->liquid_color, this->thickness);
	ds->FillGeometry(this->hatchmark, x + (this->width - hatchmark_box.Width) * 0.5F, y, this->hatch_color);
}

float OverflowPipelet::get_outlet_height(double percentage) {
	return (this->height - this->em) * float(percentage) + this->thickness * 0.5F;
}
