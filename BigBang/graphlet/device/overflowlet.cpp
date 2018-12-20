#include "graphlet/device/overflowlet.hpp"

#include "text.hpp"
#include "shape.hpp"
#include "paint.hpp"
#include "string.hpp"
#include "geometry.hpp"
#include "colorspace.hpp"

#include "measure/vhatchmark.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static CanvasSolidColorBrush^ overflow_default_color = Colours::DimGray;
static CanvasSolidColorBrush^ overflow_default_liquid_color = Colours::DarkKhaki;
static CanvasSolidColorBrush^ overflow_default_target_color = Colours::Chocolate;
static CanvasSolidColorBrush^ overflow_default_border_color = Colours::Aquamarine;
static CanvasSolidColorBrush^ overflow_default_hatch_color = Colours::GhostWhite;

/*************************************************************************************************/
OverflowPipelet::OverflowPipelet(double range, float width, float height, unsigned int step, unsigned int precision, ICanvasBrush^ color
	, ICanvasBrush^ liquid_color, ICanvasBrush^ target_color, ICanvasBrush^ border_color, CanvasSolidColorBrush^ hatchmark_color)
	: IRangelet(0.0, range), width(std::fabsf(width)), height(height), thickness(1.0F), step(step), precision(precision)
	, color((color == nullptr) ? overflow_default_color : color)
	, liquid_color((liquid_color == nullptr) ? overflow_default_liquid_color : liquid_color)
	, target_color((target_color == nullptr) ? overflow_default_target_color : target_color)
	, border_color((border_color == nullptr) ? overflow_default_border_color : border_color)
	, hatch_color((hatchmark_color == nullptr) ? overflow_default_hatch_color : hatchmark_color) {

	if (this->height == 0.0F) {
		this->height = this->width * 1.618F;
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

void OverflowPipelet::set_target_height(double h, bool force) {
	if (force || (this->target_height != h)) {
		if (this->target_style == nullptr) {
			this->target_style = make_dash_stroke(CanvasDashStyle::Dash);
			this->target_font = make_bold_text_format();
		}

		this->target_height = h;
		this->on_target_height_changed(h);
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
	Rect region = this->body->ComputeBounds();
	double percentage = h / (this->vmax - this->vmin);
	float liquid_height = this->get_outlet_height(percentage);
	float liquid_y = region.Y + region.Height - liquid_height;
	auto liquid = rectangle(0.0F, liquid_y, this->width, liquid_height + 1.0F);

	this->liquid = geometry_freeze(geometry_subtract(liquid, this->body));
}

void OverflowPipelet::on_target_height_changed(double h) {
	TextExtent te;
	Rect region = this->body->ComputeBounds();
	double percentage = h / (this->vmax - this->vmin);
	float line_y = region.Y + region.Height - this->get_outlet_height(percentage);
	auto target_line = line(0.0F, line_y, this->width, line_y, 1.0F, this->target_style);
	auto target_meter = paragraph(flstring(h, this->precision), this->target_font, &te);
	float meter_x = (this->width - te.width) * 0.5F;
	float meter_y = line_y - te.height * 0.5F;

	this->target = geometry_freeze(geometry_union(target_line, target_meter, meter_x, meter_y));
}

void OverflowPipelet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	Rect hatchmark_box = this->hatchmark->ComputeBounds();

	ds->DrawCachedGeometry(this->liquid, x, y, this->liquid_color);
	ds->FillGeometry(this->body, x, y, this->color);
	ds->DrawGeometry(this->body, x, y, this->border_color, this->thickness);
	ds->FillGeometry(this->hatchmark, x + (this->width - hatchmark_box.Width) * 0.5F, y, this->hatch_color);
	
	if (this->target != nullptr) {
		ds->DrawCachedGeometry(this->target, x, y, this->target_color);
	}
}

float OverflowPipelet::get_outlet_height(double percentage) {
	return (this->height - this->em) * float(percentage) + this->thickness * 0.5F;
}
