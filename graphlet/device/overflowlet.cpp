#include "graphlet/device/overflowlet.hpp"

#include "datum/string.hpp"
#include "datum/flonum.hpp"
#include "measure/vhatchmark.hpp"

#include "text.hpp"
#include "math.hpp"
#include "shape.hpp"
#include "paint.hpp"
#include "geometry.hpp"
#include "colorspace.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static OverflowPipeStyle default_overflow_pipe_style;

static CanvasSolidColorBrush^ overflow_default_body_color = Colours::DimGray;
static CanvasSolidColorBrush^ overflow_default_liquid_color = Colours::DarkKhaki;
static CanvasSolidColorBrush^ overflow_default_target_color = Colours::Chocolate;
static CanvasSolidColorBrush^ overflow_default_border_color = Colours::Aquamarine;
static CanvasSolidColorBrush^ overflow_default_hatch_color = Colours::GhostWhite;

static CanvasSolidColorBrush^ overflow_default_auto_color = Colours::LightSkyBlue;
static CanvasSolidColorBrush^ overflow_default_disabled_color = Colours::Crimson;

/*************************************************************************************************/
OverflowPipelet::OverflowPipelet(double range, float width, float height, unsigned int step, unsigned int precision)
	: OverflowPipelet(default_overflow_pipe_style, range, width, height, step, precision) {}

OverflowPipelet::OverflowPipelet(OverflowPipeStyle& style, double range, float width, float height, unsigned int step, unsigned int precision)
	: IRangelet(0.0, range), width(flabs(width)), height(height), thickness(1.0F), step(step), precision(precision), leftward(width < 0.0F)
	, style(style), auto_mode(false), disabled(false) {
	if (this->height == 0.0F) {
		this->height = this->width * 1.618F;
	}
}

void OverflowPipelet::construct() {
	float hatch_thickness = this->thickness * 0.5F;
	VHatchMarkMetrics metrics;
	
	this->prepare_style();
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
	if (force || (this->liquid_height != flsafe(h, this->liquid_height))) {
		this->liquid_height = h;
		this->on_liquid_height_changed(h);
	}
}

void OverflowPipelet::set_target_height(double h, bool force) {
	if (force || (this->target_height != flsafe(h, this->liquid_height))) {
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
	auto target_line = line(0.0F, line_y, this->width, line_y, 1.0F, this->style.target_style);
	auto target_meter = paragraph(flstring(h, this->precision), this->style.target_font, &te);
	float meter_x = (this->leftward ? 0.0F : (this->width - te.width) * 1.0F);
	float meter_y = line_y - (te.height - te.tspace - te.bspace) * 0.5F - te.tspace;

	this->target = geometry_freeze(geometry_union(target_line, target_meter, meter_x, meter_y));
}

void OverflowPipelet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	Rect hatchmark_box = this->hatchmark->ComputeBounds();

	ds->DrawCachedGeometry(this->liquid, x, y, this->style.liquid_color);
	ds->FillGeometry(this->body, x, y, (this->auto_mode ? this->style.auto_color : this->style.body_color));
	ds->DrawGeometry(this->body, x, y, this->style.border_color, this->thickness);
	ds->FillGeometry(this->hatchmark, x + (this->width - hatchmark_box.Width) * 0.5F, y, this->style.hatch_color);
	
	if (this->target != nullptr) {
		ds->DrawCachedGeometry(this->target, x, y, this->style.target_color);
	}

	if (this->disabled) {
		float cx = x + this->width * 0.5F;
		float cy = y + this->height * 0.5F;
		float r = flmin(this->width, this->height) * 0.25F;
		float sx, sy, ex, ey;

		circle_point(r, 45.0, &sx, &sy);
		circle_point(r, 225.0, &ex, &ey);

		ds->DrawLine(sx + cx, sy + cy, ex + cx, ey + cy, this->style.disabled_color, this->style.disable_icon_thickness);
		ds->DrawCircle(cx, cy, r, this->style.disabled_color, this->style.disable_icon_thickness);
	}
}

float OverflowPipelet::get_outlet_height(double percentage) {
	return (this->height - this->em) * float(percentage) + this->thickness * 0.5F;
}

void OverflowPipelet::prepare_style() {
	CAS_SLOT(this->style.body_color, overflow_default_body_color);
	CAS_SLOT(this->style.border_color, overflow_default_border_color);
	CAS_SLOT(this->style.liquid_color, overflow_default_liquid_color);
	CAS_SLOT(this->style.target_color, overflow_default_target_color);
	CAS_SLOT(this->style.hatch_color, overflow_default_hatch_color);

	CAS_SLOT(this->style.auto_color, overflow_default_auto_color);
	CAS_SLOT(this->style.disabled_color, overflow_default_disabled_color);

	CAS_SLOT(this->style.target_style, make_dash_stroke(CanvasDashStyle::Dash));
	CAS_SLOT(this->style.target_font, make_bold_text_format());

	FLCAS_SLOT(this->style.disable_icon_thickness, 4.0F);
}

void OverflowPipelet::disable(bool on) {
	if (this->disabled != on) {
		this->disabled = on;
		this->notify_updated();
	}
}

void OverflowPipelet::set_auto_mode(bool on) {
	if (this->auto_mode != on) {
		this->auto_mode = on;
		this->notify_updated();
	}
}
