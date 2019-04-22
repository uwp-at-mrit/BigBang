#include "graphlet/dashboard/cylinderlet.hpp"

#include "shape.hpp"
#include "geometry.hpp"

#include "measure/vhatchmark.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static CanvasSolidColorBrush^ cylinder_default_border_color = Colours::make(0xBBBBBB);
static unsigned int cylinder_default_colors[] = { 0x00BFFF, 0xB3F000, 0xFFB03A, 0xFFB03A };
static float cylinder_default_color_positions[] = { 0.0F, 0.625F, 0.75F, 1.0F };

/*************************************************************************************************/
Cylinderlet::Cylinderlet(double range, float base_width, float height, float thickness
	, unsigned int step, unsigned int precision, CanvasSolidColorBrush^ bcolor, GradientStops^ colors)
	: Cylinderlet(0.0, range, base_width, height, thickness, step, precision, bcolor, colors) {}

Cylinderlet::Cylinderlet(double vmin, double vmax, float base_width, float height, float thickness
	, unsigned int step, unsigned int precision, CanvasSolidColorBrush^ bcolor, GradientStops^ colors)
	: Cylinderlet(LiquidSurface::_, vmin, vmax, base_width, height, thickness, step, precision, bcolor, colors) {}

Cylinderlet::Cylinderlet(LiquidSurface shape, double range, float base_width, float height
	, float thickness, unsigned int step, unsigned int precision, CanvasSolidColorBrush^ bcolor, GradientStops^ colors)
	: Cylinderlet(shape, 0.0, range, base_width, height, thickness, step, precision, bcolor, colors) {}

Cylinderlet::Cylinderlet(LiquidSurface shape, double vmin, double vmax, float base_width, float height
	, float thickness, unsigned int step, unsigned int precision, CanvasSolidColorBrush^ bcolor, GradientStops^ colors)
	: IRangelet(vmin, vmax), base_width(fabsf(base_width)), height(height), thickness(thickness)
	, step(step), precision(precision), liquid_shape(shape), leftward(base_width > 0.0F)
	, border_color((bcolor == nullptr) ? cylinder_default_border_color : bcolor) {

	if (this->height == 0.0F) {
		this->height = this->base_width * 2.718F;
	}

	this->liquid_surface_radius = this->thickness * 0.618F;
	this->colors = ((colors == nullptr) ? make_gradient_stops(cylinder_default_colors, cylinder_default_color_positions) : colors);
}

void Cylinderlet::construct() {
	CanvasGeometry^ hatch;
	VHatchMarkMetrics metrics;
	float hatch_height = this->height * 0.95F;
	float hatchmark_y = (this->height - hatch_height) * 0.5F;
	float base_height = hatchmark_y;
	float base_y = this->height - base_height;
	float hat_height = base_height * 0.75F;
	float base_corner_radius = base_height * 0.618F;
	float hat_corner_radius = hat_height * 0.5F;
	float hatch_thickness = this->thickness * 0.75F;
	float mark_x = 0.0F;

	if (this->leftward) {
		hatch = vlhatchmark(hatch_height, this->vmin, this->vmax, this->step, hatch_thickness, &metrics, this->precision);
	} else {
		hatch = vrhatchmark(hatch_height, this->vmin, this->vmax, this->step, hatch_thickness, &metrics, this->precision);
		mark_x = this->base_width - metrics.width;
	}

	this->extended_width = metrics.width + hatch_thickness - base_corner_radius * 2.0F;

	{ // the only difference between left-cylinder and right-cylinder is the `base_x`;
		float base_x = (this->leftward ? this->extended_width : 0.0F);
		float glass_thickness = this->thickness * 0.5F;
		float glass_thickoff = glass_thickness * 0.5F;
		float glass_width = this->base_width - base_corner_radius * 4.0F - this->thickness;
		float glass_height = metrics.hatch_height + glass_thickness;
		float glass_x = base_x + (this->base_width - glass_width) * 0.5F;
		float glass_y = metrics.hatch_y + hatchmark_y - glass_thickoff;
		float hat_width = glass_width + hat_corner_radius * 2.0F;
		float hat_x = base_x + (this->base_width - hat_width) * 0.5F;
		float body_height = metrics.hatch_height;
		float body_width = glass_width - glass_thickness;
		
		CanvasGeometry^ glass_parts[] = {
			geometry_translate(hatch, mark_x, hatchmark_y),
			rounded_rectangle(hat_x, 0.0F, hat_width, hat_height, hat_corner_radius, hat_corner_radius),
			geometry_stroke(rounded_rectangle(glass_x, glass_y, glass_width, glass_height, glass_thickness, glass_thickness), glass_thickness),
			rounded_rectangle(base_x, base_y, this->base_width, base_height * 2.0F, base_corner_radius, base_corner_radius)
		};

		auto glass = geometry_union(glass_parts); // don't mind, it's Visual Studio's fault
		float body_x = glass_x + glass_thickoff;
		float body_y = glass_y + glass_thickoff;

		this->body = rectangle(body_x, body_y, body_width, body_height);
		this->skeleton = geometry_freeze(geometry_intersect(glass, rectangle(this->base_width + this->extended_width, this->height)));
	}

	this->set_value(0.0, true);
}

void Cylinderlet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->base_width + this->extended_width, h, this->height);
}

void Cylinderlet::on_value_changed(double v) {
	double percentage = this->get_percentage();
	
	this->color = make_solid_brush(gradient_discrete_color(this->colors, percentage));
	this->liquid = geometry_freeze(this->make_liquid_shape(percentage));
}

void Cylinderlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	ds->FillGeometry(this->body, x, y, Colours::Background);
	ds->DrawCachedGeometry(this->liquid, x, y, this->color);
	ds->DrawCachedGeometry(this->skeleton, x, y, this->border_color);
}

CanvasGeometry^ Cylinderlet::make_liquid_shape(double percentage) {
	Rect region = this->body->ComputeBounds();
	CanvasGeometry^ liquid;
	
	switch (this->liquid_shape) {
	case LiquidSurface::Convex: {
		float r = this->liquid_surface_radius;
		float hollow_height = region.Height * float(1.0 - percentage) + r;
		float hollow_y = region.Y - r;
		
		liquid = geometry_subtract(body, rounded_rectangle(region.X, hollow_y, region.Width, hollow_height, r, r));
	}; break;
	case LiquidSurface::Concave: {
		float r = this->liquid_surface_radius;
		float liquid_height = region.Height * float(percentage) + r;
		float liquid_y = region.Y + region.Height + r - liquid_height;

		liquid = geometry_intersect(body, rounded_rectangle(region.X, liquid_y, region.Width, liquid_height, r, r));
	}; break;
	default: {
		float hollow_height = region.Height * float(1.0 - percentage);

		liquid = geometry_subtract(this->body, rectangle(region.X, region.Y, region.Width, hollow_height));
	}
	}

	return liquid;
}
