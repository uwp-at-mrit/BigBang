#include "graphlet/dashboard/cylinderlet.hpp"

#include "shape.hpp"
#include "paint.hpp"
#include "hatch.hpp"
#include "geometry.hpp"
#include "colorspace.hpp"
#include "transformation.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static CanvasSolidColorBrush^ cylinder_default_border_color = Colours::make(0xBBBBBB);
static unsigned int cylinder_default_colors[] = { 0x00BFFF, 0xB3F000, 0xFFB03A, 0xFFB03A };
static float cylinder_default_color_positions[] = { 0.0F, 0.625F, 0.75F, 1.0F };

/*************************************************************************************************/
Cylinderlet::Cylinderlet(float range, float width, float height, unsigned int step, GradientStops^ colors, CanvasSolidColorBrush^ bcolor)
	: Cylinderlet(0.0F, range, width, height, step, colors, bcolor) {}

Cylinderlet::Cylinderlet(float vmin, float vmax, float width, float height, unsigned int step, GradientStops^ colors, CanvasSolidColorBrush^ bcolor)
	: Cylinderlet(LiquidSurface::Convex, MarkPosition::Left, vmin, vmax, width, height, step, colors, bcolor) {}

Cylinderlet::Cylinderlet(LiquidSurface shape, float range, float width, float height, unsigned int step, GradientStops^ colors, CanvasSolidColorBrush^ bcolor)
	: Cylinderlet(shape, 0.0F, range, width, height, step, colors, bcolor) {}

Cylinderlet::Cylinderlet(LiquidSurface shape, float vmin, float vmax, float width, float height, unsigned int step, GradientStops^ colors, CanvasSolidColorBrush^ bcolor)
	: Cylinderlet(shape, MarkPosition::Left, vmin, vmax, width, height, step, colors, bcolor) {}

Cylinderlet::Cylinderlet(MarkPosition mark, float range, float width, float height, unsigned int step, GradientStops^ colors, CanvasSolidColorBrush^ bcolor)
	: Cylinderlet(mark, 0.0F, range, width, height, step, colors, bcolor) {}

Cylinderlet::Cylinderlet(MarkPosition mark, float vmin, float vmax, float width, float height, unsigned int step, GradientStops^ colors, CanvasSolidColorBrush^ bcolor)
	: Cylinderlet(LiquidSurface::Convex, mark, vmin, vmax, width, height, step, colors, bcolor) {}

Cylinderlet::Cylinderlet(LiquidSurface shape, MarkPosition position, float range, float width, float height, unsigned int step
	, GradientStops^ colors, CanvasSolidColorBrush^ bcolor)
	: Cylinderlet(shape, position, 0.0F, range, width, height, step, colors, bcolor) {}

Cylinderlet::Cylinderlet(LiquidSurface shape, MarkPosition position, float vmin, float vmax, float width, float height, unsigned int step
	, GradientStops^ colors, CanvasSolidColorBrush^ bcolor)
	: IRangelet(vmin, vmax), width(width), height(height), thickness(3.0F)
	, step((step == 0) ? 10 : step), border_color((bcolor == nullptr) ? cylinder_default_border_color : bcolor)
	, liquid_shape(shape), mark_position(position) {
	
	if (this->height < 0.0F) {
		this->height *= (-this->width);
	} else if (this->height == 0.0F) {
		this->height = this->width * 2.718F;
	}

	this->liquid_surface_radius = this->thickness;
	this->colors = ((colors == nullptr) ? make_gradient_stops(cylinder_default_colors, cylinder_default_color_positions) : colors);
}

void Cylinderlet::construct() {
	CanvasGeometry^ hatch;
	Rect measure_box;
	float hatch_height = this->height * 0.95F;
	float hatch_y = (this->height - hatch_height) * 0.5F;
	float base_height = hatch_y;
	float base_y = this->height - base_height;
	float hat_height = base_height * 0.75F;
	float base_corner_radius = base_height * 0.618F;
	float hat_corner_radius = hat_height * 0.5F;

	if (this->mark_position == MarkPosition::Left) {
		hatch = vlhatch(hatch_height, this->vmin, this->vmax, this->step, this->thickness, &measure_box);
		this->mark_x = 0.0F;
	} else {
		hatch = vrhatch(hatch_height, this->vmin, this->vmax, this->step, this->thickness, &measure_box);
		this->mark_x = this->width - measure_box.Width;
	}

	{ // the only difference between left-cylinder and right-cylinder is the `base_x`; 
		float base_width = (this->width - measure_box.Width) * 1.2F;
		float base_x = ((this->mark_position == MarkPosition::Left) ? (this->width - base_width) : 0.0F);
		float glass_thickness = this->thickness * 0.5F;
		float glass_offset = glass_thickness * 0.5F;
		float glass_width = base_width * 0.618F;
		float glass_height = measure_box.Height + glass_thickness;
		float glass_x = base_x + (base_width - glass_width) * 0.5F;
		float glass_y = measure_box.Y + hatch_y - glass_offset;
		float hat_width = glass_width + hat_corner_radius * 2.0F;
		float hat_x = base_x + (base_width - hat_width) * 0.5F;
		float body_height = measure_box.Height;
		float body_width = glass_width - glass_thickness;
		
		CanvasGeometry^ glass_parts[] = {
			rounded_rectangle(hat_x, 0.0F, hat_width, hat_height, hat_corner_radius, hat_corner_radius),
			geometry_stroke(rounded_rectangle(glass_x, glass_y, glass_width, glass_height, glass_thickness, glass_thickness), glass_thickness),
			rounded_rectangle(base_x, base_y, base_width, base_height * 2.0F, base_corner_radius, base_corner_radius)
		};

		auto glass = geometry_union(glass_parts); // don't mind, it's Visual Studio's fault
		float body_x = glass_x + glass_offset;
		float body_y = glass_y + glass_offset;

		this->body = rectangle(body_x, body_y, body_width, body_height);
		this->skeleton = geometry_freeze(geometry_intersect(glass, rectangle(this->width, this->height)));
		this->mark = geometry_freeze(hatch->Transform(make_translation_matrix(0.0F, hatch_y)));
	}

	this->on_value_changed(0.0F);
}

void Cylinderlet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->width, h, this->height);
}

void Cylinderlet::on_value_changed(float v) {
	float percentage = this->get_percentage();
	
	this->color = make_solid_brush(gradient_discrete_color(this->colors, percentage));
	this->liquid = geometry_freeze(this->make_liquid_shape(percentage));
}

void Cylinderlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	ds->FillGeometry(this->body, x, y, Colours::Background);
	ds->DrawCachedGeometry(this->liquid, x, y, this->color);
	ds->DrawCachedGeometry(this->mark, x + this->mark_x, y, this->border_color);
	ds->DrawCachedGeometry(this->skeleton, x, y, this->border_color);
}

CanvasGeometry^ Cylinderlet::make_liquid_shape(float percentage) {
	Rect region = this->body->ComputeBounds();
	CanvasGeometry^ liquid;
	
	switch (this->liquid_shape) {
	case LiquidSurface::Convex: {
		float r = this->liquid_surface_radius;
		float liquid_height = region.Height * percentage + r;
		float liquid_y = region.Y + region.Height + r - liquid_height;
		
		liquid = geometry_intersect(body, rounded_rectangle(region.X, liquid_y, region.Width, liquid_height, r, r));
	}; break;
	case LiquidSurface::Concave: {
		float r = this->liquid_surface_radius;
		float hollow_height = region.Height * (1.0F - percentage) + r;
		float hollow_y = region.Y - r;
		
		liquid = geometry_subtract(body, rounded_rectangle(region.X, hollow_y, region.Width, hollow_height, r, r));
	}; break;
	default: {
		float hollow_height = region.Height * (1.0F - percentage);

		liquid = geometry_subtract(this->body, rectangle(region.X, region.Y, region.Width, hollow_height));
	}
	}

	return liquid;
}
