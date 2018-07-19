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

static unsigned int cylinder_default_colors[] = { 0x00BFFF, 0xB3F000, 0xFFB03A, 0xFFB03A };
static float cylinder_default_color_positions[] = { 0.0F, 0.625F, 0.75F, 1.0F };

/*************************************************************************************************/
Cylinderlet::Cylinderlet(float tmin, float tmax, float width, float height, GradientStops^ colors, CanvasSolidColorBrush^ bcolor)
	: Cylinderlet(tmin, tmax, width, height, 0, colors, bcolor) {}

Cylinderlet::Cylinderlet(float tmin, float tmax, float width, float height, unsigned int step, GradientStops^ colors, CanvasSolidColorBrush^ bcolor)
	: IRangelet(tmin, tmax), width(width), height(height), thickness(width * 0.0618F), step((step == 0) ? 10 : step), border_color(bcolor) {
	
	if (this->height < 0.0F) {
		this->height *= (-this->width);
	}

	this->liquid_surface_radius = this->thickness;
	this->colors = ((colors == nullptr) ? make_gradient_stops(cylinder_default_colors, cylinder_default_color_positions) : colors);
}

void Cylinderlet::construct() {
	float body_x, body_y, body_height;
	float hatch_height = this->height * 0.95F;
	float hatch_y = (this->height - hatch_height) * 0.5F;
	float base_height = hatch_y;
	float base_y = this->height - base_height;
	float hat_height = base_height * 0.75F;
	float base_corner_radius = base_height * 0.618F;
	float hat_corner_radius = hat_height * 0.5F;
	auto hatch = vhatch(hatch_height, this->vmin, this->vmax, this->step, this->thickness, nullptr, &body_x, &body_y, &body_height);
	float base_width = (this->width - body_x) * 1.2F;
	float base_x = this->width - base_width;
	float glass_thickness = this->thickness * 0.5F;
	float glass_offset = glass_thickness * 0.5F;
	float glass_width = base_width * 0.618F;
	float glass_height = body_height + glass_thickness;
	float glass_x = base_x + (base_width - glass_width) * 0.5F;
	float glass_y = body_y + hatch_y - glass_offset;
	float hat_width = glass_width + hat_corner_radius * 2.0F;
	float hat_x = base_x + (base_width - hat_width) * 0.5F;
	float body_width = glass_width - glass_thickness;

	CanvasGeometry^ glass_parts[] = {
		rounded_rectangle(hat_x, 0.0F, hat_width, hat_height, hat_corner_radius, hat_corner_radius),
		geometry_stroke(rounded_rectangle(glass_x, glass_y, glass_width, glass_height, glass_thickness, glass_thickness), glass_thickness),
		rounded_rectangle(base_x, base_y, base_width, base_height * 2.0F, base_corner_radius, base_corner_radius)
	};

	auto glass = geometry_union(glass_parts); // don't mind, it's Visual Studio's fault

	body_x = glass_x + glass_offset;
	body_y = glass_y + glass_offset;

	this->body = rectangle(body_x, body_y, body_width, body_height);
	this->skeleton = geometry_freeze(geometry_intersect(glass, rectangle(this->width, this->height)));
	this->mark = geometry_freeze(hatch->Transform(make_translation_matrix(0.0F, hatch_y)));

	this->on_value_changed(0.0F);
}

void Cylinderlet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->width, h, this->height);
}

void Cylinderlet::on_value_changed(float v) {
	Rect region = this->body->ComputeBounds();
	float p = this->get_percentage();
	float hollow_height = region.Height * (1.0F - p) + this->liquid_surface_radius;
	float hollow_y = region.Y - this->liquid_surface_radius;
	auto hollow = rounded_rectangle(region.X, hollow_y, region.Width, hollow_height, this->liquid_surface_radius, this->liquid_surface_radius);

	this->color = make_solid_brush(gradient_discrete_color(this->colors, p));
	this->liquid = geometry_freeze(geometry_subtract(this->body, hollow));
}

void Cylinderlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	ds->DrawCachedGeometry(this->liquid, x, y, this->color);
	ds->DrawCachedGeometry(this->mark, x, y, this->border_color);
	ds->DrawCachedGeometry(this->skeleton, x, y, this->border_color);
}
