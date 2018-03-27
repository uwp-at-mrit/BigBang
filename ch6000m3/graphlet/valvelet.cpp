#include "valvelet.hpp"

#include "shape.hpp"
#include "paint.hpp"
#include "geometry.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

Valvelet::Valvelet(float radius, double degrees, float thickness, Color& color, Color& border_color)
	: radius(radius), degrees(degrees), thickness((thickness <= 0.0F) ? 1.0F : thickness) {
	this->color = make_solid_brush(color);
	this->border_color = make_solid_brush(border_color);
	this->triangle = geometry_freeze(::triangle(radius, degrees));
}

void Valvelet::fill_extent(float x, float y, float* w, float* h) {
	float length = radius * 2.0F + this->thickness;

	SET_BOXES(w, h, length);
}

double Valvelet::get_direction_degree() {
	return this->degrees;
}

void Valvelet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float center = this->radius;
	float thickoff = this->thickness * 0.5F;
	float cx = x + center + thickoff;
	float cy = y + center + thickoff;
	float tx = x + thickoff;
	float ty = y; // TODO: why doesn't `ty` have to add the `thickoff` 

	ds->FillCircle(cx, cy, this->radius, system_background_brush());
	ds->DrawCachedGeometry(this->triangle, tx, ty, this->color);
	ds->DrawCircle(cx, cy, this->radius, this->border_color, this->thickness);
}
