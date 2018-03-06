#include "pumplet.hpp"

#include "shape.hpp"
#include "paint.hpp"
#include "geometry.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;

Pumplet::Pumplet(float radius, double degree, float thickness, Color& color, Color& ring_color)
	: radius(radius), thickness((thickness <= 0.0F) ? 1.0F : thickness) {
	this->color = make_solid_brush(color);
	this->ring_color = make_solid_brush(ring_color);

	this->triangle = geometry_freeze(::triangle(radius, degree));
}

void Pumplet::fill_extent(float x, float y, float* w, float* h) {
	float length = radius * 2.0F + this->thickness;

	SET_BOXES(w, h, length);
}

void Pumplet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float adjust_offset = this->thickness * 0.5F;
	float center = this->radius + adjust_offset;

	ds->DrawCachedGeometry(this->triangle, x + adjust_offset, y + adjust_offset, this->color);
	ds->DrawCircle(x + center, y + center, this->radius, this->ring_color, this->thickness);
}
