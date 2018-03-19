#include "snowflakelet.hpp"

#include "geometry.hpp"
#include "shape.hpp"
#include "paint.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::UI;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

Snowflakelet::Snowflakelet(float size, Color& color) : Iconlet(size) {
	this->color = make_solid_brush(color);
}

void Snowflakelet::construct() {
	float thickness = 2.0F;
	float center = this->size * 0.5F;

	auto part = stadium(0.0F, center - thickness, this->size - thickness - thickness, thickness);
	auto p0 = geometry_rotate(part, 90.0, center, center);
	auto p1 = geometry_rotate(part, 30.0, center, center);
	auto p2 = geometry_rotate(part, 150.0, center, center);

	this->snowflake = geometry_freeze(geometry_union(geometry_union(p0, p1), p2));
}

void Snowflakelet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	ds->DrawCachedGeometry(this->snowflake, x, y, this->color);
}
