#include "graphlet/dashboard/indicatorlet.hpp"

#include "shape.hpp"
#include "geometry.hpp"
#include "system.hpp"
#include "transformation.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

Indicatorlet::Indicatorlet(float size, float thickness, ICanvasBrush^ color, ICanvasBrush^ lcolor, ICanvasBrush^ ncolor, ICanvasBrush^ hcolor)
	: size(size), thickness(thickness), color(color), normal_color(ncolor), low_color(lcolor), high_color(hcolor) {

	if (this->thickness < 0.0F) {
		this->thickness *= (-this->size);
	} else if (this->thickness == 0.0F) {
		this->thickness = this->size * 0.0618F;
	}
}

void Indicatorlet::construct() {
	
}

void Indicatorlet::fill_extent(float x, float y, float* w, float* h) {
	SET_BOXES(w, h, this->size);
}

void Indicatorlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float radius = this->size * 0.5F;
	float cx = x + radius;
	float cy = y + radius;

	ds->DrawCircle(cx, cy, radius, this->high_color);
	ds->DrawCircle(cx, cy, radius - this->thickness, this->low_color);
}
