#include "graphlet/dashboard/indicatorlet.hpp"

#include "math.hpp"
#include "shape.hpp"
#include "geometry.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static const float start_degrees = 135.0;
static const float end_degrees = 405.0;

Indicatorlet::Indicatorlet(float size, float thickness, ICanvasBrush^ lcolor, ICanvasBrush^ ncolor, ICanvasBrush^ hcolor, ICanvasBrush^ color)
	: size(size), thickness(thickness), color(color), normal_color(ncolor), low_color(lcolor), high_color(hcolor) {

	if (this->thickness < 0.0F) {
		this->thickness *= (-size);
	} else if (this->thickness == 0.0F) {
		this->thickness = size * 0.0618F;
	}
}

void Indicatorlet::construct() {
	float radius = (this->size - this->thickness) * 0.5F;
	auto ring = long_arc(start_degrees, end_degrees, radius, radius, this->thickness);
	auto box = ring->ComputeBounds();

	this->bspace = this->size - box.Height;
	this->body_ring = geometry_freeze(ring);
}

void Indicatorlet::fill_extent(float x, float y, float* w, float* h) {
	SET_BOXES(w, h, this->size);
}

void Indicatorlet::fill_margin(float x, float y, float* t, float* r, float* b, float* l) {
	SET_BOXES(l, r, 0.0F);
	SET_VALUES(t, 0.0F, b, this->bspace);
}

void Indicatorlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float percentage = fmax(fmin(this->get_value(), 1.0F), 0.0F);
	float indicator_degrees = start_degrees + (end_degrees - start_degrees) * percentage;
	float cx = x + this->size * 0.5F;
	float cy = y + this->size * 0.5F;

	ds->DrawCachedGeometry(this->body_ring, cx, cy, this->color);

	if (indicator_degrees > start_degrees) {
		float radius = (this->size - this->thickness) * 0.5F;
		CanvasGeometry^ indicator = arc(start_degrees, indicator_degrees, radius, radius, this->thickness);
		ICanvasBrush^ color = this->normal_color;

		if (percentage <= 0.2F) {
			color = this->low_color;
		} else if (percentage >= 0.8F) {
			color = this->high_color;
		}

		ds->FillGeometry(indicator, cx, cy, color);
	}
}
