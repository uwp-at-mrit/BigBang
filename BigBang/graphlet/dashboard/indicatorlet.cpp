#include "graphlet/dashboard/indicatorlet.hpp"

#include "math.hpp"
#include "shape.hpp"
#include "geometry.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static const float start_degrees = 135.0;
static const float end_degrees = 405.0;

static CanvasSolidColorBrush^ indicator_default_bgcolor = WarGrey::SCADA::Colours::make(0x505050);

static unsigned int indicator_default_colors[] = {
	0x30A1F6, 0xAEEE00, 0xFFB43D
};

/*************************************************************************************************/
Indicatorlet::Indicatorlet(float size, float thickness, ICanvasBrush^ bgcolor, GradientStops^ stops)
	: Indicatorlet(1.0F, size, thickness, bgcolor, stops) {}

Indicatorlet::Indicatorlet(float range, float size, float thickness, ICanvasBrush^ bgcolor, GradientStops^ stops)
	: Indicatorlet(0.0F, range, size, thickness, bgcolor, stops) {}

Indicatorlet::Indicatorlet(float vmin, float vmax, float size, float thickness, ICanvasBrush^ bgcolor, GradientStops^ stops)
	: IRangelet(vmin, vmax), size(size), thickness(thickness)
	, bgcolor(bgcolor == nullptr ? indicator_default_bgcolor : bgcolor) {
	if (this->thickness < 0.0F) {
		this->thickness *= (-size);
	} else if (this->thickness == 0.0F) {
		this->thickness = size * 0.0618F;
	}

	this->colors = ((stops == nullptr) ? make_gradient_stops(indicator_default_colors) : stops);
}

void Indicatorlet::construct() {
	float radius = (this->size - this->thickness) * 0.5F;
	auto ring = long_arc(start_degrees, end_degrees, radius, radius, this->thickness);
	auto box = ring->ComputeBounds();

	this->bspace = this->size - box.Height;
	this->body_ring = geometry_freeze(ring);

	this->on_value_changed(0.0F);
}

void Indicatorlet::fill_extent(float x, float y, float* w, float* h) {
	SET_BOXES(w, h, this->size);
}

void Indicatorlet::fill_margin(float x, float y, float* t, float* r, float* b, float* l) {
	SET_BOXES(l, r, 0.0F);
	SET_VALUES(t, 0.0F, b, this->bspace);
}

void Indicatorlet::on_value_changed(float v) {
	this->fgcolor = make_solid_brush(gradient_discrete_color(this->colors, this->get_percentage()));
}

void Indicatorlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float percentage = this->get_percentage();
	float indicator_degrees = start_degrees + (end_degrees - start_degrees) * percentage;
	float cx = x + this->size * 0.5F;
	float cy = y + this->size * 0.5F;

	ds->DrawCachedGeometry(this->body_ring, cx, cy, this->bgcolor);

	if (indicator_degrees > start_degrees) {
		float radius = (this->size - this->thickness) * 0.5F;
		CanvasGeometry^ indicator = arc(start_degrees, indicator_degrees, radius, radius, this->thickness);
		
		ds->FillGeometry(indicator, cx, cy, fgcolor);
	}
}
