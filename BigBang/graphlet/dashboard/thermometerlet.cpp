#include "graphlet/dashboard/thermometerlet.hpp"

#include "shape.hpp"
#include "paint.hpp"
#include "geometry.hpp"
#include "transformation.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static unsigned int default_colors[] = {
	0x385BFE, 0x385BFE, 0x385BFE,
	0xB3F000, 0xB3F000, 0xB3F000, 0xB3F000,
	0xFFB03A, 0xFFB03A, 0xFFB03A
};

static CanvasGeometry^ make_thermometer_glass(float width, float height, float thickness, float* tty = nullptr, float* tby = nullptr) {
	CanvasPathBuilder^ glass = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float offset = thickness * 0.5F;
	float bradius = (width - thickness) * 0.5F;
	float bulb_width = bradius * 2.0F;
	float tube_width = bulb_width * 0.618F;
	float tradius = tube_width * 0.5F;
	float tube_lx = (bulb_width - tube_width) * 0.5F + offset;
	float tube_ty = tradius + offset;
	float tube_rx = bulb_width - tube_lx + thickness;
	float tube_by = height - bradius * (std::sinf(std::acosf(tradius / bradius)) +1.0F) - offset;

	glass->BeginFigure(tube_rx, tube_ty);
	glass->AddArc(float2(tube_lx, tube_ty), tradius, tradius, 0.0F, CanvasSweepDirection::CounterClockwise, CanvasArcSize::Small);
	glass->AddLine(tube_lx, tube_by);
	glass->AddArc(float2(tube_rx, tube_by), bradius, bradius, 0.0F, CanvasSweepDirection::CounterClockwise, CanvasArcSize::Large);
	glass->AddLine(tube_rx, tube_ty);
	glass->EndFigure(CanvasFigureLoop::Closed);

	SET_BOX(tty, tube_ty);
	SET_BOX(tby, tube_by);

	return geometry_stroke(CanvasGeometry::CreatePath(glass), thickness);
}

static CanvasGeometry^ make_thermometer_hatch(float width, float height, float thickness) {
	CanvasStrokeStyle^ style = ref new CanvasStrokeStyle();
	CanvasPathBuilder^ hatch = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	unsigned int step = 7;
	float short_x = width * (1.0F - 0.618F);
	float interval = height / float(step);
	
	style->StartCap = CanvasCapStyle::Round;
	style->EndCap = CanvasCapStyle::Round;

	hatch->BeginFigure(width, thickness);
	for (unsigned int i = 0; i < step; i++) {
		float ythis = interval * float(i) + thickness;

		hatch->EndFigure(CanvasFigureLoop::Open);
		hatch->BeginFigure((i % 3 == 0) ? thickness : short_x, ythis);
		hatch->AddLine(width, ythis);
	}
	hatch->EndFigure(CanvasFigureLoop::Open);

	return geometry_stroke(CanvasGeometry::CreatePath(hatch), thickness, style);
}

static CanvasGeometry^ make_thermometer_mercury(float bulb_width, float height) {
	float bulb_radius = bulb_width * 0.5F;
	float cx = bulb_radius;
	float cy = height - bulb_radius;
	float tube_width = bulb_width * (1.0F - 0.618F);
	float tube_x = (bulb_width - tube_width) * 0.5F;
	float tradius = tube_width * 0.5F;
	
	CanvasGeometry^ bulb = circle(cx, cy, bulb_radius);
	CanvasGeometry^ bulb_hollow = circle(cx, cy, tradius);
	CanvasGeometry^ glass = rounded_rectangle(tube_x, 0.0F, tube_width, cy - tradius, tradius, tradius);

	return geometry_subtract(geometry_union(glass, bulb), bulb_hollow);
}

/*************************************************************************************************/
Thermometerlet::Thermometerlet(float width, float height, ICanvasBrush^ bcolor, GradientStops^ stops)
	: Thermometerlet(-30.0F, 50.0F, width, height, bcolor, stops) {}

Thermometerlet::Thermometerlet(float tmin, float tmax, float width, float height, ICanvasBrush^ bcolor, GradientStops^ stops)
	: IRangelet(tmin,tmax), width(width), height(height), thickness(width * 0.0618F)
	, bulb_size(width * 0.618F), border_color(bcolor) {
	GradientStops^ cs = ((stops == nullptr) ? make_gradient_stops(default_colors) : stops);

	if (this->height < 0.0F) {
		this->height *= (-this->width);
	} else if (this->height == 0.0F) {
		this->height = this->bulb_size * 3.2F;
	}

	this->mercury_color = make_linear_gradient_brush(0.0F, -this->height, 0.0F, 0.0F, cs);
}

void Thermometerlet::construct() {
	float tube_ty, tube_by;
	float hatch_ratio = 0.85F;
	CanvasGeometry^ glass = make_thermometer_glass(this->bulb_size, this->height, this->thickness, &tube_ty, &tube_by);
	CanvasGeometry^ hatch = make_thermometer_hatch(this->width - this->bulb_size, (tube_by - tube_ty) * hatch_ratio, this->thickness);

	glass = glass->Transform(make_translation_matrix(this->width - this->bulb_size, 0.0F));
	hatch = hatch->Transform(make_translation_matrix(0.0F, tube_ty + (tube_by - tube_ty) * (1.0F - hatch_ratio) * 0.5F));
	this->skeleton = geometry_freeze(geometry_union(glass, hatch));
}

void Thermometerlet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->width, h, this->height);
}

void Thermometerlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float Tpercentage = this->get_percentage();

	if (Tpercentage >= 0.0F) {
		float bulb_cx = x + (this->width - this->bulb_size * 0.5F);
		float bulb_cy = y + (this->height - this->bulb_size * 0.5F);
		float mercury_width = this->bulb_size * 0.5F;
		float mercury_tube_height = this->height - this->bulb_size - mercury_width;
		float mercury_height = bulb_size + mercury_tube_height * Tpercentage;
		CanvasGeometry^ mercury = make_thermometer_mercury(mercury_width, mercury_height);
		
		brush_translate(this->mercury_color, x, y);

		ds->FillGeometry(mercury,
			bulb_cx - mercury_width * 0.5F,
			bulb_cy - mercury_height + mercury_width * 0.5F,
			this->mercury_color);
	}

	ds->DrawCachedGeometry(this->skeleton, x, y, this->border_color);
}
