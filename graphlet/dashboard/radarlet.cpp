#include "graphlet/dashboard/radarlet.hpp"

#include "datum/string.hpp"
#include "datum/fixnum.hpp"
#include "datum/flonum.hpp"

#include "text.hpp"
#include "math.hpp"
#include "shape.hpp"
#include "geometry.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::System;
using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static const double radian0 = -pi / 2.0;
static const float caption_size = 16.0F;

static RadarStyle default_radar_style;

static CanvasSolidColorBrush^ radar_default_color = Colours::DodgerBlue;
static CanvasSolidColorBrush^ radar_default_label_color = Colours::Snow;
static CanvasSolidColorBrush^ radar_default_ring_color = Colours::GrayText;

/*************************************************************************************************/
IRadarlet::IRadarlet(float radius, unsigned int count, float* initials)
	: IRadarlet(radius, count, default_radar_style, initials) {}

IRadarlet::IRadarlet(float radius, unsigned int count, RadarStyle& style, float* initials) : Radius(radius), count(count), style(style) {
	this->scales = new float[this->count];
	this->captions = ref new Platform::Array<Platform::String^>(this->count);

	if (initials != nullptr) {
		for (unsigned int idx = 0; idx < this->count; idx++) {
			this->scales[idx] = flmax(flmin(initials[idx], 1.0F), 0.0F);
		}
	} else {
		memset(this->scales, 0, this->count * sizeof(float));
	}
}

IRadarlet::~IRadarlet() {
	delete[] this->scales;
}

void IRadarlet::construct() {
	Platform::Array<CanvasGeometry^>^ labels = ref new Platform::Array<CanvasGeometry^>(this->count);
	CanvasGeometry^ caption = blank();
	double rad_step = 2.0 * pi / double(this->count);
	float text_height = 0.0F;
	TextExtent te;

	this->prepare_style();
	for (unsigned int idx = 0; idx < this->count; idx++) {
		CanvasGeometry^ origin = paragraph(this->captions[idx], this->style.font, &te);

		text_height = flmax(text_height, te.height);
		labels[idx] = geometry_translate(origin, te.width * -0.5F, te.height * -0.5F);
	}

	this->radius = this->Radius - text_height * 0.5F;
	for (unsigned int idx = 0; idx < this->count; idx++) {
		float radian = float(radian0 + rad_step * double(idx));
		double angle = 360.0 * double(idx) / double(this->count);
		float ix, iy;

		circle_point(this->radius, radian, &ix, &iy);
		caption = geometry_union(caption, geometry_rotate(labels[idx], angle, 0.0F, 0.0F), ix, iy);
	}

	this->caption = geometry_freeze(caption);
	this->radius = this->Radius - text_height;
	this->reshape();
}

void IRadarlet::fill_extent(float x, float y, float* w, float* h) {
	SET_BOXES(w, h, this->Radius * 2.0F);
}

void IRadarlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	double rad_step = 2.0 * pi / double(this->count);
	float cx = x + this->Radius;
	float cy = y + this->Radius;
	
	ds->DrawCachedGeometry(this->caption, cx, cy, this->style.label_color);

	for (unsigned int c = 1; c <= this->count; c++) {
		ds->DrawCircle(cx, cy, this->radius * float(c) / float(this->count), this->style.ring_color, 1.0F);
	}

	for (unsigned int idx = 0; idx < this->count; idx++) {
		float radian = float(radian0 + rad_step * double(idx));
		float ix, iy;

		circle_point(this->radius, radian, &ix, &iy);
		ds->DrawLine(cx, cy, cx + ix, cy + iy, this->style.axes_color, 1.0F);
	}

	ds->FillGeometry(this->shape, cx, cy, this->style.shape_color);
	ds->DrawGeometry(this->shape, cx, cy, this->style.outline_color, this->style.outline_thickness);
}

void IRadarlet::reshape() {
	CanvasPathBuilder^ shape = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	double rad_step = 2.0 * pi / double(this->count);
	float vx, vy;

	circle_point(this->radius * this->scales[0], float(radian0), &vx, &vy);
	shape->BeginFigure(vx, vy);

	for (unsigned int idx = 1; idx < this->count; idx++) {
		float radian = float(radian0 + rad_step * double(idx));

		circle_point(this->radius * this->scales[idx], radian, &vx, &vy);
		shape->AddLine(vx, vy);
	}

	shape->EndFigure(CanvasFigureLoop::Closed);

	this->shape = CanvasGeometry::CreatePath(shape);
}

void IRadarlet::set_scale(unsigned int index, float v) {
	if (index < this->count) {
		this->scales[index] = flmax(flmin(v, 1.0F), 0.0F);
		this->reshape();
	}
}

void IRadarlet::set_scales(float* vs) {
	for (unsigned int idx = 0; idx < this->count; idx++) {
		this->scales[idx] = flmax(flmin(vs[idx], 1.0F), 0.0F);
	}

	this->reshape();
}

void IRadarlet::prepare_style() {
	CAS_SLOT(this->style.font, make_bold_text_format(caption_size));

	CAS_SLOT(this->style.outline_color, radar_default_color);
	
	{ // transparency filling
		CanvasSolidColorBrush^ c = dynamic_cast<CanvasSolidColorBrush^>(this->style.outline_color);
		if (c != nullptr) {
			style.shape_color = Colours::make(c, 0.618);
		}
	}

	CAS_SLOT(this->style.label_color, radar_default_label_color);
	CAS_SLOT(this->style.axes_color, this->style.label_color);
	CAS_SLOT(this->style.ring_color, radar_default_ring_color);

	if (this->style.outline_thickness <= 0.0F) {
		this->style.outline_thickness = 2.0F;
	}
}
