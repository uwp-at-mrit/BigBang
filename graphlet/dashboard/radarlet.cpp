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

static const long long DEFAULT_SLOT_SIZE = 4096LL;
static const unsigned int DEFAULT_COUNT_RATE = 5;

static CanvasSolidColorBrush^ lines_default_border_color = Colours::make(0xBBBBBB);
static CanvasTextFormat^ lines_default_font = make_bold_text_format(12.0F);
static CanvasTextFormat^ lines_default_legend_font = make_bold_text_format(14.0F);

/*************************************************************************************************/
IRadarlet::IRadarlet(float radius, unsigned int count) : radius(radius), count(count) {}

void IRadarlet::construct() {

}

void IRadarlet::fill_extent(float x, float y, float* w, float* h) {
	SET_BOXES(w, h, this->radius * 2.0F);
}

void IRadarlet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	float cx = x + this->radius;
	float cy = y + this->radius;
	double radian0 = -pi / 2.0;
	double rad_step = 2.0 * pi / double(this->count);
	float r = this->radius - 1.0F;

	for (unsigned int c = 1; c < this->count; c++) {
		ds->DrawCircle(cx, cy, r * float(c) / float(this->count), Colours::Azure, 1.0F);
	}

	for (unsigned int idx = 0; idx < this->count; idx++) {
		float radian = float(radian0 + rad_step * double(idx));
		float ix, iy;

		circle_point(r, radian, &ix, &iy);
		ds->DrawLine(cx, cy, cx + ix, cy + iy, Colours::Snow, 1.0F);
	}
}
