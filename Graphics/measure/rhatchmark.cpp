#include <algorithm>

#include "measure/rhatchmark.hpp"

#include "box.hpp"
#include "text.hpp"
#include "string.hpp"

#include "math.hpp"
#include "shape.hpp"
#include "paint.hpp"
#include "geometry.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;
using namespace Windows::Foundation::Numerics;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Geometry;

static CanvasTextFormat^ default_mark_font = make_bold_text_format(14.0F);
static const float hatch_long_ratio = 0.618F;
static const float mark_space_ratio = 0.618F;

inline static Platform::String^ make_mark_string(double mark, unsigned int precision) {
	return flstring(mark, precision);
}

static CanvasGeometry^ make_rhatch(RHatchMarkMetrics* metrics, float radius, double degrees0, double degreesn
	, double interval, unsigned int step, float thickness, bool no_short) {
	CanvasPathBuilder^ hatch = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float px, py;
	float rstart = degrees_to_radians(degrees0);
	float rsweep = degrees_to_radians(degreesn - degrees0);
	float long_radius = metrics->ring_radius + metrics->hatch_width;
	float short_radius = metrics->ring_radius + metrics->hatch_width * (((step % 2 == 1) || no_short) ? 1.0F : 0.618F);

	circle_point(metrics->ring_radius, degrees0, &metrics->arc_sx, &metrics->arc_sy);
	circle_point(metrics->ring_radius, degreesn, &metrics->arc_ex, &metrics->arc_ey);
	
	hatch->BeginFigure(metrics->arc_sx, metrics->arc_sy);
	hatch->AddArc(float2(0.0F, 0.0F), metrics->ring_radius, metrics->ring_radius, rstart, rsweep);
	for (unsigned int i = 0; i <= step; i++) {
		double theta = interval * double(i) + degrees0;

		hatch->EndFigure(CanvasFigureLoop::Open);
		circle_point(metrics->ring_radius, theta, &px, &py);
		hatch->BeginFigure(px, py);
		circle_point(((i % 2 == 0) ? long_radius : short_radius), theta, &px, &py);
		hatch->AddLine(px, py);
	}
	hatch->EndFigure(CanvasFigureLoop::Open);

	return geometry_stroke(CanvasGeometry::CreatePath(hatch), thickness, make_roundcap_stroke_style(true));
}

static unsigned int resolve_step(float radius, double degrees0, double degreesn, double vmin, double vmax, float em, unsigned int precision) {
	double range = (vmax - vmin) * std::pow(10.0, precision + 2);
	double arclength = arc_length(radius, degrees0, degreesn);
	double available_height = double(arclength - em);
	unsigned int max_fxstep = ((unsigned int)(std::floor(available_height / (double(em) * 1.618))));
	unsigned int fxstep = 2;

	for (unsigned int step = max_fxstep; step > 2; step--) {
		double interval = range / double(step);
		unsigned int fxinterval = (unsigned int)interval;

		if (interval == double(fxinterval)) {
			if (fxinterval % 10 == 0) {
				if ((step < 10) || (step % 2 == 0)) {
					fxstep = step;
					break;
				}
			}
		}
	}
	
	return fxstep;
}

static double resolve_interval(unsigned int* step, float radius, double degrees0, double degreesn, double vmin, double vmax
	, float em, unsigned int precision, unsigned int* skip, double* diff) {
	unsigned int fxstep = (((*step) == 0) ? resolve_step(radius, degrees0, degreesn, vmin, vmax, em, precision) : (*step));
	double delta = (vmax - vmin) / double(fxstep);
	double interval = double(degreesn - degrees0) / double(fxstep);

	(*step) = fxstep;
	SET_BOX(skip, (fxstep % 2 == 0) ? 2 : 1);
	SET_BOX(diff, delta);

	return interval;
}

/*************************************************************************************************/
RHatchMarkMetrics WarGrey::SCADA::rhatchmark_metrics(float radius, double vmin, double vmax
	, float thickness, unsigned int precision, CanvasTextFormat^ font) {
	RHatchMarkMetrics metrics;
	Platform::String^ min_mark = make_mark_string(vmin, precision);
	Platform::String^ max_mark = make_mark_string(vmax, precision);
	Platform::String^ longer_mark = ((max_mark->Length() > min_mark->Length()) ? max_mark : min_mark);
	TextExtent te = get_text_extent(longer_mark, ((font == nullptr) ? default_mark_font : font));
	
	metrics.span = longer_mark->Length();
	metrics.ch = (te.width - te.lspace - te.rspace) / float(metrics.span);
	metrics.em = te.height - te.tspace - te.bspace;
	metrics.hatch_width = metrics.em * hatch_long_ratio;
	metrics.gap_space = metrics.em * mark_space_ratio + thickness;
	metrics.ring_radius = radius - metrics.hatch_width - metrics.gap_space - metrics.ch;

	return metrics;
}

CanvasGeometry^ WarGrey::SCADA::rhatchmark(float radius, double degrees0, double degreesn
	, double vmin, double vmax, unsigned int step, float thickness, RHatchMarkMetrics* maybe_metrics
	, unsigned int precision, bool no_short, CanvasTextFormat^ ft) {
	unsigned int skip;
	double diff;
	float mcx, mcy;
	TextExtent te;
	CanvasTextFormat^ font = ((ft == nullptr) ? default_mark_font : ft);
	RHatchMarkMetrics metrics = rhatchmark_metrics(radius, vmin, vmax, thickness, precision, font);
	double interval = resolve_interval(&step, radius, degrees0, degreesn, vmin, vmax, metrics.em, precision, &skip, &diff);
	auto hatchmark = make_rhatch(&metrics, radius, degrees0, degreesn, interval, step, thickness, no_short);

	metrics.label_lx = FLT_MAX;
	metrics.label_rx = FLT_MIN;
	metrics.label_ty = FLT_MAX;
	metrics.label_by = FLT_MIN;

	for (unsigned int i = 0; i <= step; i += (no_short ? 1 : skip)) {
		Platform::String^ mark = make_mark_string(vmax - diff * double(i), precision);
		CanvasGeometry^ p = paragraph(make_text_layout(mark, font), &te);
		float half_width = te.width * 0.5F;
		float half_height = te.height * 0.5F;

		circle_point(radius, degreesn - interval * double(i), &mcx, &mcy);
		hatchmark = geometry_union(hatchmark, p, mcx - half_width, mcy - half_height);

		metrics.label_lx = std::fminf(metrics.label_lx, mcx - half_width + te.lspace);
		metrics.label_rx = std::fmaxf(metrics.label_rx, mcx + half_width - te.rspace);
		metrics.label_ty = std::fminf(metrics.label_ty, mcy - half_height + te.tspace);
		metrics.label_by = std::fmaxf(metrics.label_by, mcy + half_height - te.bspace);
	}

	SET_BOX(maybe_metrics, metrics);

	return hatchmark;
}
