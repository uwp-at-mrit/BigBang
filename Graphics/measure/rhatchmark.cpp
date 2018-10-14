#include <algorithm>

#include "measure/rhatchmark.hpp"

#include "box.hpp"
#include "text.hpp"
#include "shape.hpp"
#include "paint.hpp"
#include "geometry.hpp"

#include "string.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Geometry;

static CanvasTextFormat^ default_mark_font = make_bold_text_format(12.0F);
static const float hatch_long_ratio = 0.618F;
static const float mark_space_ratio = 0.618F;

inline static Platform::String^ make_rmark_string(double mark, unsigned int precision) {
	return flstring(mark, precision);
}

inline static Platform::String^ make_lmark_string(double mark, unsigned int precision, unsigned int span, float* span_off) {
	Platform::String^ s = make_rmark_string(mark, precision);
	(*span_off) = float(span - s->Length());

	return s;
}

inline unsigned int mark_span(Platform::String^ mark) {
	// TODO: resolve the mark language
	return mark->Length();
}

inline float discrete_weight_position(float height, double weight) {
	return height * float(std::fmax(std::fmin(1.0 - weight, 1.0), 0.0));
}

static RHatchMarkMetrics make_input_vmetrics(float width, float height, float thickness) {
	RHatchMarkMetrics metrics;

	metrics.hatch_x = thickness * 0.5F;
	metrics.hatch_y = thickness * 0.5F;
	metrics.hatch_width = width - thickness;
	metrics.hatch_height = height - thickness;

	return metrics;
}

static void fill_consistent_vhatch_metrics(CanvasTextFormat^ maybe_font, float thickness, float* hatch_width, float* gapsize) {
	TextExtent css_metrics = get_text_extent("0", ((maybe_font == nullptr) ? default_mark_font : maybe_font));
	float chwidth = css_metrics.width;

	SET_BOX(hatch_width, chwidth * hatch_long_ratio + thickness);
	SET_BOX(gapsize, chwidth * mark_space_ratio);
}

static CanvasGeometry^ make_vlhatch(RHatchMarkMetrics* metrics, float interval, unsigned int step, float thickness) {
	CanvasPathBuilder^ hatch = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float x = metrics->hatch_x;
	float y = metrics->hatch_y;
	float width = metrics->hatch_width;
	float short_x = x + width * ((step % 2 == 0) ? 0.382F : 0.0F);
	
	metrics->hatch_height = interval * step;

	hatch->BeginFigure(x + width, y);
	hatch->AddLine(x + width, y + metrics->hatch_height);
	for (unsigned int i = 0; i <= step; i++) {
		float ythis = interval * float(i) + y;

		hatch->EndFigure(CanvasFigureLoop::Open);
		hatch->BeginFigure((i % 2 == 0) ? x : short_x, ythis);
		hatch->AddLine(x + width, ythis);
	}
	hatch->EndFigure(CanvasFigureLoop::Open);

	return geometry_stroke(CanvasGeometry::CreatePath(hatch), thickness, make_roundcap_stroke_style(true));
}

static CanvasGeometry^ make_vlhatch(RHatchMarkMetrics& metrics, double weights[], size_t count, float thickness) {
	CanvasPathBuilder^ hatch = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float x = metrics.hatch_x;
	float y = metrics.hatch_y;
	float width = metrics.hatch_width;
	float height = metrics.hatch_height;
	
	hatch->BeginFigure(x + width, y);
	hatch->AddLine(x + width, y + height);
	for (size_t i = 0; i < count; i++) {
		float ythis = discrete_weight_position(height, weights[i]) + y;

		hatch->EndFigure(CanvasFigureLoop::Open);
		hatch->BeginFigure(x, ythis);
		hatch->AddLine(x + width, ythis);
	}
	hatch->EndFigure(CanvasFigureLoop::Open);

	return geometry_stroke(CanvasGeometry::CreatePath(hatch), thickness, make_roundcap_stroke_style(true));
}

static CanvasGeometry^ make_vrhatch(RHatchMarkMetrics* metrics, float interval, unsigned int step, float thickness) {
	CanvasPathBuilder^ hatch = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float x = metrics->hatch_x;
	float y = metrics->hatch_y;
	float width = metrics->hatch_width;
	float short_x = x + width * ((step % 2 == 0) ? 0.618F : 1.0F);
	
	metrics->hatch_height = interval * step;

	hatch->BeginFigure(x, y);
	hatch->AddLine(x, y + metrics->hatch_height);
	for (unsigned int i = 0; i <= step; i++) {
		float ythis = interval * float(i) + y;

		hatch->EndFigure(CanvasFigureLoop::Open);
		hatch->BeginFigure((i % 2 == 0) ? x + width : short_x, ythis);
		hatch->AddLine(x, ythis);
	}
	hatch->EndFigure(CanvasFigureLoop::Open);

	return geometry_stroke(CanvasGeometry::CreatePath(hatch), thickness, make_roundcap_stroke_style(true));
}

static CanvasGeometry^ make_vrhatch(RHatchMarkMetrics& metrics, double weights[], size_t count, float thickness) {
	CanvasPathBuilder^ hatch = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float x = metrics.hatch_x;
	float y = metrics.hatch_y;
	float width = metrics.hatch_width;
	float height = metrics.hatch_height;

	hatch->BeginFigure(x, y);
	hatch->AddLine(x, y + height);
	for (size_t i = 0; i < count; i++) {
		float ythis = discrete_weight_position(height, weights[i]) + y;

		hatch->EndFigure(CanvasFigureLoop::Open);
		hatch->BeginFigure(x + width, ythis);
		hatch->AddLine(x, ythis);
	}
	hatch->EndFigure(CanvasFigureLoop::Open);

	return geometry_stroke(CanvasGeometry::CreatePath(hatch), thickness, make_roundcap_stroke_style(true));
}

static unsigned int resolve_step(double vmin, double vmax, float height, float em, unsigned int precision) {
	double range = (vmax - vmin) * std::pow(10.0, precision + 1);
	double available_height = double(height - em);
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

static float resolve_interval(unsigned int* step, double vmin, double vmax, float height, float em
	, unsigned int precision, unsigned int* skip, double* diff) {
	unsigned int fxstep = (((*step) == 0) ? resolve_step(vmin, vmax, height, em, precision) : (*step));
	double range = vmax - vmin;
	double delta = range / double(fxstep);
	float hatch_height = height - em;
	float interval = hatch_height / float(fxstep);

	(*step) = fxstep;
	SET_BOX(skip, (fxstep % 2 == 0) ? 2 : 1);
	SET_BOX(diff, delta);

	return interval;
}

/*************************************************************************************************/
RHatchMarkMetrics WarGrey::SCADA::rhatchmark_metrics(double degrees0, double degreesn, double vmin, double vmax, float thickness
	, unsigned int precision, CanvasTextFormat^ font) {
	RHatchMarkMetrics metrics;
	Platform::String^ min_mark = make_rmark_string(vmin, precision);
	Platform::String^ max_mark = make_rmark_string(vmax, precision);
	Platform::String^ longer_mark = ((max_mark->Length() > min_mark->Length()) ? max_mark : min_mark);
	unsigned int longer_span = longer_mark->Length();
	TextExtent te = get_text_extent(longer_mark, ((font == nullptr) ? default_mark_font : font));
	
	fill_consistent_vhatch_metrics(font, thickness, &metrics.hatch_width, &metrics.gap_space);

	metrics.ch = te.width / float(longer_span);
	metrics.em = te.height - te.tspace - te.bspace;

	metrics.mark_width = te.width;
	metrics.span = longer_span;
	metrics.top_space = te.tspace;

	metrics.hatch_y = metrics.em * 0.5F;
	metrics.width = metrics.mark_width + metrics.gap_space + metrics.hatch_width;

	return metrics;
}

CanvasGeometry^ WarGrey::SCADA::rhatchmark(float radiusX, float radiusY, double degrees0, double degreesn
	, double vmin, double vmax, unsigned int step, float thickness, RHatchMarkMetrics* maybe_metrics
	, unsigned int precision, CanvasTextFormat^ ft) {
	unsigned int skip;
	float mark_span_off;
	double diff;
	CanvasTextFormat^ font = ((ft == nullptr) ? default_mark_font : ft);
	RHatchMarkMetrics metrics = rhatchmark_metrics(degrees0, degreesn, vmin, vmax, thickness, precision, font);
	float interval = resolve_interval(&step, vmin, vmax, radiusY, metrics.em, precision, &skip, &diff);
	float mark_x = metrics.hatch_width + metrics.gap_space;

	metrics.hatch_x = thickness * 0.5F;
	auto lhatchmark = make_vlhatch(&metrics, interval, step, thickness);
	metrics.hatch_x = mark_x + metrics.mark_width + metrics.gap_space;
	auto rhatchmark = make_vrhatch(&metrics, interval, step, thickness);

	auto hatchmark = geometry_union(lhatchmark, rhatchmark);
	for (unsigned int i = 0; i <= step; i += skip) {
		Platform::String^ mark = make_lmark_string(vmax - diff * double(i), precision, metrics.span, &mark_span_off);
		float tx = mark_x + mark_span_off * metrics.ch * 0.5F;
		float ty = interval * float(i) - metrics.top_space;

		hatchmark = geometry_union(hatchmark, paragraph(make_text_layout(mark, font)), tx, ty);
	}

	SET_BOX(maybe_metrics, metrics);

	return hatchmark;
}
