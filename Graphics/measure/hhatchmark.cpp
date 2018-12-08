#include <algorithm>

#include "measure/hhatchmark.hpp"

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

inline static Platform::String^ make_mark_string(double mark, unsigned int precision) {
	return flstring(mark, precision);
}

inline unsigned int mark_span(Platform::String^ mark) {
	// TODO: resolve the mark language
	return mark->Length();
}

static Platform::String^ resolve_longest_mark(Platform::String^ marks[], size_t count) {
	Platform::String^ longest_mark = marks[0];
	unsigned int longest_span = mark_span(marks[0]);

	for (size_t idx = 1; idx < count; idx++) {
		unsigned int this_span = mark_span(marks[idx]);

		if (this_span > longest_span) {
			longest_span = this_span;
			longest_mark = marks[idx];
		}
	}

	return longest_mark;
}

static inline void fill_consistent_hhatch_metrics(CanvasTextFormat^ maybe_font, float thickness, float* hatch_height, float* gapsize) {
	TextExtent css_metrics = get_text_extent("0", ((maybe_font == nullptr) ? default_mark_font : maybe_font));
	float chwidth = css_metrics.width;

	SET_BOX(hatch_height, chwidth * hatch_long_ratio + thickness);
	SET_BOX(gapsize, chwidth * mark_space_ratio + thickness);
}

static CanvasGeometry^ make_hthatch(HHatchMarkMetrics* metrics, float interval, unsigned int step, float thickness, bool no_short) {
	CanvasPathBuilder^ hatch = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float x = metrics->hatch_x;
	float y = metrics->hatch_y;
	float height = metrics->hatch_height;
	float short_y = y + height * (((step % 2 == 1) || no_short) ? 0.0F : 0.382F);
	
	metrics->hatch_width = interval * step;

	hatch->BeginFigure(x, y + height);
	hatch->AddLine(x + metrics->hatch_width, y + height);
	for (unsigned int i = 0; i <= step; i++) {
		float xthis = interval * float(i) + x;

		hatch->EndFigure(CanvasFigureLoop::Open);
		hatch->BeginFigure(xthis, ((i % 2 == 0) ? y : short_y));
		hatch->AddLine(xthis, y + height);
	}
	hatch->EndFigure(CanvasFigureLoop::Open);

	return geometry_stroke(CanvasGeometry::CreatePath(hatch), thickness, make_roundcap_stroke_style(true));
}

static CanvasGeometry^ make_hbhatch(HHatchMarkMetrics* metrics, float interval, unsigned int step, float thickness, bool no_short) {
	CanvasPathBuilder^ hatch = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float x = metrics->hatch_x;
	float y = metrics->hatch_y;
	float height = metrics->hatch_height;
	float short_y = y + height * (((step % 2 == 1) || no_short) ? 1.0F : 0.618F);
	
	metrics->hatch_width = interval * step;

	hatch->BeginFigure(x, y);
	hatch->AddLine(x + metrics->hatch_width, y);
	for (unsigned int i = 0; i <= step; i++) {
		float xthis = interval * float(i) + x;

		hatch->EndFigure(CanvasFigureLoop::Open);
		hatch->BeginFigure(xthis, (i % 2 == 0) ? y + height : short_y);
		hatch->AddLine(xthis, y);
	}
	hatch->EndFigure(CanvasFigureLoop::Open);

	return geometry_stroke(CanvasGeometry::CreatePath(hatch), thickness, make_roundcap_stroke_style(true));
}

static unsigned int resolve_step(double vmin, double vmax, float width, float lspace, float rspace, unsigned int precision) {
	double range = (vmax - vmin) * std::pow(10.0, precision + 2);
	unsigned int max_fxstep = ((unsigned int)(std::floor(double(width) / double(lspace + rspace))));
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

static float resolve_interval(unsigned int* step, double vmin, double vmax, float width, float lspace, float rspace
	, unsigned int precision, unsigned int* skip, double* diff) {
	unsigned int fxstep = (((*step) == 0) ? resolve_step(vmin, vmax, width, lspace, rspace, precision) : (*step));
	double delta = (vmax - vmin) / double(fxstep);
	float interval = (width - lspace - rspace)  / float(fxstep);

	(*step) = fxstep;
	SET_BOX(skip, (fxstep % 2 == 0) ? 2 : 1);
	SET_BOX(diff, delta);

	return interval;
}

/*************************************************************************************************/
HHatchMarkMetrics WarGrey::SCADA::hhatchmark_metrics(double vmin, double vmax, float thickness, unsigned int precision, CanvasTextFormat^ font) {
	HHatchMarkMetrics metrics;
	Platform::String^ min_mark = make_mark_string(vmin, precision);
	Platform::String^ max_mark = make_mark_string(vmax, precision);
	unsigned int min_span = mark_span(min_mark);
	unsigned int max_span = mark_span(max_mark);
	Platform::String^ longer_mark = ((max_span > min_span) ? max_mark : min_mark);
	TextExtent te = get_text_extent(longer_mark, ((font == nullptr) ? default_mark_font : font));
	unsigned int longer_span = longer_mark->Length();

	fill_consistent_hhatch_metrics(font, thickness, &metrics.hatch_height, &metrics.gap_space);

	metrics.ch = te.width / float(longer_span);
	metrics.em = te.height - te.tspace - te.bspace;
	metrics.top_space = te.tspace;

	metrics.hatch_x = metrics.ch * min_span * 0.5F;
	metrics.hatch_rx = metrics.ch * max_span * 0.5F;
	
	metrics.height = metrics.em + metrics.gap_space + metrics.hatch_height;

	return metrics;
}

CanvasGeometry^ WarGrey::SCADA::hthatchmark(float width, double vmin, double vmax, unsigned int step, float thickness
	, HHatchMarkMetrics* maybe_metrics, unsigned int precision, bool no_short, CanvasTextFormat^ ft) {
	unsigned int skip;
	double diff;
	TextExtent te;
	CanvasTextFormat^ font = ((ft == nullptr) ? default_mark_font : ft);
	HHatchMarkMetrics metrics = hhatchmark_metrics(vmin, vmax, thickness, precision, font);
	float interval = resolve_interval(&step, vmin, vmax, width, metrics.hatch_x, metrics.hatch_rx, precision, &skip, &diff);
	float mark_ty = -metrics.top_space;
	
	metrics.hatch_y = metrics.height - metrics.hatch_height;

	auto hatchmark = make_hthatch(&metrics, interval, step, thickness, no_short);
	for (unsigned int i = 0; i <= step; i += (no_short ? 1 : skip)) {
		Platform::String^ mark = make_mark_string(vmin + diff * double(i), precision);
		auto p = paragraph(make_text_layout(mark, font), &te);
		float tx = metrics.hatch_x + interval * float(i) - te.width * 0.5F;

		hatchmark = geometry_union(hatchmark, p, tx, mark_ty);
	}

	SET_BOX(maybe_metrics, metrics);

	return hatchmark;
}

CanvasGeometry^ WarGrey::SCADA::hbhatchmark(float width, double vmin, double vmax, unsigned int step, float thickness
	, HHatchMarkMetrics* maybe_metrics, unsigned int precision, bool no_short, CanvasTextFormat^ ft) {
	unsigned int skip;
	double diff;
	TextExtent te;
	CanvasTextFormat^ font = ((ft == nullptr) ? default_mark_font : ft);
	HHatchMarkMetrics metrics = hhatchmark_metrics(vmin, vmax, thickness, precision, font);
	float mark_ty = metrics.hatch_height + metrics.gap_space - metrics.top_space;
	float interval = resolve_interval(&step, vmin, vmax, width, metrics.hatch_x, metrics.hatch_rx, precision, &skip, &diff);
	
	metrics.hatch_y = thickness * 0.5F;

	auto hatchmark = make_hbhatch(&metrics, interval, step, thickness, no_short);
	for (unsigned int i = 0; i <= step; i += (no_short ? 1 : skip)) {
		Platform::String^ mark = make_mark_string(vmin + diff * double(i), precision);
		auto p = paragraph(make_text_layout(mark, font), &te);
		float tx = metrics.hatch_x + interval * float(i) - te.width * 0.5F;

		hatchmark = geometry_union(hatchmark, p, tx, mark_ty);
	}

	SET_BOX(maybe_metrics, metrics);

	return hatchmark;
}
