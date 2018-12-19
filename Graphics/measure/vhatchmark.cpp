#include <algorithm>

#include "measure/vhatchmark.hpp"

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

static Platform::String^ resolve_longest_mark(Platform::String^ marks[], size_t count, unsigned int* span) {
	Platform::String^ longest_mark = marks[0];
	unsigned int longest_span = mark_span(marks[0]);

	for (size_t idx = 1; idx < count; idx++) {
		unsigned int this_span = mark_span(marks[idx]);

		if (this_span > longest_span) {
			longest_span = this_span;
			longest_mark = marks[idx];
		}
	}

	SET_BOX(span, longest_span);

	return longest_mark;
}

static VHatchMarkMetrics make_input_vmetrics(float width, float height, float thickness) {
	VHatchMarkMetrics metrics;

	metrics.hatch_x = thickness * 0.5F;
	metrics.hatch_y = thickness * 0.5F;
	metrics.hatch_width = width - thickness;
	metrics.hatch_height = height - thickness;

	return metrics;
}

static inline void fill_consistent_vhatch_metrics(CanvasTextFormat^ maybe_font, float thickness, float* hatch_width, float* gapsize) {
	TextExtent css_metrics = get_text_extent("0", ((maybe_font == nullptr) ? default_mark_font : maybe_font));
	float chwidth = css_metrics.width;

	SET_BOX(hatch_width, chwidth * hatch_long_ratio + thickness);
	SET_BOX(gapsize, chwidth * mark_space_ratio + thickness);
}

static CanvasGeometry^ make_vlhatch(VHatchMarkMetrics* metrics, float interval, unsigned int step, float thickness, bool no_short) {
	CanvasPathBuilder^ hatch = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float x = metrics->hatch_x;
	float y = metrics->hatch_y;
	float width = metrics->hatch_width;
	float short_x = x + width * (((step % 2 == 1) || no_short) ? 0.0F : 0.382F);
	
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

static CanvasGeometry^ make_vlhatch(VHatchMarkMetrics& metrics, double weights[], size_t count, float thickness) {
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

static CanvasGeometry^ make_vrhatch(VHatchMarkMetrics* metrics, float interval, unsigned int step, float thickness, bool no_short) {
	CanvasPathBuilder^ hatch = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float x = metrics->hatch_x;
	float y = metrics->hatch_y;
	float width = metrics->hatch_width;
	float short_x = x + width * (((step % 2 == 1) || no_short) ? 1.0F : 0.618F);
	
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

static CanvasGeometry^ make_vrhatch(VHatchMarkMetrics& metrics, double weights[], size_t count, float thickness) {
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
	double range = (vmax - vmin) * std::pow(10.0, precision + 2);
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
	double delta = (vmax - vmin) / double(fxstep);
	float interval = (height - em) / float(fxstep);

	(*step) = fxstep;
	SET_BOX(skip, (fxstep % 2 == 0) ? 2 : 1);
	SET_BOX(diff, delta);

	return interval;
}

/*************************************************************************************************/
VHatchMarkMetrics WarGrey::SCADA::vhatchmark_metrics(double vmin, double vmax, float thickness
	, unsigned int precision, CanvasTextFormat^ font) {
	VHatchMarkMetrics metrics;
	Platform::String^ min_mark = make_rmark_string(vmin, precision);
	Platform::String^ max_mark = make_rmark_string(vmax, precision);
	Platform::String^ longer_mark = ((max_mark->Length() > min_mark->Length()) ? max_mark : min_mark);
	TextExtent te = get_text_extent(longer_mark, ((font == nullptr) ? default_mark_font : font));
	unsigned int longer_span = longer_mark->Length();
	
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

VHatchMarkMetrics WarGrey::SCADA::vhatchmark_metrics(Platform::String^ marks[], size_t count, float thickness, CanvasTextFormat^ font) {
	VHatchMarkMetrics metrics;
	Platform::String^ longest_mark = resolve_longest_mark(marks, count, &metrics.span);
	TextExtent te = get_text_extent(longest_mark, ((font == nullptr) ? default_mark_font : font));
	
	fill_consistent_vhatch_metrics(font, thickness, &metrics.hatch_width, &metrics.gap_space);

	metrics.ch = te.width / float(longest_mark->Length());
	metrics.em = te.height - te.tspace - te.bspace;

	metrics.mark_width = te.width;
	metrics.top_space = te.tspace;

	metrics.hatch_y = metrics.em * 0.5F;
	metrics.width = metrics.mark_width + metrics.gap_space + metrics.hatch_width;

	return metrics;
}

CanvasGeometry^ WarGrey::SCADA::vlhatch(float width, float height, unsigned int step, float thickness) {
	VHatchMarkMetrics metrics = make_input_vmetrics(width, height, thickness);
	
	return make_vlhatch(&metrics, metrics.hatch_height / float(step), step, thickness, true);
}

CanvasGeometry^ WarGrey::SCADA::vlhatch(float width, float height, double weights[], size_t count, float thickness) {
	VHatchMarkMetrics metrics = make_input_vmetrics(width, height, thickness);
	
	return make_vlhatch(metrics, weights, count, thickness);
}

CanvasGeometry^ WarGrey::SCADA::vrhatch(float width, float height, unsigned int step, float thickness) {
	VHatchMarkMetrics metrics = make_input_vmetrics(width, height, thickness);

	return make_vrhatch(&metrics, metrics.hatch_height / float(step), step, thickness, true);
}

CanvasGeometry^ WarGrey::SCADA::vrhatch(float width, float height, double weights[], size_t count, float thickness) {
	VHatchMarkMetrics metrics = make_input_vmetrics(width, height, thickness);

	return make_vrhatch(metrics, weights, count, thickness);
}

CanvasGeometry^ WarGrey::SCADA::vhatchmark(float height, double vmin, double vmax, unsigned int step
	, float thickness, VHatchMarkMetrics* maybe_metrics, unsigned int precision, bool no_short, CanvasTextFormat^ ft) {
	unsigned int skip;
	float mark_span_off;
	double diff;
	CanvasTextFormat^ font = ((ft == nullptr) ? default_mark_font : ft);
	VHatchMarkMetrics metrics = vhatchmark_metrics(vmin, vmax, thickness, precision, font);
	float interval = resolve_interval(&step, vmin, vmax, height, metrics.em, precision, &skip, &diff);
	float mark_x = metrics.hatch_width + metrics.gap_space;

	metrics.hatch_x = thickness * 0.5F;
	auto lhatchmark = make_vlhatch(&metrics, interval, step, thickness, no_short);
	metrics.hatch_x = mark_x + metrics.mark_width + metrics.gap_space;
	auto rhatchmark = make_vrhatch(&metrics, interval, step, thickness, no_short);

	auto hatchmark = geometry_union(lhatchmark, rhatchmark);
	for (unsigned int i = 0; i <= step; i += (no_short ? 1 : skip)) {
		Platform::String^ mark = make_lmark_string(vmax - diff * double(i), precision, metrics.span, &mark_span_off);
		float tx = mark_x + mark_span_off * metrics.ch * 0.5F;
		float ty = interval * float(i) - metrics.top_space;

		hatchmark = geometry_union(hatchmark, paragraph(make_text_layout(mark, font)), tx, ty);
	}

	SET_BOX(maybe_metrics, metrics);

	return hatchmark;
}

CanvasGeometry^ WarGrey::SCADA::vhatchmark(float height, Platform::String^ marks[], double weights[], size_t count
	, float thickness, VHatchMarkMetrics* maybe_metrics, CanvasTextFormat^ ft) {
	CanvasTextFormat^ font = ((ft == nullptr) ? default_mark_font : ft);
	VHatchMarkMetrics metrics = vhatchmark_metrics(marks, count, thickness, font);
	float mark_x = metrics.hatch_width + metrics.gap_space;

	metrics.hatch_height = height - metrics.em;
	metrics.hatch_x = thickness * 0.5F;
	auto rhatchmark = make_vrhatch(metrics, weights, count, thickness);
	metrics.hatch_x = metrics.width - metrics.hatch_width;
	auto lhatchmark = make_vlhatch(metrics, weights, count, thickness);

	auto hatchmark = geometry_union(lhatchmark, rhatchmark, mark_x + metrics.mark_width + metrics.gap_space);
	for (size_t i = 0; i < count; i++) {
		unsigned int mark_span_off = metrics.span - mark_span(marks[i]);
		float tx = mark_x + mark_span_off * metrics.ch * 0.5F;
		float ty = discrete_weight_position(metrics.hatch_height, weights[i]) - metrics.top_space;

		hatchmark = geometry_union(hatchmark, paragraph(make_text_layout(marks[i], font)), tx, ty);
	}

	SET_BOX(maybe_metrics, metrics);

	return hatchmark;
}

CanvasGeometry^ WarGrey::SCADA::vlhatchmark(float height, double vmin, double vmax, unsigned int step, float thickness
	, VHatchMarkMetrics* maybe_metrics, unsigned int precision, bool no_short, CanvasTextFormat^ ft) {
	unsigned int skip;
	float mark_span_off;
	double diff;
	CanvasTextFormat^ font = ((ft == nullptr) ? default_mark_font : ft);
	VHatchMarkMetrics metrics = vhatchmark_metrics(vmin, vmax, thickness, precision, font);
	float interval = resolve_interval(&step, vmin, vmax, height, metrics.em, precision, &skip, &diff);
	
	metrics.hatch_x = metrics.width - metrics.hatch_width;

	auto hatchmark = make_vlhatch(&metrics, interval, step, thickness, no_short);
	for (unsigned int i = 0; i <= step; i += (no_short ? 1 : skip)) {
		Platform::String^ mark = make_lmark_string(vmax - diff * double(i), precision, metrics.span, &mark_span_off);
		float tx = mark_span_off * metrics.ch;
		float ty = interval * float(i) - metrics.top_space;

		hatchmark = geometry_union(hatchmark, paragraph(make_text_layout(mark, font)), tx, ty);
	}

	SET_BOX(maybe_metrics, metrics);

	return hatchmark;
}

CanvasGeometry^ WarGrey::SCADA::vlhatchmark(float height, Platform::String^ marks[], double weights[], size_t count
	, float thickness, VHatchMarkMetrics* maybe_metrics, CanvasTextFormat^ ft) {
	CanvasTextFormat^ font = ((ft == nullptr) ? default_mark_font : ft);
	VHatchMarkMetrics metrics = vhatchmark_metrics(marks, count, thickness, font);
	
	metrics.hatch_x = metrics.width - metrics.hatch_width;
	metrics.hatch_height = height - metrics.em;

	auto hatchmark = make_vlhatch(metrics, weights, count, thickness);
	for (size_t i = 0; i < count; i ++) {
		unsigned int mark_span_off = metrics.span - mark_span(marks[i]);
		float tx = mark_span_off * metrics.ch;
		float ty = discrete_weight_position(metrics.hatch_height, weights[i]) - metrics.top_space;

		hatchmark = geometry_union(hatchmark, paragraph(make_text_layout(marks[i], font)), tx, ty);
	}

	SET_BOX(maybe_metrics, metrics);

	return hatchmark;
}

CanvasGeometry^ WarGrey::SCADA::vrhatchmark(float height, double vmin, double vmax, unsigned int step, float thickness
	, VHatchMarkMetrics* maybe_metrics, unsigned int precision, bool no_short, CanvasTextFormat^ ft) {
	unsigned int skip;
	double diff;
	CanvasTextFormat^ font = ((ft == nullptr) ? default_mark_font : ft);
	VHatchMarkMetrics metrics = vhatchmark_metrics(vmin, vmax, thickness, precision, font);
	float mark_tx = metrics.hatch_width + metrics.gap_space;
	float interval = resolve_interval(&step, vmin, vmax, height, metrics.em, precision, &skip, &diff);
	
	metrics.hatch_x = thickness * 0.0F;

	auto hatchmark = make_vrhatch(&metrics, interval, step, thickness, no_short);
	for (unsigned int i = 0; i <= step; i += (no_short ? 1 : skip)) {
		Platform::String^ mark = make_rmark_string(vmax - diff * double(i), precision);
		float ty = interval * float(i) - metrics.top_space;

		hatchmark = geometry_union(hatchmark, paragraph(make_text_layout(mark, font)), mark_tx, ty);
	}

	SET_BOX(maybe_metrics, metrics);

	return hatchmark;
}

CanvasGeometry^ WarGrey::SCADA::vrhatchmark(float height, Platform::String^ marks[], double weights[], size_t count
	, float thickness, VHatchMarkMetrics* maybe_metrics, CanvasTextFormat^ ft) {
	CanvasTextFormat^ font = ((ft == nullptr) ? default_mark_font : ft);
	VHatchMarkMetrics metrics = vhatchmark_metrics(marks, count, thickness, font);
	float mark_tx = metrics.hatch_width + metrics.gap_space;
	
	metrics.hatch_x = thickness * 0.0F;
	metrics.hatch_height = height - metrics.em;

	auto hatchmark = make_vrhatch(metrics, weights, count, thickness);
	for (size_t i = 0; i < count; i++) {
		float ty = discrete_weight_position(metrics.hatch_height, weights[i]) - metrics.top_space;

		hatchmark = geometry_union(hatchmark, paragraph(make_text_layout(marks[i], font)), mark_tx, ty);
	}

	SET_BOX(maybe_metrics, metrics);

	return hatchmark;
}
