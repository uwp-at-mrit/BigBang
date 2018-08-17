#include <algorithm>

#include "hatch.hpp"

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

static CanvasTextFormat^ default_mark_font = make_bold_text_format(9.0F);
static const float hatch_long_ratio = 0.618F;
static const float mark_space_ratio = 0.618F;

inline static Platform::String^ make_rmark_string(float mark) {
	return make_wstring(L"%.1f", mark);
}

inline static Platform::String^ make_lmark_string(float mark, unsigned int span, float* span_off) {
	Platform::String^ s = make_rmark_string(mark);
	(*span_off) = float(span - s->Length());

	return s;
}

inline unsigned int mark_span(Platform::String^ mark) {
	// TODO: resolve the mark language
	return mark->Length();
}

inline float discrete_weight_position(float height, float weight) {
	return height * std::fmaxf(std::fminf(weight, 1.0F), 0.0F);
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

static CanvasGeometry^ make_vlhatch(VHatchMarkMetrics* metrics, float interval, unsigned int step, float thickness) {
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

static CanvasGeometry^ make_vlhatch(VHatchMarkMetrics& metrics, float weights[], size_t count, float thickness) {
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

static CanvasGeometry^ make_vrhatch(VHatchMarkMetrics* metrics, float interval, unsigned int step, float thickness) {
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

static CanvasGeometry^ make_vrhatch(VHatchMarkMetrics& metrics, float weights[], size_t count, float thickness) {
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

static float resolve_interval(unsigned int step, float vmin, float vmax, float height, float em, unsigned int* skip, float* diff) {
	/** TODO
	 * Improper `range` and `step` may cause `D2DERR_BAD_NUMBER(HRESULT: 0x88990011)`.
	 *
	 * Meanwhile there is no detail about how to choose a suitable (range, step) pair,
	 * But it is suggested that the `range` should be divisible by the `step`.
	 */
	float range = vmax - vmin;
	float hatch_height = height - em;
	float interval = hatch_height / float(step);
	float delta = range / float(step);

	SET_BOX(skip, (step % 2 == 0) ? 2 : 1);
	SET_BOX(diff, delta);

	return interval;
}

/*************************************************************************************************/
VHatchMarkMetrics WarGrey::SCADA::vhatchmark_metrics(float vmin, float vmax, float thickness, CanvasTextFormat^ font) {
	VHatchMarkMetrics metrics;
	Platform::String^ min_mark = make_rmark_string(vmin);
	Platform::String^ max_mark = make_rmark_string(vmax);
	Platform::String^ longer_mark = ((max_mark->Length() > min_mark->Length()) ? max_mark : min_mark);
	unsigned int longer_span = longer_mark->Length();
	TextExtent te = get_text_extent(longer_mark, ((font == nullptr) ? default_mark_font : font));
	float char_width = te.width / float(longer_span);
	float gapsize = char_width * mark_space_ratio;
	float hwidth = char_width * hatch_long_ratio + thickness;

	metrics.mark_width = te.width;
	metrics.span = longer_span;
	metrics.em = te.height - te.tspace - te.bspace;
	metrics.ch = char_width;
	metrics.tspace = te.tspace;

	metrics.hatch_y = metrics.em * 0.5F;
	metrics.hatch_width = hwidth;
	metrics.width = te.width + gapsize + hwidth;

	return metrics;
}

VHatchMarkMetrics WarGrey::SCADA::vhatchmark_metrics(Platform::String^ marks[], size_t count, float thickness, CanvasTextFormat^ font) {
	VHatchMarkMetrics metrics;
	Platform::String^ longest_mark = resolve_longest_mark(marks, count, &metrics.span);
	TextExtent te = get_text_extent(longest_mark, ((font == nullptr) ? default_mark_font : font));
	float char_width = te.width / float(metrics.span);
	float gapsize = mark_space_ratio * char_width;
	float hwidth = char_width * hatch_long_ratio + thickness;

	metrics.mark_width = te.width;
	metrics.em = te.height - te.tspace - te.bspace;
	metrics.ch = char_width;
	metrics.tspace = te.tspace;

	metrics.hatch_y = metrics.em * 0.5F;
	metrics.hatch_width = hwidth;
	metrics.width = te.width + gapsize + hwidth;

	return metrics;
}

CanvasGeometry^ WarGrey::SCADA::vlhatch(float width, float height, unsigned int step, float thickness) {
	VHatchMarkMetrics metrics = make_input_vmetrics(width, height, thickness);
	
	return make_vlhatch(&metrics, metrics.hatch_height / float(step), step, thickness);
}

CanvasGeometry^ WarGrey::SCADA::vlhatch(float width, float height, float weights[], size_t count, float thickness) {
	VHatchMarkMetrics metrics = make_input_vmetrics(width, height, thickness);
	
	return make_vlhatch(metrics, weights, count, thickness);
}

CanvasGeometry^ WarGrey::SCADA::vrhatch(float width, float height, unsigned int step, float thickness) {
	VHatchMarkMetrics metrics = make_input_vmetrics(width, height, thickness);

	return make_vrhatch(&metrics, metrics.hatch_height / float(step), step, thickness);
}

CanvasGeometry^ WarGrey::SCADA::vrhatch(float width, float height, float weights[], size_t count, float thickness) {
	VHatchMarkMetrics metrics = make_input_vmetrics(width, height, thickness);

	return make_vrhatch(metrics, weights, count, thickness);
}

CanvasGeometry^ WarGrey::SCADA::vlhatchmark(float height, float vmin, float vmax, unsigned int step, float thickness
	, VHatchMarkMetrics* maybe_metrics, CanvasTextFormat^ ft) {
	unsigned int skip;
	float diff, mark_span_off;
	CanvasTextFormat^ font = ((ft == nullptr) ? default_mark_font : ft);
	VHatchMarkMetrics metrics = vhatchmark_metrics(vmin, vmax, thickness, font);
	float interval = resolve_interval(step, vmin, vmax, height, metrics.em, &skip, &diff);
	
	metrics.hatch_x = metrics.width - metrics.hatch_width;

	auto hatchmark = make_vlhatch(&metrics, interval, step, thickness);
	for (unsigned int i = 0; i <= step; i += skip) {
		Platform::String^ mark = make_lmark_string(vmax - diff * float(i), metrics.span, &mark_span_off);
		float tx = mark_span_off * metrics.ch;
		float ty = interval * float(i) - metrics.tspace;

		hatchmark = geometry_union(hatchmark, paragraph(make_text_layout(mark, font)), tx, ty);
	}

	SET_BOX(maybe_metrics, metrics);

	return hatchmark;
}

CanvasGeometry^ WarGrey::SCADA::vlhatchmark(float height, Platform::String^ marks[], float weights[], size_t count
	, float thickness, VHatchMarkMetrics* maybe_metrics, CanvasTextFormat^ ft) {
	CanvasTextFormat^ font = ((ft == nullptr) ? default_mark_font : ft);
	VHatchMarkMetrics metrics = vhatchmark_metrics(marks, count, thickness, font);
	
	metrics.hatch_x = metrics.width - metrics.hatch_width;
	metrics.hatch_height = height - metrics.em;

	auto hatchmark = make_vlhatch(metrics, weights, count, thickness);
	for (size_t i = 0; i < count; i ++) {
		unsigned int mark_span_off = metrics.span - mark_span(marks[i]);
		float tx = mark_span_off * metrics.ch;
		float ty = discrete_weight_position(metrics.hatch_height, weights[i]) - metrics.tspace;

		hatchmark = geometry_union(hatchmark, paragraph(make_text_layout(marks[i], font)), tx, ty);
	}

	SET_BOX(maybe_metrics, metrics);

	return hatchmark;
}

CanvasGeometry^ WarGrey::SCADA::vrhatchmark(float height, float vmin, float vmax, unsigned int step, float thickness
	, VHatchMarkMetrics* maybe_metrics, CanvasTextFormat^ ft) {
	unsigned int skip;
	float diff;
	CanvasTextFormat^ font = ((ft == nullptr) ? default_mark_font : ft);
	VHatchMarkMetrics metrics = vhatchmark_metrics(vmin, vmax, thickness, font);
	float mark_tx = metrics.hatch_width + mark_space_ratio * metrics.ch;
	float interval = resolve_interval(step, vmin, vmax, height, metrics.em, &skip, &diff);
	
	auto hatchmark = make_vrhatch(&metrics, interval, step, thickness);
	for (unsigned int i = 0; i <= step; i += skip) {
		Platform::String^ mark = make_rmark_string(vmax - diff * float(i));
		float ty = interval * float(i) - metrics.tspace;

		hatchmark = geometry_union(hatchmark, paragraph(make_text_layout(mark, font)), mark_tx, ty);
	}

	SET_BOX(maybe_metrics, metrics);

	return hatchmark;
}

CanvasGeometry^ WarGrey::SCADA::vrhatchmark(float height, Platform::String^ marks[], float weights[], size_t count
	, float thickness, VHatchMarkMetrics* maybe_metrics, CanvasTextFormat^ ft) {
	CanvasTextFormat^ font = ((ft == nullptr) ? default_mark_font : ft);
	VHatchMarkMetrics metrics = vhatchmark_metrics(marks, count, thickness, font);
	float mark_tx = metrics.hatch_width + mark_space_ratio * metrics.ch;
	
	metrics.hatch_height = height - metrics.em;

	auto hatchmark = make_vrhatch(metrics, weights, count, thickness);
	for (size_t i = 0; i < count; i++) {
		float ty = discrete_weight_position(metrics.hatch_height, weights[i]) - metrics.tspace;

		hatchmark = geometry_union(hatchmark, paragraph(make_text_layout(marks[i], font)), mark_tx, ty);
	}

	SET_BOX(maybe_metrics, metrics);

	return hatchmark;
}
