#include <algorithm>

#include "hatch.hpp"

#include "box.hpp"
#include "text.hpp"
#include "shape.hpp"
#include "paint.hpp"
#include "geometry.hpp"
#include "transformation.hpp"

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

static float resolve_vmetrics(float vmin, float vmax, float thickness, CanvasTextFormat^ maybe_font, float* hatch_width = nullptr
	, unsigned int* span = nullptr, float* ch = nullptr, float* em = nullptr, float* tspace = nullptr) {
	Platform::String^ min_mark = make_rmark_string(vmin);
	Platform::String^ max_mark = make_rmark_string(vmax);
	Platform::String^ longer_mark = ((max_mark->Length() > min_mark->Length()) ? max_mark : min_mark);
	unsigned int longer_span = longer_mark->Length();
	TextExtent te = get_text_extent(longer_mark, ((maybe_font == nullptr) ? default_mark_font : maybe_font));
	float char_width = te.width / float(longer_span);
	float hwidth = char_width * hatch_long_ratio + thickness;

	SET_BOX(hatch_width, hwidth);
	SET_BOX(span, longer_span);
	SET_BOX(ch, char_width);
	SET_BOX(em, te.height - te.tspace - te.bspace);
	SET_BOX(tspace, te.tspace);

	return te.width + mark_space_ratio * char_width + hwidth;
}

static CanvasGeometry^ make_vlhatch(float width, float interval, unsigned int step, float thickness, float x = 0.0F, float y = 0.0F) {
	CanvasPathBuilder^ hatch = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float short_x = x + width * ((step % 2 == 0) ? 0.382F : 0.0F);
	float height = interval * step;

	hatch->BeginFigure(x + width, y);
	hatch->AddLine(x + width, y + height);
	for (unsigned int i = 0; i <= step; i++) {
		float ythis = interval * float(i) + y;

		hatch->EndFigure(CanvasFigureLoop::Open);
		hatch->BeginFigure((i % 2 == 0) ? x : short_x, ythis);
		hatch->AddLine(x + width, ythis);
	}
	hatch->EndFigure(CanvasFigureLoop::Open);

	return geometry_stroke(CanvasGeometry::CreatePath(hatch), thickness, make_roundcap_stroke_style(true));
}

static CanvasGeometry^ make_vrhatch(float width, float interval, unsigned int step, float thickness, float x = 0.0F, float y = 0.0F) {
	CanvasPathBuilder^ hatch = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float short_x = x + width * ((step % 2 == 0) ? 0.618F : 1.0F);
	float height = interval * step;

	hatch->BeginFigure(x, y);
	hatch->AddLine(x, y + height);
	for (unsigned int i = 0; i <= step; i++) {
		float ythis = interval * float(i) + y;

		hatch->EndFigure(CanvasFigureLoop::Open);
		hatch->BeginFigure((i % 2 == 0) ? x + width : short_x, ythis);
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
float WarGrey::SCADA::vhatchmark_width(float vmin, float vmax, float thickness, CanvasTextFormat^ font
	, float* hatch_width, float* ch, float* em) {
	return resolve_vmetrics(vmin, vmax, thickness, font, hatch_width, nullptr, ch, em);
}

CanvasGeometry^ WarGrey::SCADA::vlhatch(float width, float height, unsigned int step, float thickness) {
	float interval = (height - thickness) / float(step);
	float offset = thickness * 0.5F;
	
	return make_vlhatch(width - thickness, interval, step, thickness, offset, offset);
}

CanvasGeometry^ WarGrey::SCADA::vrhatch(float width, float height, unsigned int step, float thickness) {
	float interval = (height - thickness) / float(step);
	float offset = thickness * 0.5F;

	return make_vrhatch(width - thickness, interval, step, thickness, offset, offset);
}

CanvasGeometry^ WarGrey::SCADA::vlhatchmark(float height, float vmin, float vmax, unsigned int step, float thickness, Rect* box, CanvasTextFormat^ ft) {
	float hatch_width, ch, em, tspace, diff, mark_span_off;
	unsigned int span, skip;
	CanvasTextFormat^ font = ((ft == nullptr) ? default_mark_font : ft);
	float hatchmark_width = resolve_vmetrics(vmin, vmax, thickness, font, &hatch_width, &span, &ch, &em, &tspace);
	float hatch_x = hatchmark_width - hatch_width;
	float hatch_y = em * 0.5F;
	float interval = resolve_interval(step, vmin, vmax, height, em, &skip, &diff);

	auto marks = make_vlhatch(hatch_width, interval, step, thickness, hatch_x, hatch_y);
	for (unsigned int i = 0; i <= step; i += skip) {
		Platform::String^ mark = make_lmark_string(vmax - diff * float(i), span, &mark_span_off);
		auto translation = make_translation_matrix(mark_span_off * ch, interval * float(i) - tspace);

		marks = geometry_union(marks, paragraph(make_text_layout(mark, font)), translation);
	}

	if (box != nullptr) {
		box->Y = hatch_y;
		box->Width = hatchmark_width;
		box->Height = interval * float(step);
	}

	return marks;
}

CanvasGeometry^ WarGrey::SCADA::vrhatchmark(float height, float vmin, float vmax, unsigned int step, float thickness, Rect* box, CanvasTextFormat^ ft) {
	float hatch_width, ch, em, tspace, diff;
	unsigned int span, skip;
	CanvasTextFormat^ font = ((ft == nullptr) ? default_mark_font : ft);
	float hatchmark_width = resolve_vmetrics(vmin, vmax, thickness, font, &hatch_width, &span, &ch, &em, &tspace);
	float hatch_y = em * 0.5F;
	float mark_xoff = hatch_width + mark_space_ratio * ch;
	float interval = resolve_interval(step, vmin, vmax, height, em, &skip, &diff);
	
	auto marks = make_vrhatch(hatch_width, interval, step, thickness, 0.0F, hatch_y);
	for (unsigned int i = 0; i <= step; i += skip) {
		Platform::String^ mark = make_rmark_string(vmax - diff * float(i));
		auto translation = make_translation_matrix(mark_xoff, interval * float(i) - tspace);

		marks = geometry_union(marks, paragraph(make_text_layout(mark, font)), translation);
	}

	if (box != nullptr) {
		box->Y = hatch_y;
		box->Width = hatchmark_width;
		box->Height = interval * float(step);
	}

	return marks;
}
