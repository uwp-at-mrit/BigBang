#include <algorithm>

#include "hatch.hpp"

#include "box.hpp"
#include "text.hpp"
#include "shape.hpp"
#include "paint.hpp"
#include "geometry.hpp"
#include "transformation.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Geometry;

static CanvasTextFormat^ default_mark_font = make_bold_text_format(9.0F);
static const float hatch_long_ratio = 1.0F;
static const float mark_space_ratio = 1.0F;

inline static Platform::String^ make_rmark_string(float mark) {
	return mark.ToString();
}

inline static Platform::String^ make_lmark_string(float mark, unsigned int width, float* mark_xoff) {
	Platform::String^ s = make_rmark_string(mark);
	(*mark_xoff) = float(width - s->Length());

	return s;
}

static CanvasGeometry^ make_vlhatch(float width, float interval, unsigned int step, float thickness, float x = 0.0F, float y = 0.0F) {
	CanvasPathBuilder^ hatch = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float short_x = x + width * (1.0F - 0.618F);
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
	float short_x = x + width * 0.618F;
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

/*************************************************************************************************/
CanvasGeometry^ WarGrey::SCADA::vlhatch(float height, float vmin, float vmax, unsigned int step, float thickness, Rect* box, CanvasTextFormat^ ft) {
	Platform::String^ min_mark = vmin.ToString(); 
	Platform::String^ max_mark = vmax.ToString();
	unsigned int span = std::max(max_mark->Length(), min_mark->Length());
	CanvasTextFormat^ font = ((ft == nullptr) ? default_mark_font : ft);
	TextExtent te = get_text_extent(max_mark, font);
	float ch = te.width / span;
	float em = te.height - te.tspace - te.bspace;
	float hatch_width = ch * hatch_long_ratio + thickness;
	float hatch_x = float(span) * ch + mark_space_ratio * ch;
	float hatch_y = em * 0.5F;
	float interval = (height - em) / float(step);
	float delta = (vmax - vmin) / float(step);
	float mark_xunit_off;

	auto marks = make_vlhatch(hatch_width, interval, step, thickness, hatch_x, hatch_y);
	for (unsigned int i = 0; i <= step; i += 2) {
		Platform::String^ mark = make_lmark_string(vmax - delta * float(i), span, &mark_xunit_off);

		auto translation = make_translation_matrix(mark_xunit_off * ch, interval * float(i) - te.tspace);
		marks = geometry_union(marks, paragraph(make_text_layout(mark, font)), translation);
	}

	if (box != nullptr) {
		box->Y = hatch_y;
		box->Width = hatch_x + hatch_width;
		box->Height = interval * float(step);
	}

	return marks;
}

CanvasGeometry^ WarGrey::SCADA::vrhatch(float height, float vmin, float vmax, unsigned int step, float thickness, Rect* box, CanvasTextFormat^ ft) {
	Platform::String^ min_mark = vmin.ToString();
	Platform::String^ max_mark = vmax.ToString();
	unsigned int span = std::max(max_mark->Length(), min_mark->Length());
	CanvasTextFormat^ font = ((ft == nullptr) ? default_mark_font : ft);
	TextExtent te = get_text_extent(max_mark, font);
	float ch = te.width / span;
	float em = te.height - te.tspace - te.bspace;
	float hatch_width = ch * hatch_long_ratio + thickness;
	float hatch_y = em * 0.5F;
	float interval = (height - em) / float(step);
	float delta = (vmax - vmin) / float(step);
	float mark_xoff = hatch_width + mark_space_ratio * ch;

	auto marks = make_vrhatch(hatch_width, interval, step, thickness, 0.0F, hatch_y);
	for (unsigned int i = 0; i <= step; i += 2) {
		Platform::String^ mark = make_rmark_string(vmax - delta * float(i));

		auto translation = make_translation_matrix(mark_xoff, interval * float(i) - te.tspace);
		marks = geometry_union(marks, paragraph(make_text_layout(mark, font)), translation);
	}

	if (box != nullptr) {
		float mark_span = float(span) * ch + mark_space_ratio * ch;

		box->Y = hatch_y;
		box->Width = mark_span + hatch_width;
		box->Height = interval * float(step);
	}

	return marks;
}
