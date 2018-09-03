#include "graphlet/dashboard/lineslet.hpp"

#include "string.hpp"
#include "time.hpp"

#include "text.hpp"
#include "shape.hpp"
#include "geometry.hpp"

using namespace WarGrey::SCADA;

using namespace Windows::Foundation;

using namespace Microsoft::Graphics::Canvas;
using namespace Microsoft::Graphics::Canvas::Text;
using namespace Microsoft::Graphics::Canvas::Brushes;
using namespace Microsoft::Graphics::Canvas::Geometry;

static CanvasSolidColorBrush^ lines_default_border_color = Colours::make(0xBBBBBB);
static CanvasTextFormat^ lines_default_font = make_bold_text_format(9.0F);

/*************************************************************************************************/
TimeSeries WarGrey::SCADA::make_today_series(unsigned int step) {
	TimeSeries ts;

	ts.start = today_first_100ns();
	ts.span = 24LL * 3600LL * 1000LL * 1000LL * 10LL;
	ts.step = step;

	return ts;
}

/*************************************************************************************************/
Lineslet::Lineslet(double range, float width, float height, unsigned int step, unsigned int precision)
	: Lineslet(0.0, range, width, height, step, precision) {}

Lineslet::Lineslet(double range, TimeSeries& ts, float width, float height, unsigned int step, unsigned int precision)
	: Lineslet(0.0, range, ts, width, height, step, precision) {}

Lineslet::Lineslet(double vmin, double vmax, float width, float height, unsigned int step, unsigned int precision)
	: Lineslet(vmin, vmax, make_today_series(), width, height, step, precision) {}

Lineslet::Lineslet(double vmin, double vmax, TimeSeries& ts, float width, float height, unsigned int step, unsigned int precision)
	: width(std::fabsf(width)), height(height), step((step == 0) ? 5U : step), precision(precision)
	, vmin(vmin), vmax(vmax), series(ts) {

	if (this->height < 0.0F) {
		this->height *= (-this->width);
	} else if (this->height == 0.0F) {
		this->height = this->width * 0.2718F;
	}

	if (this->vmin > this->vmax) {
		this->vmin = vmax; 
		this->vmax = vmin;
	}
}

void Lineslet::construct() {
}

void Lineslet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->width, h, this->height);
}

void Lineslet::prepare_style(LinesStatus status, LinesStyle& style) {
	CAS_SLOT(style.font, lines_default_font);
	CAS_SLOT(style.border_color, lines_default_border_color);
	CAS_SLOT(style.haxes_color, Colours::RoyalBlue);
	CAS_SLOT(style.haxes_style, make_dash_stroke(CanvasDashStyle::DashDot));
	CAS_SLOT(style.vaxes_color, Colours::Firebrick);
	CAS_SLOT(style.vaxes_style, make_dash_stroke(CanvasDashStyle::DashDot));

	FLCAS_SLOT(style.border_thickness, 3.0F);
	FLCAS_SLOT(style.lines_thickness, 1.0F);
	FLCAS_SLOT(style.haxes_thickness, 0.5F);
	FLCAS_SLOT(style.vaxes_thickness, 1.0F);
}

void Lineslet::on_status_changed(LinesStatus status) {
	this->construct_vertical_axes();
	this->construct_horizontal_axes();
}

void Lineslet::construct_vertical_axes() {
	LinesStyle s = this->get_style();
	CanvasGeometry^ vaxes = blank();
	CanvasPathBuilder^ axes = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float interval = this->height / float(this->step + 1);
	double delta = (this->vmax - this->vmin) / double(this->step + 1);
	float y = this->height - s.haxes_thickness * 0.5F;
	float mark_height;

	for (unsigned int i = 1; i <= step; i++) {
		float ythis = y - interval * float(i);
		Platform::String^ mark = flstring(this->vmin + delta * double(i), this->precision);
		CanvasGeometry^ gmark = paragraph(mark, s.font, nullptr, &mark_height);

		axes->BeginFigure(0.0F, ythis);
		axes->AddLine(this->width, ythis);
		axes->EndFigure(CanvasFigureLoop::Open);

		vaxes = geometry_union(vaxes, gmark, s.border_thickness + mark_height * 0.618F, ythis - mark_height);
	}

	vaxes = geometry_union(vaxes, geometry_stroke(CanvasGeometry::CreatePath(axes), s.vaxes_thickness, s.vaxes_style));
	this->vaxes = geometry_freeze(vaxes);
}

void Lineslet::construct_horizontal_axes() {
	LinesStyle s = this->get_style();
	CanvasPathBuilder^ axes = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	CanvasGeometry^ hmarks = blank();
	float interval = this->width / float(this->series.step + 1);
	long long delta = this->series.span / (this->series.step + 1);
	float x = s.haxes_thickness * 0.5F;
	float y = this->height - s.border_thickness;
	float mark_width, mark_height;

	for (unsigned int i = 0; i <= this->series.step + 1; i++) {
		float xthis = x + interval * float(i);
		Platform::String^ mark = make_daytimestamp(this->series.start + delta * i, false);
		CanvasGeometry^ gmark = paragraph(mark, s.font, &mark_width, &mark_height);

		axes->BeginFigure(xthis, 0.0F);
		axes->AddLine(xthis, this->height);
		axes->EndFigure(CanvasFigureLoop::Open);

		hmarks = geometry_union(hmarks, gmark, xthis - mark_width * 0.5F, y - mark_height);
	}

	this->hmarks = geometry_freeze(hmarks);
	this->haxes = geometry_stroke(CanvasGeometry::CreatePath(axes), s.haxes_thickness, s.haxes_style);
}

void Lineslet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	LinesStyle s = this->get_style();
	float border_off = s.border_thickness * 0.5F;
	
	ds->FillRectangle(x, y, this->width, this->height, Colours::Background);

	ds->DrawCachedGeometry(this->vaxes, x, y, s.vaxes_color);

	if (this->get_status() == LinesStatus::Realtime) {
		Rect haxes_box = this->haxes->ComputeBounds();
		long long now = today_current_100ns();
		float percentage = std::fminf(float(now - this->series.start) / float(this->series.span), 1.0F);
		float xmin = this->width - haxes_box.Width;
		float haxes_x = std::fmaxf(xmin, std::fminf(this->width * 0.5F - haxes_box.Width * percentage, 0.0F));
		
		ds->FillGeometry(this->haxes, x + haxes_x, y, s.haxes_color);
		ds->DrawCachedGeometry(this->hmarks, x + haxes_x, y, s.haxes_color);
	}
	
	ds->DrawRectangle(x + border_off, y + border_off,
		this->width - s.border_thickness, this->height - s.border_thickness,
		s.border_color, s.border_thickness);
}
