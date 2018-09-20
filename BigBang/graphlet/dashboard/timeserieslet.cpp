#include "graphlet/dashboard/timeserieslet.hpp"

#include "time.hpp"
#include "string.hpp"

#include "colorspace.hpp"

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
static CanvasTextFormat^ lines_default_legend_font = make_bold_text_format(12.0F);

/*************************************************************************************************/
TimeSeries WarGrey::SCADA::make_this_minute_series(unsigned int step) {
	TimeSeries ts;

	ts.start = current_floor_seconds(minute_span_s);
	ts.span = minute_span_s;
	ts.step = step;

	return ts;
}

TimeSeries WarGrey::SCADA::make_this_hour_series(unsigned int step) {
	TimeSeries ts;

	ts.start = current_floor_seconds(hour_span_s);
	ts.span = hour_span_s;
	ts.step = step;

	return ts;
}

TimeSeries WarGrey::SCADA::make_today_series(unsigned int step) {
	TimeSeries ts;

	ts.start = current_floor_seconds(day_span_s);
	ts.span = day_span_s;
	ts.step = step;

	return ts;
}

ICanvasBrush^ WarGrey::SCADA::lookup_default_light_color(unsigned int idx) {
	return make_solid_brush(lookup_light_color(idx + 1));
}

ICanvasBrush^ WarGrey::SCADA::lookup_default_dark_color(unsigned int idx) {
	return make_solid_brush(lookup_dark_color(idx + 1));
}

/*************************************************************************************************/
ITimeSerieslet::ITimeSerieslet(double vmin, double vmax, TimeSeries& ts, unsigned int n
	,float width, float height, unsigned int step, unsigned int precision)
	: width(std::fabsf(width)), height(height), step((step == 0) ? 5U : step), precision(precision)
	, vmin(vmin), vmax(vmax), series(ts) {

	if (this->height == 0.0F) {
		this->height = this->width * 0.2718F;
	}

	if (this->vmin > this->vmax) {
		this->vmin = vmax; 
		this->vmax = vmin;
	}

	this->colors = ref new Platform::Array<ICanvasBrush^>(n);
	this->lines = ref new Platform::Array<CanvasGeometry^>(n);
	this->legends = ref new Platform::Array<CanvasTextLayout^>(n);
	this->names = ref new Platform::Array<Platform::String^>(n);
	this->values = ref new Platform::Array<double>(n);
	this->xs = ref new Platform::Array<double>(n);
	this->ys = ref new Platform::Array<double>(n);
}

void ITimeSerieslet::update(long long count, long long interval, long long uptime) {
	long long now = current_seconds();
	long long next_start = this->series.start + this->series.span;

	if (now > next_start) {
		this->update_time_series(next_start);
	}
}

void ITimeSerieslet::construct_line(unsigned int idx, Platform::String^ name) {
	TimeSeriesStyle s = this->get_style();

	this->names[idx] = name;
	this->lines[idx] = blank();
	this->values[idx] = std::nan("waiting for the first datum");

	this->update_legend(idx, s);
}

void ITimeSerieslet::fill_extent(float x, float y, float* w, float* h) {
	SET_VALUES(w, this->width, h, this->height);
}

void ITimeSerieslet::prepare_style(TimeSeriesStatus status, TimeSeriesStyle& style) {
	CAS_SLOT(style.lookup_color, lookup_default_light_color);

	CAS_SLOT(style.font, lines_default_font);
	CAS_SLOT(style.legend_font, lines_default_legend_font);
	CAS_SLOT(style.border_color, lines_default_border_color);
	CAS_SLOT(style.haxes_color, Colours::RoyalBlue);
	CAS_SLOT(style.haxes_style, make_dash_stroke(CanvasDashStyle::DashDot));
	CAS_SLOT(style.vaxes_color, Colours::Firebrick);
	CAS_SLOT(style.vaxes_style, make_dash_stroke(CanvasDashStyle::DashDot));

	FLCAS_SLOT(style.border_thickness, 3.0F);
	FLCAS_SLOT(style.lines_thickness, 1.0F);
	FLCAS_SLOT(style.haxes_thickness, 0.5F);
	FLCAS_SLOT(style.vaxes_thickness, 0.5F);

	if ((style.legend_fx < 0.0F) || (style.legend_fx > 1.0F)) {
		style.legend_fx = 0.8F;
	}
}

void ITimeSerieslet::on_status_changed(TimeSeriesStatus status) {
	TimeSeriesStyle s = this->get_style();

	this->update_vertical_axes(s);
	this->update_horizontal_axes(s);

	for (unsigned int idx = 0; idx < this->legends->Length; idx++) {
		this->update_legend(idx, s);
		this->colors[idx] = s.lookup_color(idx);
	}
}

void ITimeSerieslet::update_time_series(long long next_start) {
	this->series.start = next_start;
	this->update_horizontal_axes(this->get_style());

	for (unsigned int idx = 0; idx < this->lines->Length; idx++) {
		this->lines[idx] = blank();
		this->values[idx] = std::nan("datum is reset");
	}
}

void ITimeSerieslet::update_vertical_axes(TimeSeriesStyle& s) {
	CanvasGeometry^ vaxes = blank();
	CanvasPathBuilder^ axes = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	float interval = this->height / float(this->step + 1);
	double delta = (this->vmax - this->vmin) / double(this->step + 1);
	float y = this->height - s.haxes_thickness * 0.5F;
	TextExtent mark_te;

	for (unsigned int i = 1; i <= step; i++) {
		float ythis = y - interval * float(i);
		Platform::String^ mark = flstring(this->vmin + delta * double(i), this->precision);
		CanvasGeometry^ gmark = paragraph(mark, s.font, &mark_te);

		axes->BeginFigure(0.0F, ythis);
		axes->AddLine(this->width, ythis);
		axes->EndFigure(CanvasFigureLoop::Open);

		vaxes = geometry_union(vaxes, gmark, s.border_thickness + mark_te.height * 0.618F, ythis - mark_te.height);
	}

	vaxes = geometry_union(vaxes, geometry_stroke(CanvasGeometry::CreatePath(axes), s.vaxes_thickness, s.vaxes_style));
	this->vaxes = geometry_freeze(vaxes);
}

void ITimeSerieslet::update_horizontal_axes(TimeSeriesStyle& s) {
	CanvasPathBuilder^ axes = ref new CanvasPathBuilder(CanvasDevice::GetSharedDevice());
	CanvasGeometry^ hmarks = blank();
	float interval = this->width / float(this->series.step + 1);
	long long delta = this->series.span / (this->series.step + 1);
	float x = s.haxes_thickness * 0.5F;
	float y = this->height - s.border_thickness;
	TextExtent mark_te;

	long long now = current_seconds();

	for (unsigned int i = 0; i <= this->series.step + 1; i++) {
		float xthis = x + interval * float(i);
		Platform::String^ mark = make_daytimestamp(this->series.start + delta * i, false);
		CanvasGeometry^ gmark = paragraph(mark, s.font, &mark_te);

		axes->BeginFigure(xthis, 0.0F);
		axes->AddLine(xthis, this->height);
		axes->EndFigure(CanvasFigureLoop::Open);

		hmarks = geometry_union(hmarks, gmark, xthis - mark_te.width * 0.5F, y - mark_te.height);
	}

	this->hmarks = geometry_freeze(hmarks);
	this->haxes = geometry_stroke(CanvasGeometry::CreatePath(axes), s.haxes_thickness, s.haxes_style);
}

void ITimeSerieslet::update_legend(unsigned int idx, TimeSeriesStyle& s) {
	double value = this->values[idx];
	Platform::String^ legend = this->names[idx] + ": ";

	legend += (std::isnan(value) ? speak(":nodatum:") : flstring(value, this->precision + 1));	
	this->legends[idx] = make_text_layout(legend, s.legend_font);
}

void ITimeSerieslet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	TimeSeriesStyle s = this->get_style();
	float border_off = s.border_thickness * 0.5F;
	float view_x = 0.0F;
	
	ds->FillRectangle(x, y, this->width, this->height, Colours::Background);

	ds->DrawCachedGeometry(this->vaxes, x, y, s.vaxes_color);

	if (this->get_status() == TimeSeriesStatus::Realtime) {
		Rect haxes_box = this->haxes->ComputeBounds();
		long long now = current_seconds();
		float percentage = std::fminf(float(now - this->series.start) / float(this->series.span), 1.0F);
		float xmin = this->width - haxes_box.Width;
		
		view_x = std::fmaxf(xmin, std::fminf(this->width * 0.5F - haxes_box.Width * percentage, 0.0F));
		
		ds->FillGeometry(this->haxes, x + view_x, y, s.haxes_color);
		ds->DrawCachedGeometry(this->hmarks, x + view_x, y, s.haxes_color);
	}

	{ // draw legends and lines
		float legend_x = x + this->width * s.legend_fx;
		float legend_label_height = this->legends[0]->LayoutBounds.Height;
		float legend_label_x = legend_x + legend_label_height * 1.618F;
		float legend_width = legend_label_height;
		float legend_height = legend_label_height * 0.618F;
		float legend_yoff = (legend_label_height - legend_height) * 0.5F;
		
		for (unsigned int idx = 0; idx < this->lines->Length; idx++) {
			ds->FillGeometry(this->lines[idx], x + view_x, y + s.lines_thickness * 0.5F, this->colors[idx]);
		}

		for (unsigned int idx = 0; idx < this->legends->Length; idx++) {
			float yoff = legend_label_height * (float(idx) + 0.618F);

			ds->FillRectangle(legend_x, y + legend_yoff + yoff, legend_width, legend_height, this->colors[idx]);
			ds->DrawTextLayout(this->legends[idx], legend_label_x, y + yoff, this->colors[idx]);
		}
	}
	
	ds->DrawRectangle(x + border_off, y + border_off,
		this->width - s.border_thickness, this->height - s.border_thickness,
		s.border_color, s.border_thickness);
}

void ITimeSerieslet::fill_this_position(long long time, double v, double* x, double* y) {
	Rect haxes_box = this->haxes->ComputeBounds();
	double fx = double(time - this->series.start) / double(this->series.span);
	double fy = (this->vmin == this->vmax) ? 1.0 : (this->vmax - v) / (this->vmax - this->vmin);

	// TODO: find a scalable method.
	SET_BOX(x, fx * haxes_box.Width);
	SET_BOX(y, fy * haxes_box.Height);
}

void ITimeSerieslet::set_value(unsigned int idx, double v) {
	static auto linestyle = make_roundcap_stroke_style();
	TimeSeriesStyle s = this->get_style();
	long long now = current_seconds();
	long long next_start = this->series.start + this->series.span;
	double px, py;

	if (now > next_start) {
		this->update_time_series(next_start);
	}

	this->fill_this_position(now, v, &px, &py);
	if (!std::isnan(this->values[idx])) {
		this->lines[idx] = geometry_union(this->lines[idx],
			line(float(this->xs[idx]), float(this->ys[idx]),
				float(px), float(py), s.lines_thickness, linestyle));
	}

	this->values[idx] = v;
	this->update_legend(idx, s);

	this->xs[idx] = px;
	this->ys[idx] = py;
}
