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
static CanvasTextFormat^ lines_default_font = make_bold_text_format(12.0F);
static CanvasTextFormat^ lines_default_legend_font = make_bold_text_format(14.0F);

private struct WarGrey::SCADA::TimeSeriesLine {
public:
	void set_value(long long timestamp, double value) {
		this->timestamps.push(timestamp);
		this->values.push(value);
	}

	void update_legend(unsigned int precision, WarGrey::SCADA::TimeSeriesStyle& style, ICanvasBrush^ color = nullptr) {
		Platform::String^ legend = this->name + ": ";

		legend += (this->values.empty() ? speak("nodatum", "status") : flstring(this->values.back(), precision));
		this->legend = make_text_layout(legend, style.legend_font);

		if (color != nullptr) {
			this->color = color;
		}
	}

public:
	std::queue<long long> timestamps;
	std::queue<double> values;

public:
	Microsoft::Graphics::Canvas::Brushes::ICanvasBrush^ color;
	Microsoft::Graphics::Canvas::Text::CanvasTextLayout^ legend;
	Platform::String^ name;
};

/*************************************************************************************************/
TimeSeries WarGrey::SCADA::make_minute_series(unsigned int count, unsigned int step) {
	TimeSeries ts;

	ts.start = current_seconds() - time_zone_utc_bias_seconds();
	ts.span = minute_span_s * std::max(count, 1U);
	ts.step = step;

	return ts;
}

TimeSeries WarGrey::SCADA::make_hour_series(unsigned int count, unsigned int step) {
	TimeSeries ts;

	ts.start = current_seconds() - time_zone_utc_bias_seconds();
	ts.span = hour_span_s * std::max(count, 1U);
	ts.step = step;

	return ts;
}

TimeSeries WarGrey::SCADA::make_today_series(unsigned int step) {
	TimeSeries ts;

	ts.start = current_seconds() - time_zone_utc_bias_seconds();
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
	, vmin(vmin), vmax(vmax), series(ts), count(n) {

	if (this->height == 0.0F) {
		this->height = this->width * 0.2718F;
	}

	if (this->vmin > this->vmax) {
		this->vmin = vmax; 
		this->vmax = vmin;
	}
}

ITimeSerieslet::~ITimeSerieslet() {
	if (this->lines != nullptr) {
		delete[] this->lines;
	}
}

void ITimeSerieslet::update(long long count, long long interval, long long uptime) {
	long long now = current_seconds() - time_zone_utc_bias_seconds();
	long long next_start = this->series.start + this->series.span;

	if (now > next_start) {
		this->update_time_series(this->series.start + this->series.span / this->series.step);
	}
}

void ITimeSerieslet::construct_line(unsigned int idx, Platform::String^ name) {
	TimeSeriesStyle style = this->get_style();

	if (this->lines == nullptr) {
		this->lines = new TimeSeriesLine[this->count];
	}

	this->lines[idx].name = name;
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
	CAS_SLOT(style.lines_style, make_roundcap_stroke_style());

	FLCAS_SLOT(style.border_thickness, 3.0F);
	FLCAS_SLOT(style.lines_thickness, 1.0F);
	FLCAS_SLOT(style.haxes_thickness, 0.5F);
	FLCAS_SLOT(style.vaxes_thickness, 0.5F);

	if ((style.legend_fx < 0.0F) || (style.legend_fx > 1.0F)) {
		style.legend_fx = 0.75F;
	}
}

void ITimeSerieslet::on_status_changed(TimeSeriesStatus status) {
	TimeSeriesStyle style = this->get_style();

	this->update_vertical_axes(style);
	this->update_horizontal_axes(style);

	for (unsigned int idx = 0; idx < this->count; idx++) {
		this->lines[idx].update_legend(this->precision + 1U, style, style.lookup_color(idx));
	}
}

void ITimeSerieslet::update_time_series(long long next_start) {
	this->series.start = next_start;
	this->update_horizontal_axes(this->get_style());

	// TODO: pop old values;
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
	long long now = current_seconds() - time_zone_utc_bias_seconds();
	TextExtent mark_te;

	for (unsigned int i = 0; i <= this->series.step + 1; i++) {
		float xthis = x + interval * float(i);
		Platform::String^ mark = make_daytimestamp_utc(this->series.start + delta * i, false);
		CanvasGeometry^ gmark = paragraph(mark, s.font, &mark_te);

		axes->BeginFigure(xthis, 0.0F);
		axes->AddLine(xthis, this->height);
		axes->EndFigure(CanvasFigureLoop::Open);

		hmarks = geometry_union(hmarks, gmark, xthis - mark_te.width * 0.5F, y - mark_te.height);
	}

	this->hmarks = geometry_freeze(hmarks);
	this->haxes = geometry_stroke(CanvasGeometry::CreatePath(axes), s.haxes_thickness, s.haxes_style);
}

void ITimeSerieslet::draw(CanvasDrawingSession^ ds, float x, float y, float Width, float Height) {
	TimeSeriesStyle style = this->get_style();
	float border_off = style.border_thickness * 0.5F;
	float view_x = 0.0F;
	
	ds->FillRectangle(x, y, this->width, this->height, Colours::Background);

	ds->DrawCachedGeometry(this->vaxes, x, y, style.vaxes_color);

	if (this->get_status() == TimeSeriesStatus::Realtime) {
		Rect haxes_box = this->haxes->ComputeBounds();
		long long now = current_seconds() - time_zone_utc_bias_seconds();
		float percentage = std::fminf(float(now - this->series.start) / float(this->series.span), 1.0F);
		float xmin = this->width - haxes_box.Width;
		
		view_x = std::fmaxf(xmin, std::fminf(this->width * 0.5F - haxes_box.Width * percentage, 0.0F));
		
		ds->FillGeometry(this->haxes, x + view_x, y, style.haxes_color);
		ds->DrawCachedGeometry(this->hmarks, x + view_x, y, style.haxes_color);

		{ // draw lines
			for (unsigned idx = 0; idx < this->count; idx++) {
				float last_x = std::nanf("no datum");
				float last_y = std::nanf("no datum");
				float rx = x + haxes_box.Width;
				auto t = this->lines[idx].timestamps._Get_container().begin();
				auto v = this->lines[idx].values._Get_container().begin();
				auto end = this->lines[idx].timestamps._Get_container().end();

				while (t != end) {
					double fx = double((*t) - this->series.start) / double(this->series.span);
					double fy = (this->vmin == this->vmax) ? 1.0 : (this->vmax - (*v)) / (this->vmax - this->vmin);
					float this_x = float(fx) * haxes_box.Width;
					float this_y = float(fy) * haxes_box.Height;

					if (std::isnan(last_x) || (this_x < x) || (this_x > rx)) {
						last_x = this_x;
						last_y = this_y;
					} else {
						if (((this_x - last_x) > 1.0F) || (std::fabsf(this_y - last_y) > 1.0)) {
							ds->DrawLine(last_x, last_y, this_x, this_y, this->lines[idx].color, style.lines_thickness);
						
							last_x = this_x;
							last_y = this_y;
						}
					}

					t++;
					v++;
				}
			}
		}
	}

	{ // draw legends
		float legend_x = x + this->width * style.legend_fx;
		float legend_label_height = this->lines[0].legend->LayoutBounds.Height;
		float legend_label_x = legend_x + legend_label_height * 1.618F;
		float legend_width = legend_label_height;
		float legend_height = legend_label_height * 0.618F;
		float legend_yoff = (legend_label_height - legend_height) * 0.5F;

		for (unsigned int idx = 0; idx < this->count; idx++) {
			float yoff = legend_label_height * (float(idx) + 0.618F);

			ds->FillRectangle(legend_x, y + legend_yoff + yoff, legend_width, legend_height, this->lines[idx].color);
			ds->DrawTextLayout(this->lines[idx].legend, legend_label_x, y + yoff, this->lines[idx].color);
		}
	}
	
	ds->DrawRectangle(x + border_off, y + border_off,
		this->width - style.border_thickness, this->height - style.border_thickness,
		style.border_color, style.border_thickness);
}

void ITimeSerieslet::set_value(unsigned int idx, double v) {
	TimeSeriesStyle style = this->get_style();
	long long now = current_seconds() - time_zone_utc_bias_seconds();

	this->lines[idx].set_value(now, v);
	this->lines[idx].update_legend(this->precision + 1U, style);
}
